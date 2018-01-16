module Select.Dispatch where

import Prelude

import Control.Comonad.Store (Store, runStore, seeks, store)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Control.Monad.Aff (Fiber)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.State (class MonadState)
import DOM.Event.KeyboardEvent (KeyboardEvent)
import DOM.Event.Types as ET
import Data.Time.Duration (Milliseconds)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Effects (Effects)

{-

This module contains the query types for all primitives and allows multiple components
to share query types.

-}

-- | The `Dispatch` type serves as the query type for all primitives
-- | and has data constructors for handling queries of the parent's type
-- | or wrapping the queries of the primitives. When you use any primitive,
-- | you will supply the Dispatch type as the primitive's query.
-- | See the respective primitives for their available queries.
-- |
-- | `item` - Your defined type of the items you are storing in the primitives
-- | `o` - The query type of the parent component containing the primitives
-- | `e` - The effects type of the primitives
-- | `a` - The Halogen return type, which must remain polymorphic as in all Halogen components.
data Dispatch item o e a
  = ParentQuery (o Unit) a
  | Search (SearchQuery item o e) a
  | Container (ContainerQuery item o e) a

-- | All primitives use the `Store` type as their component state. Any additional
-- | data, like a traditional Halogen State record, will usually be provided.
-- |
-- | Note: It's necessary to use a `Store` comonad as the state type for primitives
-- | because they receive their render function as input. The render function cannot
-- | be stored in the State type synonym due to cycles, and as your component render
-- | function can only take `State` as its input, you need the render function available
-- | via the comonad, StoreT.
-- |
-- | `s` - The State type defined for the primitive.
type State s item o e = Store s (H.ComponentHTML (Dispatch item o e))

{-

SEARCH PRIMITIVE

-}

-- | The query type for the `Search` primitive. This primitive handles text input
-- | and debouncing.
-- |
-- | TextInput - handle new text input as a string
-- | SearchReceiver - update the component with new `Input` when the parent re-renders.
data SearchQuery item o e
  = TextInput String
  | SearchReceiver (SearchInput item o e)

-- | The `Search` primitive internal state
-- | - `search`: the `String` contained within the primitive
-- | - `ms`: number of milliseconds for the input to be debounced before passing
-- |         a message to the parent. Set to 0.0 if you don't want debouncing.
-- | - `debouncer`: used to facilitate debouncing of the input
type SearchState e =
  { search    :: String
  , ms        :: Milliseconds
  , debouncer :: Maybe (Debouncer e)
  }

-- | The `Debouncer` type alias, used to debounce user input in the `Search` primitive.
type Debouncer e =
  { var   :: AVar Unit
  , fiber :: Fiber (Effects e) Unit }

-- | The input type of the `Search` primitive
-- | - `search`: an optional initial value for the `search` key on the `SearchState`
-- | - `debounceTime`: a value in milliseconds for the debounce delay. Set to 0.0 for
-- | no debouncing.
-- | - `render`: the render function for the primitive
type SearchInput item o e =
  { search :: Maybe String
  , debounceTime :: Milliseconds
  , render :: SearchState e -> H.ComponentHTML (Dispatch item o e) }


{-

CONTAINER PRIMITIVE

-}

-- | The query type for the `Container` primitive.
-- |
-- | Highlight - change the highlighted item to the next, previous, or a specific index.
-- | Select - select an item at the specified index
-- | Key - capture key events for arrow navigation, Escape to close, and Enter to select.
-- | Mouse - capture mouse events to close the menu or select an item
-- | Blur - trigger the DOM blur event
-- | Visibility - set the visibility by toggling, setting to on, or setting to off.
-- | ContainerReceiver - update the component on new `Input` when the parent re-renders.
data ContainerQuery item o e
  = Highlight   Target
  | Select      Int
  | Key         KeyboardEvent
  | Mouse       MouseState
  | Blur
  | Visibility  VisibilityStatus
  | ContainerReceiver (ContainerInput item o e)

-- | For targeting items in the container via the `ContainerQuery`'s `Highlight` constructor
data Target
  = Next
  | Prev
  | Index Int

-- | For maintaining the state of the mouse in the `Container`
data MouseState
  = Down
  | Up

-- | For showing or hiding the `Container`
data VisibilityStatus
  = On
  | Off
  | Toggle

-- | The internal state of the `Container` primitive
-- | - `items`: an array of items held within the `Container`
-- | - `open`: whether the `Container` is visible
-- | - `highlightedIndex`: the index of the highlighted item, if one exists
-- | - `lastIndex`: the index of the last item in the `Container`
-- | - `mouseDown`: whether the mouse is clicked or not
type ContainerState item =
  { items            :: Array item
  , open             :: Boolean
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  , mouseDown        :: Boolean
  }

-- | The input type of the `Container` primitive
-- | - `items`: the initial value of `items` in the `ContainerState`
-- | - `render`: the `render` function for the `Container` primitive
type ContainerInput item o e =
  { items  :: Array item
  , render :: ContainerState item -> H.ComponentHTML (Dispatch item o e) }


-- | A helper function for conveniently evaluating a `ParentQuery` using the parent's `eval` function
-- |
-- | It accepts an `eval` function, a `Dispatch` query, and the `a` from the parent's context.
-- |
-- | WARNING: This function should only be used when you truly want to ignore all emitted queries except
-- | for your own parent component queries. If you need to route queries among primitives (for example, route
-- | a key press on a search primitive to a key event on a container primitive), then you will need to manually
-- | route them. See the documentation for more details.
emit :: ∀ a0 a1 o item e f. Applicative f => (o Unit -> f Unit) -> Dispatch item o e a0 -> a1 -> f a1
emit f (ParentQuery o _) a = a <$ f o
emit _ _ a = pure a

-- | Helper for wholly updating the `State` (`Store`) of a primitive
-- |
-- | Used when the `render` function needs to be updated
-- | Note: Use `seeks` if only the primitive's internal state needs to be updated (not the entire Store).
updateStore :: ∀ state html. (state -> html) -> (state -> state) -> Store state html -> Store state html
updateStore r f = (\(Tuple _ s) -> store r s) <<< runStore <<< seeks f

-- | Helper to get and unpack the primitive state type from the Store type. When used with pattern matching,
-- | you can access state with:
-- | (Tuple renderFunction state) <- getState
getState :: ∀ m s a. MonadState (Store s a) m => m (Tuple (s -> a) s)
getState = pure <<< runStore =<< H.get


{-

RENDERING

-}

-- | Helper used to concatenate two `Array IProps`, assuring that our events overwrite
-- | the user's when they conflict.
-- |
-- | For allowing the user to include arbitrary properties
-- | on the elements in the render function passed to the primitive.
augmentHTML ::
   ∀ props q
   . Array (H.IProp props q)
  -> Array (H.IProp props q)
  -> Array (H.IProp props q)
augmentHTML = flip (<>)

-- | Embed a parent query into a `Dispatch` type. In use:
-- | [ onClick $ input_ $ embed YourQueryType ]
embed :: ∀ item parentQuery e. H.Action parentQuery -> Unit -> Dispatch item parentQuery e Unit
embed = ParentQuery <<< H.action

-- | Intended for use on the text input field with the Search primitive. If you are using a button
-- | to toggle the menu instead, use the `getToggleProps` helper.
getInputProps ::
   ∀ item o e p
   . Array
       ( H.IProp
         ( onFocus :: ET.FocusEvent
         , onKeyDown :: ET.KeyboardEvent
         , onInput :: ET.Event
         , value :: String
         , onMouseDown :: ET.MouseEvent
         , onMouseUp :: ET.MouseEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
  -> Array
       ( H.IProp
         ( onFocus :: ET.FocusEvent
         , onKeyDown :: ET.KeyboardEvent
         , onInput :: ET.Event
         , value :: String
         , onMouseDown :: ET.MouseEvent
         , onMouseUp :: ET.MouseEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
getInputProps = augmentHTML
  [ HE.onFocus      $ HE.input_ $ Container $ Visibility Toggle
  , HE.onKeyDown    $ HE.input  $ \ev -> Container $ Key ev
  , HE.onValueInput $ HE.input  $ \ev -> Search $ TextInput ev
  , HE.onMouseDown  $ HE.input_ $ Container $ Mouse Down
  , HE.onMouseUp    $ HE.input_ $ Container $ Mouse Up
  , HE.onBlur       $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]

-- | Intended for use on a clickable toggle to show/hide the `Container` primitive. If
-- | you are using a text field to manage the container, use `getInputProps` instead.
getToggleProps ::
   ∀ item o e p
   . Array
       ( H.IProp
         ( onClick :: ET.MouseEvent
         , onKeyDown :: ET.KeyboardEvent
         , onMouseDown :: ET.MouseEvent
         , onMouseUp :: ET.MouseEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
  -> Array
       ( H.IProp
         ( onClick :: ET.MouseEvent
         , onKeyDown :: ET.KeyboardEvent
         , onMouseDown :: ET.MouseEvent
         , onMouseUp :: ET.MouseEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
getToggleProps = augmentHTML
  [ HE.onClick      $ HE.input_ $ Container $ Visibility Toggle
  , HE.onKeyDown    $ HE.input  $ \ev -> Container $ Key ev
  , HE.onMouseDown  $ HE.input_ $ Container $ Mouse Down
  , HE.onMouseUp    $ HE.input_ $ Container $ Mouse Up
  , HE.onBlur       $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]

-- | Intended to be used on the container primitive to capture key, click, highlighting, and other events
getContainerProps ::
   ∀ item o e p
   . Array
       ( H.IProp
         ( onMouseDown :: ET.MouseEvent
         , onMouseUp :: ET.MouseEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
  -> Array
       ( H.IProp
         ( onMouseDown :: ET.MouseEvent
         , onMouseUp :: ET.MouseEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
getContainerProps = augmentHTML
  [ HE.onMouseDown $ HE.input_ $ Container $ Mouse Down
  , HE.onMouseUp   $ HE.input_ $ Container $ Mouse Up
  , HE.onBlur      $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]

-- | Intended for anything that will be embedded into the container primitive. For example, if you embed
-- | a button with your own functionality into the container primitive, you might do this:
-- | button ( getChildProps [ onClick $ input_ $ embed YourQueryType ] ) [ text "Button text" ]
getChildProps ::
   ∀ item o e p
   . Array
       ( H.IProp
         ( onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
  -> Array
       ( H.IProp
         ( onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
getChildProps = augmentHTML
  [ HE.onBlur      $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]

-- | Intended for use on the element rendered for each item in the `Container`.
getItemProps ::
   ∀ item o e p
   . Int
  -> Array
       ( H.IProp
         ( onClick :: ET.MouseEvent
         , onMouseOver :: ET.MouseEvent
         , onKeyDown :: ET.KeyboardEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
  -> Array
       ( H.IProp
         ( onClick :: ET.MouseEvent
         , onMouseOver :: ET.MouseEvent
         , onKeyDown :: ET.KeyboardEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | p
         )
         (Dispatch item o e)
       )
getItemProps index = augmentHTML
  [ HE.onClick     $ HE.input_ $ Container $ Select index
  , HE.onMouseOver $ HE.input_ $ Container $ Highlight (Index index)
  , HE.onKeyDown   $ HE.input  $ \ev -> Container $ Key ev
  , HE.onBlur      $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]
