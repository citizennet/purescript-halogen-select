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

-- | The `Dispatch` type serves as the query type for ALL primitives
-- | and has data constructors for handling queries of the parent's type
-- | or wrapping the queries of the primitives
data Dispatch item o e a
  = ParentQuery (o Unit) a
  | Search (SearchQuery item o e) a
  | Container (ContainerQuery item o e) a

-- | Primitives share the same Store type but can provide their own state type
-- |
-- | It's necessary to use a `Store` comonad as the state type for the primitives
-- | so that they can receive their `render` functions as input
type State s item o e = Store s (H.ComponentHTML (Dispatch item o e))

{-

SEARCH PRIMITIVE

-}

-- | The query type for the `Search` primitive
data SearchQuery item o e
  = TextInput String
  | SearchReceiver (SearchInput item o e)

-- | The `Search` primitive's internal state
-- | - `search`: the `String` contained within the primitive
-- | - `ms`: number of milliseconds for the input to be debounced before passing
-- |         a message to the parent
-- | - `debouncer`: used to facilitate debouncing of the input
type SearchState e =
  { search    :: String
  , ms        :: Milliseconds
  , debouncer :: Maybe (Debouncer e)
  }

-- | The `Debouncer` type alias, used to debounce user input in the `Search` primitive
type Debouncer e =
  { var   :: AVar Unit
  , fiber :: Fiber (Effects e) Unit }

-- | The input type of the `Search` primitive
-- | - `search`: an optional initial value for the `search` key on the `SearchState`
-- | - `debounceTime`: a value in milliseconds for the debounce delay
-- | - `render`: the render function for the primitive
type SearchInput item o e =
  { search :: Maybe String
  , debounceTime :: Milliseconds
  , render :: SearchState e -> H.ComponentHTML (Dispatch item o e) }


{-

CONTAINER PRIMITIVE

-}

-- | The query type for the `Container` primitive
data ContainerQuery item o e
  = Highlight   Target              -- Change the highlighted item
  | Select      Int                 -- Select a particular item
  | Key         KeyboardEvent       -- A key has been pressed
  | Mouse       MouseState          -- Update mousedown state
  | Blur                            -- Blur event
  | Visibility  VisibilityStatus    -- Open or close the menu
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
-- | It accepts an `eval` function, a `Dispatch` query, and the `a` from the parent's context
emit :: ∀ a0 a1 o item e f. Applicative f => (o Unit -> f Unit) -> Dispatch item o e a0 -> a1 -> f a1
emit f (ParentQuery o _) a = a <$ f o
emit _ _ a = pure a

-- | Helper for wholly updating the `State` (`Store`) of a primitive
-- |
-- | Useful when the `render` function needs to be updated
-- |
-- | *Use `seeks` if only the primitive's internal state needs to be updated*

updateStore :: ∀ state html. (state -> html) -> (state -> state) -> Store state html -> Store state html
updateStore r f = (\(Tuple _ s) -> store r s) <<< runStore <<< seeks f

-- | Helper to get and unpack the primitive state type from the Store type
getState :: ∀ m s a. MonadState (Store s a) m => m (Tuple (s -> a) s)
getState = pure <<< runStore =<< H.get


--
--
-- RENDERING
--
--

-- | Helper used to concatenate two `Array IProps`, assuring that our events overwrite
-- | the user's when they conflict

-- | For allowing the user to include arbitrary properties
-- | on the elements in the render function passed to the primitive
augmentHTML ::
   ∀ props q
   . Array (H.IProp props q)
  -> Array (H.IProp props q)
  -> Array (H.IProp props q)
augmentHTML = flip (<>)

-- | Embed a parent query into a `Dispatch` type
embed :: ∀ item parentQuery e. H.Action parentQuery -> Unit -> Dispatch item parentQuery e Unit
embed = ParentQuery <<< H.action

-- | Intended for use on the text input field.
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

-- | Intended for use on a clickable toggle to show/hide the `Container`
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

-- | Intended to be used on the container primitive itself
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

-- | Intended for anything that will be embedded into the container primitive
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

-- | Intended for use on the element rendered for each item in the `Container`
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
