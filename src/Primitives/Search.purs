-- | The Search primitive captures and debounces user input. It can be used in conjunction
-- | with the Container primitive to make search-driven selection components like
-- | typeaheads, image pickers, and date pickers.

module Select.Primitives.Search where

import Prelude

import Data.Time.Duration (Milliseconds)
import DOM.Event.Types as ET
import Control.Comonad (extract)
import Control.Comonad.Store (seeks, store)
import Control.Monad.Aff (Fiber, delay, error, forkAff, killFiber)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, takeVar, AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen (IProp, Component, ComponentDSL, ComponentHTML, action, component, liftAff, modify) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (fork, raise) as H
import Select.Primitives.State (State, updateStore, getState)
import Select.Primitives.Container as C

type SearchEffects eff = ( avar :: AVAR | eff )

-- | The query type for the `Search` primitive. This primitive handles text input
-- | and debouncing. It has a special query for the purpose of embedding Container
-- | queries, used to route key events to the container from an input field.
-- |
-- | Arguments:
-- |
-- | - `o`: The type of the parent query to embed. This is usually the component that
-- | -      mounts the search primitive, but it could also be the query type of some
-- |        higher component.
-- | - `item`: Your custom item type, used by your renderers.
-- | - `eff`: The row of effects to use in the primitive. You are expected to provide your
-- |          rows wrapped in the select SearchEffects type:
-- |
-- | ```purescript
-- | type ChildQuery eff = SearchQuery MyQuery MyItem (SearchEffects eff)
-- | ```
-- |
-- | Constructors:
-- |
-- | - `TextInput`: Handle new text input as a string
-- | - `Raise`: Embed a parent query that can be returned to the parent for evaluation
-- | - `FromContainer`: Embed a container query that can be routed to a container slot
-- | - `SearchReceiver`: Update the component with new `Input` when the parent re-renders
data SearchQuery o item eff a
  = TextInput String a
  | Raise (o Unit) a
  | FromContainer (C.ContainerQuery o item Unit) a
  | SearchReceiver (SearchInput o item eff) a

-- | The `Search` primitive internal state.
-- |
-- | Arguments:
-- |
-- | - `eff`: The extensible row of effects used in the primitive for debouncing. You should pass
-- |          your own effects.
-- |
-- | Fields:
-- |
-- | - `search`: The `String` contained within the primitive. This is captured from user
-- |             input, or can be set by the parent.
-- | - `ms`: Number of milliseconds for the input to be debounced before passing
-- |         a message to the parent. Set to 0.0 if you don't want debouncing.
-- | - `debouncer`: Facilitates debouncing for the input field.
type SearchState eff =
  { search    :: String
  , ms        :: Milliseconds
  , debouncer :: Maybe (Debouncer eff)
  }

-- | The `Debouncer` type alias, used to debounce user input in the `Search` primitive.
type Debouncer eff =
  { var   :: AVar Unit
  , fiber :: Fiber eff Unit }

-- | The input type of the `Search` primitive
-- |
-- | Fields:
-- |
-- | - `search`: An optional initial value for the `search` key on the `SearchState`
-- | - `debounceTime`: A value in milliseconds for the debounce delay. Set to 0.0 for
-- |                   no debouncing.
-- | - `render`: The render function for the primitive
type SearchInput o item eff =
  { search :: Maybe String
  , debounceTime :: Milliseconds
  , render :: SearchState eff -> H.ComponentHTML (SearchQuery o item eff) }


-- | The possible messages a parent can evaluate from the search primitive.
-- |
-- | Constructors:
-- |
-- | - `NewSearch`: The user has entered a new search and it has passed the debouncer.
-- | - `ContainerQuery`: The user has triggered an embedded container query, like using arrow
-- |                     keys or Enter to select an item. Usually, you will route this to the
-- |                     container associated with your search primitive.
-- |
-- | ```purescript
-- | eval (FromContainer q a) -> H.raise (ContainerQuery q) *> pure a
-- | ```
-- |
-- | - `Emit`: An embedded parent query has been triggered. This can be evaluated automatically
-- |           with this code in the parent's eval function:
-- |
-- | ```purescript
-- | eval (HandleSearch (Emit q) next) = eval q *> pure next
-- | ```
data Message o item
  = NewSearch String
  | ContainerQuery (C.ContainerQuery o item Unit)
  | Emit (o Unit)


-- | The primitive handles state and transformations but defers all rendering to the parent. The
-- | render function can be written using our helper functions to ensure the right events are included.
component :: ∀ o item eff m
  . MonadAff (SearchEffects eff) m
 => H.Component
     HH.HTML
     (SearchQuery o item (SearchEffects eff))
     (SearchInput o item (SearchEffects eff))
     (Message o item)
     m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input SearchReceiver
    }
  where
    initialState
      :: SearchInput o item (SearchEffects eff)
      -> State (SearchState (SearchEffects eff)) (SearchQuery o item (SearchEffects eff))
    initialState i = store i.render
      { search: fromMaybe "" i.search
      , ms: i.debounceTime
      , debouncer: Nothing
      }

    eval
      :: (SearchQuery o item (SearchEffects eff))
      ~> H.ComponentDSL
          (State (SearchState (SearchEffects eff)) (SearchQuery o item (SearchEffects eff)))
          (SearchQuery o item (SearchEffects eff))
          (Message o item)
          m
    eval = case _ of
      TextInput str a -> a <$ do
        (Tuple _ st) <- getState
        H.modify $ seeks _ { search = str }

        case st.debouncer of
          Nothing -> unit <$ do
            var <- H.liftAff makeEmptyVar
            fiber <- H.liftAff $ forkAff do
                delay st.ms
                putVar unit var

            -- This computation will fork and run later. When the var is finally filled,
            -- it will run the effect.
            _ <- H.fork $ do
              _ <- H.liftAff $ takeVar var
              H.modify $ seeks _ { debouncer = Nothing }

              (Tuple _ newState) <- getState
              H.raise $ NewSearch newState.search

            H.modify $ seeks _ { debouncer = Just { var, fiber } }

          Just debouncer -> do
            let var = debouncer.var
            _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber

            fiber <- H.liftAff $ forkAff do
                delay st.ms
                putVar unit var

            H.modify $ seeks _ { debouncer = Just { var, fiber } }

      FromContainer q a -> do
        H.raise (ContainerQuery q)
        pure a

      -- Raise the embedded query
      Raise q a -> H.raise (Emit q) *> pure a

      -- Only update `render`. To send a new search to the field, use a query from
      -- the parent to set the text.
      SearchReceiver i a -> H.modify (updateStore i.render id) *> pure a


-- | Attach the necessary properties to the input field you render in the page. This
-- | should be used directly on the input field's list of properties:
-- |
-- | ```purescript
-- | input_ $ getInputProps [ class_ (ClassName "my-class"), placeholder "Search..." ]
-- | ```
-- |
-- | Note: This will overwrite any existing properties of the same name.
getInputProps :: ∀ o item e eff
   . Array
      (H.IProp
        ( onFocus :: ET.FocusEvent
        , onKeyDown :: ET.KeyboardEvent
        , onInput :: ET.Event
        , value :: String
        , onMouseDown :: ET.MouseEvent
        , onMouseUp :: ET.MouseEvent
        , onBlur :: ET.FocusEvent
        , tabIndex :: Int
        | e
        )
        (SearchQuery o item eff)
      )
  -> Array
      (H.IProp
        ( onFocus :: ET.FocusEvent
        , onKeyDown :: ET.KeyboardEvent
        , onInput :: ET.Event
        , value :: String
        , onMouseDown :: ET.MouseEvent
        , onMouseUp :: ET.MouseEvent
        , onBlur :: ET.FocusEvent
        , tabIndex :: Int
        | e
        )
        (SearchQuery o item eff)
      )
getInputProps = flip (<>)
  [ HE.onFocus      $ HE.input_ $ FromContainer $ H.action $ C.Visibility C.Toggle
  , HE.onKeyDown    $ HE.input  $ \ev -> FromContainer $ H.action $ C.Key ev
  , HE.onValueInput $ HE.input  TextInput
  , HE.onMouseDown  $ HE.input_ $ FromContainer $ H.action $ C.Mouse C.Down
  , HE.onMouseUp    $ HE.input_ $ FromContainer $ H.action $ C.Mouse C.Up
  , HE.onBlur       $ HE.input_ $ FromContainer $ H.action $ C.Blur
  , HP.tabIndex 0
  ]
