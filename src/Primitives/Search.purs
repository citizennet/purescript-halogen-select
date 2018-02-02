module Select.Primitives.Search where

import Prelude

import Data.Time.Duration (Milliseconds)
import DOM.Event.Types as ET
import Control.Comonad (extract)
import Control.Comonad.Store (seeks, store)
import Control.Monad.Aff (Aff, Fiber, delay, error, forkAff, killFiber)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, takeVar)
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
import Select.Effects (Effects)

{-

The Search primitive captures user input and returns it to the parent.

-}

-- | The query type for the `Search` primitive. This primitive handles text input
-- | and debouncing.
-- |
-- | - `TextInput`: Handle new text input as a string
-- | - `SearchReceiver`: Update the component with new `Input` when the parent re-renders
data SearchQuery o item e a
  = TextInput String a
  | Raise (o Unit) a
  | FromContainer (C.ContainerQuery o item Unit) a
  | SearchReceiver (SearchInput o item e) a

-- | The `Search` primitive internal state
-- |
-- | - `search`: The `String` contained within the primitive
-- | - `ms`: Number of milliseconds for the input to be debounced before passing
-- |         a message to the parent. Set to 0.0 if you don't want debouncing.
-- | - `debouncer`: Used to facilitate debouncing of the input
type SearchState e =
  { search    :: String
  , ms        :: Milliseconds
  , debouncer :: Maybe (Debouncer e)
  }

-- | The `Debouncer` type alias, used to debounce user input in the `Search` primitive.
type Debouncer e =
  { var   :: AVar Unit
  , fiber :: Fiber e Unit }

-- | The input type of the `Search` primitive
-- |
-- | - `search`: An optional initial value for the `search` key on the `SearchState`
-- | - `debounceTime`: A value in milliseconds for the debounce delay. Set to 0.0 for
-- | no debouncing.
-- | - `render`: The render function for the primitive
type SearchInput o item e =
  { search :: Maybe String
  , debounceTime :: Milliseconds
  , render :: SearchState e -> H.ComponentHTML (SearchQuery o item e) }


-- | The Search sends the parent messages in two instances:
-- | - `Emit`: An embedded query has been triggered, and you must decide how to handle it; typically via evaluating
-- |           in the parent or re-routing the query to another primitive.
-- | - `NewSearch`: Some new text has been searched (this is automatically debounced).
data Message o item
  = NewSearch String
  | ContainerQuery (C.ContainerQuery o item Unit)
  | Emit (o Unit)


-- | The primitive handles state and transformations but defers all rendering to the parent. The
-- | render function can be written using our helper functions to ensure the right events are included. See the `Dispatch`
-- | module for more information.

component :: ∀ o item e m
  . MonadAff (Effects e) m
 => H.Component
     HH.HTML
     (SearchQuery o item (Effects e))
     (SearchInput o item (Effects e))
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
      :: SearchInput o item (Effects e)
      -> State (SearchState (Effects e)) (SearchQuery o item (Effects e))
    initialState i = store i.render
      { search: fromMaybe "" i.search
      , ms: i.debounceTime
      , debouncer: Nothing
      }

    eval
      :: (SearchQuery o item (Effects e))
      ~> H.ComponentDSL
          (State (SearchState (Effects e))
          (SearchQuery o item (Effects e)))
          (SearchQuery o item (Effects e))
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



getInputProps :: ∀ o item e e0
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
        (SearchQuery o item e0)
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
        (SearchQuery o item e0)
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
