-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select where

import Prelude

import Prim.Row as Row
import Record.Builder as Builder
import Control.Monad.Free (liftF)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, over)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), Variant, inj, onMatch)
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.ChildQuery (ChildQueryBox)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-----
-- ACTIONS

newtype Action item st act ps m = Action (Variant
  ( search :: String 
  , highlight :: Target
  , select :: Target
  , focus :: Boolean
  , key :: KE.KeyboardEvent
  , preventClick :: ME.MouseEvent
  , setVisibility :: Visibility
  , receive :: Input item st act ps m
  , initialize :: Unit
  , andThen :: Tuple (Action item st act ps m) (Action item st act ps m)
  | act
  ))

derive instance newtypeAction :: Newtype (Action item st act ps m) _

search :: forall item st act ps m. String -> Action item st act ps m
search = Action <<< inj (SProxy :: _ "search")

highlight :: forall item st act ps m. Target -> Action item st act ps m
highlight = Action <<< inj (SProxy :: _ "highlight")

select :: forall item st act ps m. Target -> Action item st act ps m
select = Action <<< inj (SProxy :: _ "select")

focus :: forall item st act ps m. Boolean -> Action item st act ps m
focus = Action <<< inj (SProxy :: _ "focus")

key :: forall item st act ps m. KE.KeyboardEvent -> Action item st act ps m
key = Action <<< inj (SProxy :: _ "key")

preventClick :: forall item st act ps m. ME.MouseEvent -> Action item st act ps m
preventClick = Action <<< inj (SProxy :: _ "preventClick")

setVisibility :: forall item st act ps m. Visibility -> Action item st act ps m
setVisibility = Action <<< inj (SProxy :: _ "setVisibility")

receive :: forall item st act ps m. Input item st act ps m -> Action item st act ps m
receive = Action <<< inj (SProxy :: _ "receive") 

initialize :: forall item st act ps m. Unit -> Action item st act ps m
initialize = Action <<< inj (SProxy :: _ "initialize") 

andThen 
  :: forall item st act ps m
   . Action item st act ps m 
  -> Action item st act ps m -> Action item st act ps m
andThen act1 = Action <<< inj (SProxy :: _ "andThen") <<< Tuple act1

-----
-- QUERIES 

data Query item st act ps m a
  = GetVisibility (Visibility -> a)
  | ReplaceItems (Array item) a
  -- send a query through `Select` to a child component
  | SendQuery (ChildQueryBox ps (Maybe a))
  | PerformAction (Action item st act ps m) a

-----
-- MESSAGES

type Message item msg = Variant
  ( searched :: String
  , selected :: item
  , visibilityChanged :: Visibility
  | msg
  )

searched :: forall item msg. String -> Message item msg
searched = inj (SProxy :: _ "searched")

selected :: forall item msg. item -> Message item msg
selected = inj (SProxy :: _ "selected")

visibilityChanged :: forall item msg. Visibility -> Message item msg
visibilityChanged = inj (SProxy :: _ "visibilityChanged")

-----
-- HELPER TYPES

-- | Represents a way to navigate on `Highlight` events: to the previous
-- | item, next item, or the item at a particular index.
data Target = Prev | Next | Index Int
derive instance eqTarget :: Eq Target

-- | Represents whether the component should display the item container. You
-- | should use this in your render function to control visibility:
-- |
-- | ```purescript
-- | render state = if state.visibility == On then renderAll else renderInputOnly
-- | ```
-- |
-- | This is a Boolean Algebra, where `On` corresponds to true, and `Off` to
-- | false, as one might expect. Thus, `not` will invert visibility.
data Visibility = Off | On
derive instance eqVisibility :: Eq Visibility
derive instance ordVisibility :: Ord Visibility

instance heytingAlgebraVisibility :: HeytingAlgebra Visibility where
  tt = On
  ff = Off
  not On = Off
  not Off = On
  conj On On = On
  conj _ _ = Off
  disj Off Off = Off
  disj _ _ = On
  implies On Off = Off
  implies _ _ = On
instance booleanAlgebraVisibility :: BooleanAlgebra Visibility

-- | Text-driven inputs will operate like a normal search-driven selection component.
-- | Toggle-driven inputs will capture key streams and debounce in reverse (only notify
-- | about searches when time has expired).
data InputType
  = TextInput
  | Toggle

-- | The component's state
-- |
-- | - `inputType`: Controls whether the component is input-driven or toggle-driven
-- | - `search`: The text the user has typed into the text input, or stream of keys
-- |             they have typed on the toggle.
-- | - `debounceTime`: How long, in milliseconds, before events should occur based
-- |                   on user searches.
-- | - `debounceRef`: A representation of a running timer that, when it expires, will
-- |                  trigger debounced events.
-- | - `inputElement`: A reference to the toggle or input element.
-- | - `items`: An array of user-provided `item`s.
-- | - `visibility`: Whether the array of items should be considered visible or not.
-- |                 Useful for rendering.
-- | - `highlightedIndex`: What item in the array of items should be considered
-- |                       highlighted. Useful for rendering.
-- | - `lastIndex`: The length of the array of items.
newtype State item st act ps m = State
  { inputType :: InputType
  , search :: String
  , debounceTime :: Milliseconds
  , debounceRef :: Maybe (Ref (Maybe Debouncer))
  , items :: Array item
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex :: Int
  , render :: State item st act ps m -> H.ComponentHTML (Action item st act ps m) ps m
  | st
  }

derive instance newtypeState :: Newtype (State item st act ps m) _

-- | Represents a running computation that, when it completes, will trigger debounced
type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit 
  }

-- | The component's input type, which includes the component's render function. This
-- | render function can also be used to share data with the parent component, as every
-- | time the parent re-renders, the render function will refresh in `Select`.
type Input item st act ps m =
  { inputType :: InputType
  , items :: Array item
  , search :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , render :: State item st act ps m -> H.ComponentHTML (Action item st act ps m) ps m
  | st
  }

component 
  :: forall item st act ps msg m
   . Row.Lacks "debounceRef" st
  => Row.Lacks "visibility" st
  => Row.Lacks "highlightedIndex" st
  => Row.Lacks "lastIndex" st
  => MonadAff m
  => (Variant act -> H.HalogenM (State item st act ps m) (Action item st act ps m) ps (Message item msg) m Unit)
  -> H.Component HH.HTML (Query item st act ps m) (Input item st act ps m) (Message item msg) m
component handleExtraActions = H.mkComponent
  { initialState
  , render: \s@(State st) -> st.render s
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery handleExtraActions
      , handleAction = handleAction handleExtraActions
      , receive = Just <<< receive
      , initialize = Just $ initialize unit
      }
  }

initialState
  :: forall item st act ps m
   . Row.Lacks "debounceRef" st
  => Row.Lacks "visibility" st
  => Row.Lacks "highlightedIndex" st
  => Row.Lacks "lastIndex" st
  => Input item st act ps m 
  -> State item st act ps m
initialState input = State (Builder.build pipeline input)
  where
  pipeline =
    Builder.modify (SProxy :: _ "debounceTime") (fromMaybe (Milliseconds 0.0))
      >>> Builder.modify (SProxy :: _ "search") (fromMaybe "")
      >>> Builder.insert (SProxy :: _ "debounceRef") Nothing
      >>> Builder.insert (SProxy :: _ "visibility") Off
      >>> Builder.insert (SProxy :: _ "highlightedIndex") Nothing
      >>> Builder.insert (SProxy :: _ "lastIndex") (length input.items - 1)

handleAction 
  :: forall item st act ps msg m
   . MonadAff m 
  => (Variant act -> H.HalogenM (State item st act ps m) (Action item st act ps m) ps (Message item msg) m Unit)
  -> Action item st act ps m
  -> H.HalogenM (State item st act ps m) (Action item st act ps m) ps (Message item msg) m Unit
handleAction handleExtraActions = unwrap >>> flip onMatch handleExtraActions
  { initialize: \_ -> do
      ref <- H.liftEffect $ Ref.new Nothing
      H.modify_ (over State _ { debounceRef = Just ref })

  , search: \str -> do
      (State st) <- H.get
      ref  <- H.liftEffect $ map join $ traverse Ref.read st.debounceRef
      H.modify_ (over State _ { search = str })
      void $ H.fork $ handleAction' $ setVisibility On

      case st.inputType, ref of
        TextInput, Nothing -> unit <$ do
          var   <- H.liftAff AVar.empty
          fiber <- H.liftAff $ forkAff do
            delay st.debounceTime
            AVar.put unit var

          -- This compututation will fork and run in the background. When the
          -- var is finally filled, the action will run (raise a new search)
          _ <- H.fork do
            _ <- H.liftAff $ AVar.take var
            void $ H.liftEffect $ traverse_ (Ref.write Nothing) st.debounceRef
            H.modify_ (over State _ { highlightedIndex = Just 0 })
            (State newState) <- H.get
            H.raise $ searched newState.search

          void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

        TextInput, Just debouncer -> do
          let var = debouncer.var
          _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber
          fiber <- H.liftAff $ forkAff do
            delay st.debounceTime
            AVar.put unit var

          void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

        -- Key stream is not yet implemented. However, this should capture user
        -- key events and expire their search after a set number of milliseconds.
        _, _ -> pure unit

  , highlight: \target -> do
      (State st) <- H.get
      when (st.visibility == Off) do
        let targetIndex = getTargetIndex st target
        H.modify_ (over State _ { highlightedIndex = Just targetIndex })

  , select: \target -> do 
      (State st) <- H.get
      when (st.visibility == On) do 
        let raise' ix = for_ (st.items !! ix) (H.raise <<< selected)
        case target of
          Index ix -> raise' ix
          Next -> raise' $ getTargetIndex st target
          Prev -> raise' $ getTargetIndex st target

  , focus: \shouldFocus -> do
      inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
      for_ inputElement \el -> H.liftEffect case shouldFocus of 
        true -> HTMLElement.focus el
        _ -> HTMLElement.blur el

  , key: \ev -> do
      void $ H.fork $ handleAction' $ setVisibility On
      let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
      case KE.code ev of
        "ArrowUp" -> 
          preventIt *> handleAction' (highlight Prev)
        "ArrowDown" -> 
          preventIt *> handleAction' (highlight Next)
        "Escape" -> do
          inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
          preventIt
          for_ inputElement (H.liftEffect <<< HTMLElement.blur)
        "Enter" -> do
          (State st) <- H.get
          preventIt
          for_ st.highlightedIndex (handleAction' <<< select <<< Index)
        otherKey -> pure unit

  , preventClick: H.liftEffect <<< preventDefault <<< ME.toEvent

  , setVisibility: \v -> do
      (State st) <- H.get
      when (st.visibility /= v) do
        H.modify_ $ over State _ { visibility = v, highlightedIndex = Just 0 }
        H.raise $ visibilityChanged v

  , receive: \input ->
      H.modify_ (over State \st -> st { render = input.render })
  
  , andThen: \(Tuple act1 act2) ->
      handleAction' act1 *> handleAction' act2
  }
  where
  handleAction' = handleAction handleExtraActions
  getTargetIndex st = case _ of
    Index i -> i
    Prev -> case st.highlightedIndex of
      Just i | i /= 0 -> i - 1
      _ -> st.lastIndex
    Next -> case st.highlightedIndex of
      Just i | i /= st.lastIndex -> i + 1
      _ -> 0

-- Just the normal Halogen eval
handleQuery 
  :: forall item st act ps msg m a
   . MonadAff m 
  => (Variant act -> H.HalogenM (State item st act ps m) (Action item st act ps m) ps (Message item msg) m Unit)
  -> Query item st act ps m a
  -> H.HalogenM (State item st act ps m) (Action item st act ps m) ps (Message item msg) m (Maybe a)
handleQuery handleExtraActions = case _ of
  GetVisibility f -> do
    (State st) <- H.get
    pure (Just (f st.visibility))

  ReplaceItems items a -> do
    H.modify_ $ over State _
      { items = items
      , lastIndex = length items - 1
      , highlightedIndex = Nothing 
      }
    pure (Just a)

  SendQuery box -> 
    H.HalogenM $ liftF $ H.ChildQuery box

  PerformAction act a -> do
    handleAction handleExtraActions (unsafeCoerce act)
    pure (Just a)
