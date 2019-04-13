-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select where

import Prelude

import Control.Monad.Free (liftF)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for_, traverse, traverse_)
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.ChildQuery (ChildQueryBox)
import Prim.Row as Row
import Record.Builder as Builder
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

data Action act
  = Search String
  | Highlight Target
  | Select Target (Maybe ME.MouseEvent)
  | ToggleClick ME.MouseEvent
  | Focus Boolean
  | Key KE.KeyboardEvent
  | PreventClick ME.MouseEvent
  | SetVisibility Visibility
  | Initialize (Maybe (Action act))
  | Action act

type Action' = Action Void

-----
-- QUERIES

data Query query ps a
  = Send (ChildQueryBox ps (Maybe a))
  | Query (query a)

type Query' = Query (Const Void) ()

-----
-- Message

data Message
  = Searched String
  | Selected Int
  | VisibilityChanged Visibility

-----
-- HELPER TYPES

-- | The component slot type for easy use in a parent component
type Slot query ps msg = H.Slot (Query query ps) msg

-- | The component slot type when there is no extension
type Slot' = Slot (Const Void) () Void

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
data Visibility = Off | On
derive instance eqVisibility :: Eq Visibility
derive instance ordVisibility :: Ord Visibility

-- | Text-driven inputs will operate like a normal search-driven selection component.
-- | Toggle-driven inputs will capture key streams and debounce in reverse (only notify
-- | about searches when time has expired).
data InputType = Text | Toggle

-- | The component state
type State st =
  { inputType :: InputType
  , search :: String
  , debounceTime :: Milliseconds
  , debounceRef :: Maybe (Ref (Maybe Debouncer))
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int
  , getItemCount :: {| st } -> Int
  | st
  }

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

type Input st =
  { inputType :: InputType
  , search :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , getItemCount :: {| st } -> Int
  | st
  }

type Spec st query act ps msg m =
  { -- usual Halogen component spec 
    render 
      :: State st 
      -> H.ComponentHTML (Action act) ps m
    
    -- handle additional actions provided to the component
  , handleAction 
      :: act
      -> H.HalogenM (State st) (Action act) ps msg m Unit

    -- handle additional queries provided to the component
  , handleQuery 
      :: forall a
       . query a 
      -> H.HalogenM (State st) (Action act) ps msg m (Maybe a)

    -- handle messages emitted by the component; provide H.raise to simply
    -- raise the Select messages to the parent.
  , handleMessage 
      :: Message
      -> H.HalogenM (State st) (Action act) ps msg m Unit

    -- optionally handle input on parent re-renders; off by default, but use 
    -- `Just <<< Receive` to enable Select's default receiver 
  , receive 
      :: Input st 
      -> Maybe (Action act)

    -- perform some action when the component initializes.
  , initialize 
      :: Maybe (Action act)

    -- optionally perform some action on initialization. disabled by default.
  , finalize
      :: Maybe (Action act)
  }

type Spec' st m = Spec st (Const Void) Void () Void m

defaultSpec :: forall st query act ps msg m. Spec st query act ps msg m
defaultSpec = 
  { render: const (HH.text mempty)
  , handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  , handleMessage: const (pure unit)
  , receive: const Nothing
  , initialize: Nothing
  , finalize: Nothing
  }
  
component
  :: forall st query act ps msg m
   . MonadAff m
  => Row.Lacks "debounceRef" st
  => Row.Lacks "visibility" st
  => Row.Lacks "highlightedIndex" st
  => Spec st query act ps msg m
  -> H.Component HH.HTML (Query query ps) (Input st) msg m
component spec = H.mkComponent
  { initialState
  , render: spec.render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery spec.handleQuery
      , handleAction = handleAction spec.handleAction spec.handleMessage
      , initialize = Just (Initialize spec.initialize)
      , receive = spec.receive
      , finalize = spec.finalize
      }
  }
  where
  initialState :: Input st -> State st
  initialState = Builder.build pipeline
    where
    pipeline = 
      Builder.modify (SProxy :: _ "search") (fromMaybe "")
        >>> Builder.modify (SProxy :: _ "debounceTime") (fromMaybe mempty)
        >>> Builder.insert (SProxy :: _ "debounceRef") Nothing
        >>> Builder.insert (SProxy :: _ "visibility") Off
        >>> Builder.insert (SProxy :: _ "highlightedIndex") Nothing

handleQuery
  :: forall st query act ps msg m a
   . MonadAff m
  => (query a -> H.HalogenM (State st) (Action act) ps msg m (Maybe a))
  -> Query query ps a
  -> H.HalogenM (State st) (Action act) ps msg m (Maybe a)
handleQuery handleQuery' = case _ of
  Send box ->
    H.HalogenM $ liftF $ H.ChildQuery box

  Query query ->
    handleQuery' query

handleAction
  :: forall st act ps msg m
   . MonadAff m
  => Row.Lacks "debounceRef" st
  => Row.Lacks "visibility" st
  => Row.Lacks "highlightedIndex" st
  => (act -> H.HalogenM (State st) (Action act) ps msg m Unit)
  -> (Message -> H.HalogenM (State st) (Action act) ps msg m Unit)
  -> Action act
  -> H.HalogenM (State st) (Action act) ps msg m Unit
handleAction handleAction' handleMessage = case _ of
  Initialize mbAction -> do
    ref <- H.liftEffect $ Ref.new Nothing
    H.modify_ _ { debounceRef = Just ref }
    for_ mbAction handle
  
  Search str -> do
    st <- H.get
    ref <- H.liftEffect $ map join $ traverse Ref.read st.debounceRef
    H.modify_ _ { search = str }
    void $ H.fork $ handle $ SetVisibility On

    case st.inputType, ref of
      Text, Nothing -> unit <$ do
        var   <- H.liftAff AVar.empty
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put unit var

        -- This compututation will fork and run in the background. When the
        -- var is finally filled, the action will run
        _ <- H.fork do
          _ <- H.liftAff $ AVar.take var
          void $ H.liftEffect $ traverse_ (Ref.write Nothing) st.debounceRef
          H.modify_ _ { highlightedIndex = Just 0 }
          newState <- H.get
          handleMessage $ Searched newState.search

        void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

      Text, Just debouncer -> do
        let var = debouncer.var
        _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put unit var
        void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

      -- Key stream is not yet implemented. However, this should capture user
      -- key events and expire their search after a set number of milliseconds.
      _, _ -> pure unit

  Highlight target -> do
    st <- H.get
    when (st.visibility == Off) do
      H.modify_ _ { highlightedIndex = Just $ getTargetIndex st target }

  Select target mbEv -> do
    for_ mbEv (H.liftEffect <<< preventDefault <<< ME.toEvent)
    st <- H.get
    when (st.visibility == On) case target of
      Index ix -> handleMessage $ Selected ix
      Next -> handleMessage $ Selected $ getTargetIndex st target
      Prev -> handleMessage $ Selected $ getTargetIndex st target

  ToggleClick ev -> do
    H.liftEffect $ preventDefault $ ME.toEvent ev
    st <- H.get
    case st.visibility of
      On -> do
        handle $ Focus false
        handle $ SetVisibility Off
      Off -> do
        handle $ Focus true
        handle $ SetVisibility On

  Focus shouldFocus -> do
    inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
    for_ inputElement \el -> H.liftEffect case shouldFocus of
      true -> HTMLElement.focus el
      _ -> HTMLElement.blur el

  Key ev -> do
    void $ H.fork $ handle $ SetVisibility On
    let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
    case KE.code ev of
      "ArrowUp" ->
        preventIt *> handle (Highlight Prev)
      "ArrowDown" ->
        preventIt *> handle (Highlight Next)
      "Escape" -> do
        inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
        preventIt
        for_ inputElement (H.liftEffect <<< HTMLElement.blur)
      "Enter" -> do
        st <- H.get
        preventIt
        for_ st.highlightedIndex \ix ->
          handle $ Select (Index ix) Nothing
      otherKey -> pure unit

  PreventClick ev ->
    H.liftEffect $ preventDefault $ ME.toEvent ev

  SetVisibility v -> do
    st <- H.get
    when (st.visibility /= v) do
      H.modify_ _ { visibility = v, highlightedIndex = Just 0 }
      handleMessage $ VisibilityChanged v

  Action act -> handleAction' act

  where
  -- eta-expansion is necessary to avoid infinite recursion
  handle act = handleAction handleAction' handleMessage act

  getTargetIndex st = case _ of
    Index i -> i
    Prev -> case st.highlightedIndex of
      Just i | i /= 0 -> i - 1
      _ -> lastIndex st
    Next -> case st.highlightedIndex of
      Just i | i /= lastIndex st -> i + 1
      _ -> 0
    where
    -- we know that the getItemCount function will only touch user fields,
    -- and that the state record contains *at least* the user fields, so
    -- this saves us from a set of unnecessary record deletions / modifications
    userState :: State st -> {| st }
    userState = unsafeCoerce

    lastIndex :: State st -> Int
    lastIndex = (_ - 1) <<< st.getItemCount <<< userState



