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
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-----
-- ACTIONS

data Action st query ps
  = Search String
  | Highlight Target
  | Select Target (Maybe ME.MouseEvent)
  | ToggleClick ME.MouseEvent
  | Focus Boolean
  | Key KE.KeyboardEvent
  | PreventClick ME.MouseEvent
  | SetVisibility Visibility
  | Initialize
  | AndThen (Action st query ps) (Action st query ps)
  | Receive (Input st)
  | AsAction (Query query ps Unit)

type Action' = Action () (Const Void) ()

-----
-- QUERIES

data Query query ps a
  = Send (ChildQueryBox ps (Maybe a))
  | Embed (query a)

type Query' = Query (Const Void) ()

-----
-- MESSAGES

data Message msg
  = Searched String
  | Selected Int
  | VisibilityChanged Visibility
  | Raised msg

type Message' = Message Void

-----
-- HELPER TYPES

-- | The component slot type for easy use in a parent component
type Slot query ps msg = H.Slot (Query query ps) (Message msg)

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
  = Text
  | Toggle

-- | The component state
type State st =
  { inputType :: InputType
  , search :: String
  , debounceTime :: Milliseconds
  , debounceRef :: Maybe (Ref (Maybe Debouncer))
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex :: Int
  | st
  }

type State' = State ()

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

type Input st =
  { inputType :: InputType
  , search :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , lastIndex :: Int
  | st
  }

type Input' = Input ()

type Spec st query ps msg m =
  { render :: State st -> H.ComponentHTML (Action st query ps) ps m
  , handleQuery :: forall a. query a -> H.HalogenM (State st) (Action st query ps) ps (Message msg) m (Maybe a)
  , handleMessage :: Message msg -> H.HalogenM (State st) (Action st query ps) ps (Message msg) m Unit
  , initialize :: Maybe (Action st query ps)
  , receive :: Input st -> Maybe (Action st query ps)
  }

defaultSpec :: forall st query ps msg m. Spec st query ps msg m
defaultSpec = 
  { render: const (HH.text mempty)
  , handleQuery: const (pure Nothing)
  , handleMessage: const (pure unit)
  , initialize: Just Initialize
  , receive: Just <<< Receive
  }
  
component
  :: forall st query ps msg m
   . MonadAff m
  => Row.Lacks "debounceRef" st
  => Row.Lacks "visibility" st
  => Row.Lacks "highlightedIndex" st
  => Spec st query ps msg m
  -> H.Component HH.HTML (Query query ps) (Input st) (Message msg) m
component spec = H.mkComponent
  { initialState
  , render: spec.render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery spec.handleQuery
      , handleAction = handleAction spec.handleQuery spec.handleMessage
      , initialize = spec.initialize
      , receive = spec.receive
      }
  }
  where
  initialState :: Input st -> State st
  initialState input = Builder.build pipeline input
    where
    pipeline = 
      Builder.modify (SProxy :: _ "search") (fromMaybe "")
        >>> Builder.modify (SProxy :: _ "debounceTime") (fromMaybe mempty)
	>>> Builder.insert (SProxy :: _ "debounceRef") Nothing
        >>> Builder.insert (SProxy :: _ "visibility") Off
	>>> Builder.insert (SProxy :: _ "highlightedIndex") Nothing

handleAction
  :: forall st query ps msg m
   . MonadAff m
  => Row.Lacks "debounceRef" st
  => Row.Lacks "visibility" st
  => Row.Lacks "highlightedIndex" st
  => (forall a. query a -> H.HalogenM (State st) (Action st query ps) ps (Message msg) m (Maybe a))
  -> (Message msg -> H.HalogenM (State st) (Action st query ps) ps (Message msg) m Unit)
  -> Action st query ps
  -> H.HalogenM (State st) (Action st query ps) ps (Message msg) m Unit
handleAction handleExtraQuery handleMessage = case _ of
  Initialize -> do
    ref <- H.liftEffect $ Ref.new Nothing
    H.modify_ _ { debounceRef = Just ref }
  
  -- We want to update user-added state values from the parent as well as the lastIndex but
  -- not internal fields. Because we don't know what extra fields the user may have provided,
  -- we have to use Builder here to go off of their concrete provided Input type. 
  Receive input -> do
    st <- H.get
    let 
      pipeline = 
        Builder.modify (SProxy :: SProxy "search") (const st.search)
          >>> Builder.modify (SProxy :: SProxy "debounceTime") (const st.debounceTime)
	  >>> Builder.insert (SProxy :: SProxy "debounceRef") st.debounceRef
          >>> Builder.insert (SProxy :: SProxy "visibility") st.visibility
	  >>> Builder.insert (SProxy :: SProxy "highlightedIndex") st.highlightedIndex
    H.put (Builder.build pipeline input)

  Search str -> do
    st <- H.get
    ref <- H.liftEffect $ map join $ traverse Ref.read st.debounceRef
    H.modify_ _ { search = str }
    void $ H.fork $ handleAction' $ SetVisibility On

    case st.inputType, ref of
      Text, Nothing -> unit <$ do
        var   <- H.liftAff AVar.empty
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put unit var

        -- This compututation will fork and run in the background. When the
        -- var is finally filled, the action will run (raise a new search)
        _ <- H.fork do
          _ <- H.liftAff $ AVar.take var
          void $ H.liftEffect $ traverse_ (Ref.write Nothing) st.debounceRef
          H.modify_ _ { highlightedIndex = Just 0 }
          newState <- H.get
          raise' $ Searched newState.search

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
      let targetIndex = getTargetIndex st target
      H.modify_ _ { highlightedIndex = Just targetIndex }

  Select target mbEv -> do
    for_ mbEv (H.liftEffect <<< preventDefault <<< ME.toEvent)
    st <- H.get
    when (st.visibility == On) case target of
      Index ix -> raise' $ Selected ix
      Next -> raise' $ Selected $ getTargetIndex st target
      Prev -> raise' $ Selected $ getTargetIndex st target

  ToggleClick ev -> do
    H.liftEffect $ preventDefault $ ME.toEvent ev
    st <- H.get
    case st.visibility of
      On -> do
        handleAction' $ Focus false
        handleAction' $ SetVisibility Off
      Off -> do
        handleAction' $ Focus true
        handleAction' $ SetVisibility On

  Focus shouldFocus -> do
    inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
    for_ inputElement \el -> H.liftEffect case shouldFocus of
      true -> HTMLElement.focus el
      _ -> HTMLElement.blur el

  Key ev -> do
    void $ H.fork $ handleAction' $ SetVisibility On
    let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
    case KE.code ev of
      "ArrowUp" ->
        preventIt *> handleAction' (Highlight Prev)
      "ArrowDown" ->
        preventIt *> handleAction' (Highlight Next)
      "Escape" -> do
        inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
        preventIt
        for_ inputElement (H.liftEffect <<< HTMLElement.blur)
      "Enter" -> do
        st <- H.get
        preventIt
        for_ st.highlightedIndex \ix ->
          handleAction' $ Select (Index ix) Nothing
      otherKey -> pure unit

  PreventClick ev ->
    H.liftEffect $ preventDefault $ ME.toEvent ev

  SetVisibility v -> do
    st <- H.get
    when (st.visibility /= v) do
      H.modify_ _ { visibility = v, highlightedIndex = Just 0 }
      raise' $ VisibilityChanged v

  AndThen act1 act2 -> do
    handleAction' act1
    handleAction' act2
    pure unit
  
  AsAction query -> do
    _ <- handleQuery handleExtraQuery query
    pure unit

  where
  -- recursively evaluate an action
  handleAction' act = handleAction handleExtraQuery handleMessage act

  -- attempt to handle a message internally, and then raise the
  -- message to a parent component.
  raise' msg = do
    void $ H.fork $ handleMessage msg
    H.raise msg

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
  :: forall st query ps msg m a
   . MonadAff m
  => (query a -> H.HalogenM (State st) (Action st query ps) ps (Message msg) m (Maybe a))
  -> Query query ps a
  -> H.HalogenM (State st) (Action st query ps) ps (Message msg) m (Maybe a)
handleQuery handleExtraQuery = case _ of
  Send box ->
    H.HalogenM $ liftF $ H.ChildQuery box

  Embed query ->
    handleExtraQuery query

