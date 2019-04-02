-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), Variant, expand, inj, onMatch)
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.ChildQuery (ChildQueryBox)
import Renderless.State (getState, modifyState_, modifyStore)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-----
-- ACTIONS

type Action item ps v m = Variant
  ( search :: String 
  , highlight :: Target
  , select :: Target
  , focus :: Boolean
  , key :: KE.KeyboardEvent
  , preventClick :: ME.MouseEvent
  , setVisibility :: Visibility
  , receive :: Input item ps v m
  , initialize :: Unit
  , andThen :: Tuple (Action item ps v m) (Action item ps v m)
  | v
  )

search :: forall item ps v m. String -> Action item ps v m
search = inj (SProxy :: _ "search")

highlight :: forall item ps v m. Target -> Action item ps v m
highlight = inj (SProxy :: _ "highlight")

select :: forall item ps v m. Target -> Action item ps v m
select = inj (SProxy :: _ "select")

focus :: forall item ps v m. Boolean -> Action item ps v m
focus = inj (SProxy :: _ "focus")

key :: forall item ps v m. KE.KeyboardEvent -> Action item ps v m
key = inj (SProxy :: _ "key")

preventClick :: forall item ps v m. KE.KeyboardEvent -> Action item ps v m
preventClick = inj (SProxy :: _ "preventClick")

setVisibility :: forall item ps v m. Visibility -> Action item ps v m
setVisibility = inj (SProxy :: _ "setVisibility")

receive :: forall item ps v m. Input item ps v m -> Action item ps v m
receive = inj (SProxy :: _ "input") 

initialize :: forall item ps v m. Unit -> Action item ps v m
initialize = inj (SProxy :: _ "initialize") 

andThen :: forall item ps v m. Action item ps v m -> Action item ps v m -> Action item ps v m
andThen act1 = inj (SProxy :: _ "setVisibility") <<< Tuple act1

-----
-- QUERIES 

data Query item ps v m a
  = GetVisibility (Visibility -> a)
  | ReplaceItems (Array item) a
  -- send a query through `Select` to a child component
  | SendQuery (ChildQueryBox ps (Maybe a))
  | PerformAction (Action item ps v m) a

-----
-- MESSAGES

type Message item out = Variant
  ( searched :: String
  , selected :: item
  , visibilityChanged :: Visibility
  | out
  )

searched :: forall item out. String -> Message item out
searched = inj (SProxy :: _ "searched")

selected :: forall item out. item -> Message item out
selected = inj (SProxy :: _ "selected")

visibilityChanged :: forall item out. Visibility -> Message item out
visibilityChanged = inj (SProxy :: _ "visibility")

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

-- | The component's state, as packed in `Store`
type StateStore item ps v m =
  Store State (H.ComponentHTML (Action item ps v m) ps m)

-- | The component's state, once unpacked from `Store`.
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
type State item =
  { inputType        :: InputType
  , search           :: String
  , debounceTime     :: Milliseconds
  , debounceRef      :: Maybe (Ref (Maybe Debouncer))
  , items            :: Array item
  , visibility       :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  }

-- | Represents a running computation that, when it completes, will trigger debounced
-- | .cts.
type Debouncer =
  { var   :: AVar Unit
  , fiber :: Fiber Unit 
  }

-- | The component's input type, which includes the component's render function. This
-- | render function can also be used to share data with the parent component, as every
-- | time the parent re-renders, the render function will refresh in `Select`.
newtype Input item ps v m = Input
  { inputType     :: InputType
  , items         :: Array item
  , initialSearch :: Maybe String
  , debounceTime  :: Maybe Milliseconds
  , render        :: State item -> H.ComponentHTML (Action item ps v m) ps m
  }

type SelectHalogenM item ps v out m a =
  H.HalogenM (StateStore item ps v m) (Action item ps v m) ps (Message out) m a

component 
  :: forall item ps v out m
   . MonadAff m
  => (Variant v -> SelectHalogenM item ps v out m)
  -> H.Component HH.HTML (Query item ps v m) (Input item ps v m) (Message out) m
component = H.mkComponent
  { initialState
  , render: extract
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , receive = Just <<< receive
      , initialize = Just (initialize unit)
      }
  }
  where
  initialState i = store i.render
    { inputType: i.inputType
    , search: fromMaybe "" i.initialSearch
    , debounceTime: fromMaybe (Milliseconds 0.0) i.debounceTime
    , debounceRef: Nothing
    , items: i.items
    , highlightedIndex: Nothing
    , visibility: Off
    , lastIndex: length i.items - 1
    }

handleAction 
  :: forall item ps v out m
   . MonadAff m 
  => (Variant v -> SelectHalogenM item ps v out m)
  -> Action item ps v m
  -> SelectHalogenM item ps v out m Unit
handleAction handleExtraActions = flip onMatch handleExtraActions
  { initialize: \_ -> do
      ref <- H.liftEffect $ Ref.new Nothing
      modifyState_ _ { debounceRef = Just ref }

  , search: \str -> do
      st <- getState
      ref  <- H.liftEffect $ map join $ traverse Ref.read st.debounceRef
      modifyState_ _ { search = str }
      void $ H.fork $ handleAction $ setVisibility On

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
            modifyState_ _ { highlightedIndex = Just 0 }
            newState <- getState
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
      st <- getState
      when (st.visibility /= Off) $ do
        let highlightedIndex = case target of
              Prev  -> case st.highlightedIndex of
                Just i | i /= 0 ->
                  Just (i - 1)
                _ ->
                  Just st.lastIndex
              Next  -> case st.highlightedIndex of
                Just i | i /= st.lastIndex ->
                  Just (i + 1)
                _ ->
                  Just 0
              Index i ->
                Just i
        modifyState_ _ { highlightedIndex = highlightedIndex }

  , select: \index -> do
      st <- getState
      when (st.visibility == On) $
        for_ (st.items !! index)
          \item -> H.raise (selected item)

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
          st <- getState
          preventIt
          for_ st.highlightedIndex (handleAction' <<< select)
        otherKey    -> pure unit

  , preventClick: H.liftEffect <<< preventDefault <<< ME.toEvent

  , setVisibility: \v -> do
      st <- getState
      when (st.visibility /= v) do
        modifyState_ _ { visibility = v, highlightedIndex = Just 0 }
        H.raise $ visibilityChanged v

  , receive: \input -> do
      modifyStore input.render identity
  
  , andThen: \(Tuple act1 act2) ->
      handleAction' act1 *> handleAction' act2
  }
  where
  handleAction' = handleAction handleExtraActions

-- Just the normal Halogen eval
handleQuery 
  :: forall item ps v out m a
   . MonadAff m 
  => (Variant v -> SelectHalogenM item ps v out m a)
  -> Query item ps v m a
  -> SelectHalogenM item ps v out m a
handleQuery handleExtraActions = case _ of

  GetVisibility f -> do
    st <- getState
    pure (Just (f st.visibility))

  ReplaceItems items a -> do
    modifyState_ _
      { items = items
      , lastIndex = length items - 1
      , highlightedIndex = Nothing 
      }
    pure (Just a)

  SendQuery box -> 
    H.HalogenM $ liftF $ H.ChildQuery box

  PerformAction act a -> do
    handleAction handleExtraActions (expand act)
    pure (Just a)