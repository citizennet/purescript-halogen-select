module SelectHook where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff (Milliseconds)
import Effect.Aff.Class (class MonadAff)
import Example.Hooks.UseDebouncer (UseDebouncer, useDebouncer)
import Halogen as H
import Halogen.Hooks (Hook, HookM, StateToken, UseState, useState)
import Halogen.Hooks as Hooks
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

data Action action
  = Search String
  | Highlight Target
  | Select Target (Maybe ME.MouseEvent)
  | ToggleClick ME.MouseEvent
  | Focus Boolean
  | Key KE.KeyboardEvent
  | PreventClick ME.MouseEvent
  | SetVisibility Visibility
  | Initialize (Maybe action)
  | Action action

data Event
  = Searched String
  | Selected Int
  | VisibilityChanged Visibility

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

type SelectInput slots output m =
  { inputType :: InputType
  , search :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , getItemCount :: HookM slots output m Int
  , handleEvent :: Event -> HookM slots output m Unit
  }

type SelectState =
  { search :: String
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int
  }

newtype UseSelect hooks =
  UseSelect (UseDebouncer String (UseState SelectState hooks))

derive instance newtypeUseSelect :: Newtype (UseSelect hooks) _

useSelect :: forall slots output m
           . MonadAff m
          => SelectInput slots output m
          -> Hook slots output m UseSelect ({ select :: SelectState /\ StateToken SelectState, searchDebouncer :: String -> HookM slots output m Unit })
useSelect inputRec = Hooks.wrap Hooks.do
  state /\ stateToken <- useState
    { search: fromMaybe "" inputRec.search
    , visibility: Off
    , highlightedIndex: Nothing
    }

  let debounceTime = fromMaybe mempty inputRec.debounceTime

  searchDebouncer <- useDebouncer debounceTime \lastSearchState -> Hooks.do
    case inputRec.inputType of
      Text -> do
        Hooks.modify_ stateToken (_ { highlightedIndex = Just 0 })
        inputRec.handleEvent (Searched lastSearchState)

      -- Key stream is not yet implemented. However, this should capture user
      -- key events and expire their search after a set number of milliseconds.
      _ -> pure unit

  Hooks.pure { select: state /\ stateToken, searchDebouncer }

handleAction :: forall slots output action m
              . MonadAff m
             => StateToken SelectState
             -> (String -> HookM slots output m Unit)
             -> HookM slots output m Int
             -> (action -> HookM slots output m Unit)
             -> (Event -> HookM slots output m Unit)
             -> Action action
             -> HookM slots output m Unit
handleAction stateToken searchDebouncer getItemCount handleAction' handleEvent = case _ of
  Initialize mbAction -> do
    for_ mbAction handleAction'

  Search str -> do
    Hooks.modify_ stateToken (_ { search = str })
    void $ Hooks.fork $ handle $ SetVisibility On
    searchDebouncer str

  Highlight target -> do
    st <- Hooks.get stateToken
    when (st.visibility == On) do
      itemCount <- getItemCount
      Hooks.modify_ stateToken (_ { highlightedIndex = Just $ getTargetIndex st itemCount target })

  Select target mbEv -> do
    for_ mbEv (H.liftEffect <<< preventDefault <<< ME.toEvent)
    st <- Hooks.get stateToken
    when (st.visibility == On) case target of
      Index ix -> handleEvent $ Selected ix
      Next -> do
        itemCount <- getItemCount
        handleEvent $ Selected $ getTargetIndex st itemCount target
      Prev -> do
        itemCount <- getItemCount
        handleEvent $ Selected $ getTargetIndex st itemCount target

  ToggleClick ev -> do
    H.liftEffect $ preventDefault $ ME.toEvent ev
    st <- Hooks.get stateToken
    case st.visibility of
      On -> do
        handle $ Focus false
        handle $ SetVisibility Off
      Off -> do
        handle $ Focus true
        handle $ SetVisibility On

  Focus shouldFocus -> do
    inputElement <- Hooks.getHTMLElementRef $ H.RefLabel "select-input"
    for_ inputElement \el -> H.liftEffect case shouldFocus of
      true -> HTMLElement.focus el
      _ -> HTMLElement.blur el

  Key ev -> do
    void $ Hooks.fork $ handle $ SetVisibility On
    let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
    case KE.key ev of
      x | x == "ArrowUp" || x == "Up" ->
        preventIt *> handle (Highlight Prev)
      x | x == "ArrowDown" || x == "Down" ->
        preventIt *> handle (Highlight Next)
      x | x == "Escape" || x == "Esc" -> do
        inputElement <- Hooks.getHTMLElementRef $ H.RefLabel "select-input"
        preventIt
        for_ inputElement (H.liftEffect <<< HTMLElement.blur)
      "Enter" -> do
        st <- Hooks.get stateToken
        preventIt
        for_ st.highlightedIndex \ix ->
          handle $ Select (Index ix) Nothing
      otherKey -> pure unit

  PreventClick ev ->
    H.liftEffect $ preventDefault $ ME.toEvent ev

  SetVisibility v -> do
    st <- Hooks.get stateToken
    when (st.visibility /= v) do
      Hooks.modify_ stateToken (_ { visibility = v, highlightedIndex = Just 0 })
      handleEvent $ VisibilityChanged v

  Action act -> handleAction' act

  where
  -- eta-expansion is necessary to avoid infinite recursion
  handle :: Action action -> HookM slots output m Unit
  handle act = handleAction stateToken searchDebouncer getItemCount handleAction' handleEvent act

  getTargetIndex st itemCount = case _ of
    Index i -> i
    Prev -> case st.highlightedIndex of
      Just i | i /= 0 -> i - 1
      _ -> itemCount - 1
    Next -> case st.highlightedIndex of
      Just i | i /= (itemCount - 1) -> i + 1
      _ -> 0
