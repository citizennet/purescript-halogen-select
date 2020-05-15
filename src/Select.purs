module Select where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (Hook, HookM, StateId, UseState, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Actions.Events (preventKeyEvent, preventMouseEvent)
import Halogen.Hooks.Extra.Hooks (UseDebouncer, useDebouncer)
import Web.Event.Event as E
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.FocusEvent as FE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

-- | The properties that must be supported by the HTML element that serves
-- | as a menu toggle. This should be used with toggle-driven `Select` components.
type ToggleProps props =
  ( onFocus :: FE.FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onMouseDown :: ME.MouseEvent
  , onClick :: ME.MouseEvent
  , onBlur :: FE.FocusEvent
  , tabIndex :: Int
  | props
  )

-- | The properties that must be supported by the HTML element that serves
-- | as a text input. This should be used with input-driven `Select` components.
type InputProps props =
  ( onFocus :: FE.FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onInput :: E.Event
  , value :: String
  , onMouseDown :: ME.MouseEvent
  , onBlur :: FE.FocusEvent
  , tabIndex :: Int
  | props
  )

-- | The properties that must be supported by the HTML element that acts as a
-- | selectable "item" in your UI. This should be attached to every item that
-- | can be selected.
type ItemProps props =
  ( onMouseDown :: ME.MouseEvent
  , onMouseOver :: ME.MouseEvent
  | props
  )

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

type SelectInput m =
  { inputType :: InputType
  , search :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , getItemCount :: HookM m Int
  , pushNewSearch  :: String -> HookM m Unit
  , pushVisibilityChanged :: Visibility -> HookM m Unit
  , pushSelectedIdxChanged :: Int -> HookM m Unit
  }

type SelectState =
  { search :: String
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int
  }

newtype SelectReturn m = SelectReturn
  { search :: String
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int

  , setFocus :: Boolean -> HookM m Unit
  , setVisibility :: Visibility -> HookM m Unit
  , clearSearch :: HookM m Unit

  , toggleProps
      :: forall toggleProps
       . Array (HP.IProp (ToggleProps toggleProps) (HookM m Unit))
  , itemProps
      :: forall itemProps
       . Int
      -> Array (HP.IProp (ItemProps itemProps) (HookM m Unit))
  , containerProps
      :: forall containerProps
       . Array (HP.IProp (onMouseDown :: ME.MouseEvent | containerProps) (HookM m Unit))
  , inputProps
      :: forall inputProps
       . Array (HP.IProp (InputProps inputProps) (HookM m Unit))
  }

-- | When pushing all Select events into the same handler, this data type
-- | distinguishes one event type from another.
data SelectEvent
  = NewSearch String
  | VisibilityChangedTo Boolean
  | SelectedIndex Int

newtype UseSelect hooks = UseSelect
  (UseDebouncer String
  (UseState SelectState hooks))

derive instance newtypeUseSelect :: Newtype (UseSelect hooks) _

-- | A `SelectInput` value whose defaults can be overrided. **Note**:
-- | `getItemCount` must be overrided:
-- |
-- | Default values are:
-- | ```
-- | { inputType: Toggle
-- | , search: Nothing
-- | , debounceTime: Nothing
-- | , getItemCount: pure 0 -- this must be overrided!
-- | , pushNewSearch: \_ -> pure unit
-- | , pushVisibilityChanged: \_ -> pure unit
-- | , pushSelectedIdxChanged: \_ -> pure unit
-- | }
-- | ```
-- |
-- | Example:
-- | ```
-- | events <- useEvent
-- | SelectReturn select <- useSelect $ selectInput
-- |   { getItemCount = pure (length items)
-- |   , pushNewSearch = events.push
-- |   }
-- | ```
selectInput :: forall m. SelectInput m
selectInput =
  { inputType: Toggle
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: pure 0
  , pushNewSearch: mempty
  , pushVisibilityChanged: mempty
  , pushSelectedIdxChanged: mempty
  }

useSelect
  :: forall m
   . MonadAff m
  => SelectInput m
  -> Hook m UseSelect (SelectReturn m)
useSelect inputRec =
  let
    initialSearchValue = fromMaybe "" inputRec.search
    debounceTime = fromMaybe mempty inputRec.debounceTime
  in Hooks.wrap Hooks.do
    state /\ stateId <- useState
      { search: initialSearchValue
      , visibility: Off
      , highlightedIndex: Nothing
      }

    searchDebouncer <- useDebouncer debounceTime \lastSearchState -> Hooks.do
      case inputRec.inputType of
        Text -> do
          -- modifyState (_ { highlightedIndex = (Just 0) })
          inputRec.pushNewSearch lastSearchState

        -- Key stream is not yet implemented. However, this should capture user
        -- key events and expire their search after a set number of milliseconds.
        _ -> pure unit

    Hooks.pure $ SelectReturn
      -- state
      { search: state.search
      , visibility: state.visibility
      , highlightedIndex: state.highlightedIndex

      -- actions
      , setFocus
      , setVisibility: setVisibility stateId
      , clearSearch: Hooks.modify_ stateId (_ { search = "" })

      -- props
      , toggleProps: toggleProps stateId
      , itemProps: itemProps stateId
      , containerProps
      , inputProps: inputProps stateId searchDebouncer
      }
    where
      -- | An array of `IProps` with `ToggleProps`. It
      -- | allows the toggle element to register key events for navigation or highlighting,
      -- | record open and close events based on focus and blur, and to be focused with
      -- | the tab key.
      -- |
      -- | ```purescript
      -- | renderToggle = div (setToggleProps [ class "btn-class" ]) [ ...html ]
      -- | ```
      toggleProps :: forall toggleProps. _ -> Array (HP.IProp (ToggleProps toggleProps) (HookM m Unit))
      toggleProps stateId =
        [ HE.onFocus \_ -> Just (setVisibility stateId On)
        , HE.onMouseDown \ev -> Just (toggleClick stateId ev)
        , HE.onKeyDown \ev -> Just (key stateId ev)
        , HE.onBlur \ev -> Just (setVisibility stateId Off)
        , HP.tabIndex 0
        , HP.ref (H.RefLabel "select-input")
        ]

      -- | An array of `IProps` with `ItemProps`. It
      -- | allows items to be highlighted and selected.
      -- |
      -- | This expects an index for use in highlighting. It's useful in combination
      -- | with `mapWithIndex`:
      -- |
      -- | ```purescript
      -- | renderItem index itemHTML =
      -- |   HH.li (setItemProps index [ props ]) [ itemHTML ]
      -- |
      -- | render = renderItem `mapWithIndex` itemsArray
      -- | ```
      itemProps :: forall itemProps. _ -> Int -> Array (HP.IProp (ItemProps itemProps) (HookM m Unit))
      itemProps stateId index =
        [ HE.onMouseDown \ev -> Just (select stateId (Index index) (Just ev))
        , HE.onMouseOver \_ -> Just (highlight stateId (Index index))
        ]

      -- | An array of `IProps` with a `MouseDown`
      -- | handler. It prevents clicking on an item within an enclosing HTML element
      -- | from bubbling up a blur event to the DOM. This should be used on the parent
      -- | element that contains your items.
      containerProps :: forall containerProps. Array (HP.IProp (onMouseDown :: ME.MouseEvent | containerProps) (HookM m Unit))
      containerProps =
        [ HE.onMouseDown \ev -> Just (preventMouseEvent ev) ]


      -- | An array of `IProps` with `InputProps`. It
      -- | allows the input element to capture string values, register key events for
      -- | navigation, record open and close events based on focus and blur, and to be
      -- | focused with the tab key.
      -- |
      -- | ```purescript
      -- | renderInput = input_ (setInputProps [ class "my-class" ])
      -- | ```
      inputProps :: forall inputProps. _ -> _ -> Array (HP.IProp (InputProps inputProps) (HookM m Unit))
      inputProps stateId searchDebouncer =
        [ HE.onFocus \_ -> Just (setVisibility stateId On)
        , HE.onKeyDown \ev -> Just (key stateId ev)
        , HE.onValueInput \str -> Just (handleSearch stateId searchDebouncer str)
        , HE.onMouseDown \_ -> Just (setVisibility stateId On)
        , HE.onBlur \_ -> Just (setVisibility stateId Off)
        , HP.tabIndex 0
        , HP.ref (H.RefLabel "select-input")
        ]

      getTargetIndex :: Maybe Int -> Int -> Target -> Int
      getTargetIndex highlightedIndex itemCount = case _ of
        Index i -> i
        Prev -> case highlightedIndex of
          Just i | i /= 0 -> i - 1
          _ -> itemCount - 1
        Next -> case highlightedIndex of
          Just i | i /= (itemCount - 1) -> i + 1
          _ -> 0

      setFocus :: Boolean -> HookM m Unit
      setFocus shouldFocus = do
        inputElement <- Hooks.getHTMLElementRef $ H.RefLabel "select-input"
        for_ inputElement \el -> H.liftEffect case shouldFocus of
          true -> HTMLElement.focus el
          _ -> HTMLElement.blur el

      setVisibility
        :: StateId SelectState
        -> Visibility
        -> HookM m Unit
      setVisibility stateId v = do
        st <- Hooks.get stateId
        when (st.visibility /= v) do
          Hooks.modify_ stateId (_ { visibility = v, highlightedIndex = Just 0 })
          inputRec.pushVisibilityChanged v

      handleSearch
        :: StateId SelectState
        -> (String -> HookM m Unit)
        -> String
        -> HookM m Unit
      handleSearch stateId searchDebouncer str = do
        Hooks.modify_ stateId (_ { search = str })
        void $ Hooks.fork $ setVisibility stateId On
        searchDebouncer str

      highlight
        :: StateId SelectState
        -> Target
        -> HookM m Unit
      highlight stateId target = do
        st <- Hooks.get stateId
        when (st.visibility == On) do
          itemCount <- inputRec.getItemCount
          let newIndex = Just (getTargetIndex st.highlightedIndex itemCount target)
          Hooks.modify_ stateId (_ { highlightedIndex = newIndex })

      select
        :: StateId SelectState
        -> Target
        -> Maybe MouseEvent
        -> HookM m Unit
      select stateId target mbEv = do
        for_ mbEv preventMouseEvent
        st <- Hooks.get stateId
        when (st.visibility == On) case target of
          Index ix -> inputRec.pushSelectedIdxChanged ix
          Next -> do
            itemCount <- inputRec.getItemCount
            inputRec.pushSelectedIdxChanged $ getTargetIndex st.highlightedIndex itemCount target
          Prev -> do
            itemCount <- inputRec.getItemCount
            inputRec.pushSelectedIdxChanged $ getTargetIndex st.highlightedIndex itemCount target

      toggleClick
        :: StateId SelectState
        -> MouseEvent
        -> HookM m Unit
      toggleClick stateId ev = do
        preventMouseEvent ev
        st <- Hooks.get stateId
        case st.visibility of
          On -> do
            setFocus false
            setVisibility stateId Off
          Off -> do
            setFocus true
            setVisibility stateId On

      key
        :: StateId SelectState
        -> KeyboardEvent
        -> HookM m Unit
      key stateId ev = do
        void $ Hooks.fork $ setVisibility stateId On
        let preventIt = preventKeyEvent ev
        case KE.key ev of
          x | x == "ArrowUp" || x == "Up" ->
            preventIt *> highlight stateId Prev
          x | x == "ArrowDown" || x == "Down" ->
            preventIt *> highlight stateId Next
          x | x == "Escape" || x == "Esc" -> do
            inputElement <- Hooks.getHTMLElementRef $ H.RefLabel "select-input"
            preventIt
            for_ inputElement (H.liftEffect <<< HTMLElement.blur)
          "Enter" -> do
            st <- Hooks.get stateId
            preventIt
            for_ st.highlightedIndex \ix ->
              select stateId (Index ix) Nothing
          otherKey -> pure unit
