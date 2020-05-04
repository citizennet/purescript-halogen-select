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
import Halogen.Hooks (Hook, HookM, UseState, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Actions.Events (preventKeyEvent, preventMouseEvent)
import Halogen.Hooks.Extra.Hooks.UseDebouncer (UseDebouncer, useDebouncer)
import Web.Event.Event as E
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.FocusEvent as FE
import Web.UIEvent.KeyboardEvent as KE
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

type SelectReturn
      m toggleProps itemProps containerProps inputProps =
  { search :: String
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int

  , setFocus :: Boolean -> HookM m Unit
  , setVisibility :: Visibility -> HookM m Unit
  , clearSearch :: HookM m Unit

  , toggleProps :: Array (HP.IProp (ToggleProps toggleProps) (HookM m Unit))
  , itemProps :: Int -> Array (HP.IProp (ItemProps itemProps) (HookM m Unit))
  , containerProps :: Array (HP.IProp (onMouseDown :: ME.MouseEvent | containerProps) (HookM m Unit))
  , inputProps :: Array (HP.IProp (InputProps inputProps) (HookM m Unit))
  }

-- | When pushing all Select events into the same handler, this data type
-- | distinguishes one event type from another.
data Event
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
-- | select <- useSelect $ selectInput
-- |   { getItemCount = pure (length items)
-- |   , pushNewSearch = events.push
-- |   }
-- | ```
selectInput :: forall m. SelectInput m
selectInput = do
  let
    ignoreEvent :: forall a. a -> HookM m Unit
    ignoreEvent = \_ -> pure unit

  { inputType: Toggle
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: pure 0
  , pushNewSearch: ignoreEvent
  , pushVisibilityChanged: ignoreEvent
  , pushSelectedIdxChanged: ignoreEvent
  }

useSelect
  :: forall m toggleProps itemProps containerProps inputProps
   . MonadAff m
  => SelectInput m
  -> Hook m UseSelect
        (SelectReturn m toggleProps itemProps containerProps inputProps)
useSelect inputRec =
  let
    initialSearchValue = fromMaybe "" inputRec.search
    debounceTime = fromMaybe mempty inputRec.debounceTime
  in Hooks.wrap Hooks.do
    state /\ tState <- useState
      { search: initialSearchValue
      , visibility: Off
      , highlightedIndex: Nothing
      }

    searchDebouncer <- useDebouncer debounceTime \lastSearchState -> Hooks.do
      case inputRec.inputType of
        Text -> do
          Hooks.modify_ tState (_ { highlightedIndex = (Just 0) })
          inputRec.pushNewSearch lastSearchState

        -- Key stream is not yet implemented. However, this should capture user
        -- key events and expire their search after a set number of milliseconds.
        _ -> pure unit

    Hooks.pure
      -- state
      { search: state.search
      , visibility: state.visibility
      , highlightedIndex: state.highlightedIndex

      -- actions
      , setFocus
      , setVisibility: setVisibility tState inputRec.pushVisibilityChanged
      , clearSearch: clearSearch tState

      -- props
      , toggleProps: toggleProps tState inputRec.pushVisibilityChanged inputRec.pushSelectedIdxChanged
      , itemProps: itemProps tState inputRec.pushSelectedIdxChanged
      , containerProps
      , inputProps: inputProps tState searchDebouncer inputRec.pushVisibilityChanged inputRec.pushSelectedIdxChanged
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
      toggleProps :: _ -> _ -> _ -> Array (HP.IProp (ToggleProps toggleProps) (HookM m Unit))
      toggleProps tState pushVisibilityChanged pushSelectedIdxChanged =
        [ HE.onFocus \_ -> Just (setVisibility tState pushVisibilityChanged On)
        , HE.onMouseDown \ev -> Just (toggleClick tState pushVisibilityChanged ev)
        , HE.onKeyDown \ev -> Just (key tState pushVisibilityChanged pushSelectedIdxChanged ev)
        , HE.onBlur \ev -> Just (setVisibility tState pushVisibilityChanged Off)
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
      itemProps :: _ -> _ -> Int -> Array (HP.IProp (ItemProps itemProps) (HookM m Unit))
      itemProps tState pushSelectedIdxChanged index =
        [ HE.onMouseDown \ev -> Just (select tState pushSelectedIdxChanged (Index index) (Just ev))
        , HE.onMouseOver \_ -> Just (highlight tState (Index index))
        ]

      -- | An array of `IProps` with a `MouseDown`
      -- | handler. It prevents clicking on an item within an enclosing HTML element
      -- | from bubbling up a blur event to the DOM. This should be used on the parent
      -- | element that contains your items.
      containerProps :: Array (HP.IProp (onMouseDown :: ME.MouseEvent | containerProps) (HookM m Unit))
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
      inputProps :: _ -> _ -> _ -> _ -> Array (HP.IProp (InputProps inputProps) (HookM m Unit))
      inputProps tState searchDebouncer pushVisibilityChanged pushSelectedIdxChanged =
        [ HE.onFocus \_ -> Just (setVisibility tState pushVisibilityChanged On)
        , HE.onKeyDown \ev -> Just (key tState pushVisibilityChanged pushSelectedIdxChanged ev)
        , HE.onValueInput \str -> Just (handleSearch tState searchDebouncer pushVisibilityChanged str)
        , HE.onMouseDown \_ -> Just (setVisibility tState pushVisibilityChanged On)
        , HE.onBlur \_ -> Just (setVisibility tState pushVisibilityChanged Off)
        , HP.tabIndex 0
        , HP.ref (H.RefLabel "select-input")
        ]

      getTargetIndex highlightedIndex itemCount = case _ of
        Index i -> i
        Prev -> case highlightedIndex of
          Just i | i /= 0 -> i - 1
          _ -> itemCount - 1
        Next -> case highlightedIndex of
          Just i | i /= (itemCount - 1) -> i + 1
          _ -> 0

      setFocus shouldFocus = do
        inputElement <- Hooks.getHTMLElementRef $ H.RefLabel "select-input"
        for_ inputElement \el -> H.liftEffect case shouldFocus of
          true -> HTMLElement.focus el
          _ -> HTMLElement.blur el

      setVisibility tState pushVisibilityChanged v = do
        st <- Hooks.get tState
        when (st.visibility /= v) do
          Hooks.put tState (st { visibility = v, highlightedIndex = Just 0 })
          pushVisibilityChanged v

      clearSearch tState = do
        Hooks.modify_ tState (_ { search = "" })

      handleSearch tState searchDebouncer pushVisibilityChanged str = do
        Hooks.modify_ tState (_ { search = str })
        void $ Hooks.fork $ setVisibility tState pushVisibilityChanged On
        searchDebouncer str

      highlight tState target = do
        st <- Hooks.get tState
        when (st.visibility == On) do
          itemCount <- inputRec.getItemCount
          let newIndex = Just (getTargetIndex st.highlightedIndex itemCount target)
          Hooks.put tState (st { highlightedIndex = newIndex })

      select tState pushSelectedIdxChanged target mbEv = do
        for_ mbEv preventMouseEvent
        st <- Hooks.get tState
        when (st.visibility == On) case target of
          Index ix -> pushSelectedIdxChanged ix
          Next -> do
            itemCount <- inputRec.getItemCount
            pushSelectedIdxChanged $ getTargetIndex st.highlightedIndex itemCount target
          Prev -> do
            itemCount <- inputRec.getItemCount
            pushSelectedIdxChanged $ getTargetIndex st.highlightedIndex itemCount target

      toggleClick tState pushVisibilityChanged ev = do
        preventMouseEvent ev
        st <- Hooks.get tState
        case st.visibility of
          On -> do
            setFocus false
            setVisibility tState pushVisibilityChanged Off
          Off -> do
            setFocus true
            setVisibility tState pushVisibilityChanged On

      key tState pushVisibilityChanged pushSelectedIdxChanged ev = do
        void $ Hooks.fork $ setVisibility tState pushVisibilityChanged On
        let preventIt = preventKeyEvent ev
        case KE.key ev of
          x | x == "ArrowUp" || x == "Up" ->
            preventIt *> highlight tState Prev
          x | x == "ArrowDown" || x == "Down" ->
            preventIt *> highlight tState Next
          x | x == "Escape" || x == "Esc" -> do
            inputElement <- Hooks.getHTMLElementRef $ H.RefLabel "select-input"
            preventIt
            for_ inputElement (H.liftEffect <<< HTMLElement.blur)
          "Enter" -> do
            st <- Hooks.get tState
            preventIt
            for_ st.highlightedIndex \ix ->
              select tState pushSelectedIdxChanged (Index ix) Nothing
          otherKey -> pure unit
