module SelectHook where

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
import Halogen.Hooks.Extra.Hooks.UseEvent (EventProps, UseEvent, useEvent)
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

type SelectInput slots output m =
  { inputType :: InputType
  , search :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , getItemCount :: HookM slots output m Int
  }

type SelectReturn slots output m
                  toggleProps itemProps containerProps inputProps =
  { search :: String
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int

  , setFocus :: Boolean -> HookM slots output m Unit
  , setVisibility :: Visibility -> HookM slots output m Unit
  , clearSearch :: HookM slots output m Unit

  , onNewSearch :: EventProps slots output m String
  , onVisibilityChanged :: EventProps slots output m Visibility
  , onSelectedIdxChanged :: EventProps slots output m Int

  , toggleProps :: Array (HP.IProp (ToggleProps toggleProps) (HookM slots output m Unit))
  , itemProps :: Int -> Array (HP.IProp (ItemProps itemProps) (HookM slots output m Unit))
  , containerProps :: Array (HP.IProp (onMouseDown :: ME.MouseEvent | containerProps) (HookM slots output m Unit))
  , inputProps :: Array (HP.IProp (InputProps inputProps) (HookM slots output m Unit))
  }

newtype UseSelect hooks =
  UseSelect (UseDebouncer String (UseEvent Int (UseEvent Visibility (UseEvent String
    (UseState (Maybe Int) (UseState Visibility (UseState String hooks)))))))

derive instance newtypeUseSelect :: Newtype (UseSelect hooks) _

useSelect
  :: forall slots output m toggleProps itemProps containerProps inputProps
   . MonadAff m
  => SelectInput slots output m
  -> Hook slots output m UseSelect
        (SelectReturn slots output m toggleProps itemProps containerProps inputProps)
useSelect inputRec =
  let
    initialSearchValue = fromMaybe "" inputRec.search
    debounceTime = fromMaybe mempty inputRec.debounceTime
  in Hooks.wrap Hooks.do
    search /\ tSearch <- useState (fromMaybe "" inputRec.search)
    visibility /\ tVisibility <- useState Off
    highlightedIndex /\ tHighlightedIndex <- useState Nothing

    onNewSearch <- useEvent
    onVisibilityChanged <- useEvent
    onSelectedIdxChanged <- useEvent

    searchDebouncer <- useDebouncer debounceTime \lastSearchState -> Hooks.do
      case inputRec.inputType of
        Text -> do
          Hooks.put tHighlightedIndex (Just 0)
          onNewSearch.push lastSearchState

        -- Key stream is not yet implemented. However, this should capture user
        -- key events and expire their search after a set number of milliseconds.
        _ -> pure unit

    Hooks.pure
      -- state
      { search
      , visibility
      , highlightedIndex

      -- actions
      , setFocus
      , setVisibility: setVisibility tVisibility tHighlightedIndex onVisibilityChanged
      , clearSearch: clearSearch tSearch

      -- events
      , onNewSearch: onNewSearch.props
      , onVisibilityChanged: onVisibilityChanged.props
      , onSelectedIdxChanged: onSelectedIdxChanged.props

      -- props
      , toggleProps: toggleProps tVisibility tHighlightedIndex onVisibilityChanged onSelectedIdxChanged
      , itemProps: itemProps tVisibility tHighlightedIndex onSelectedIdxChanged
      , containerProps
      , inputProps: inputProps tVisibility tHighlightedIndex tSearch searchDebouncer onVisibilityChanged onSelectedIdxChanged
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
      toggleProps :: _ -> _ -> _ -> _ -> Array (HP.IProp (ToggleProps toggleProps) (HookM slots output m Unit))
      toggleProps tVisibility tHighlightedIndex onVisibilityChanged onSelectedIdxChanged =
        [ HE.onFocus \_ -> Just (setVisibility tVisibility tHighlightedIndex onVisibilityChanged On)
        , HE.onMouseDown \ev -> Just (toggleClick tVisibility tHighlightedIndex onVisibilityChanged ev)
        , HE.onKeyDown \ev -> Just (key tVisibility tHighlightedIndex onVisibilityChanged onSelectedIdxChanged ev)
        , HE.onBlur \ev -> Just (setVisibility tVisibility tHighlightedIndex onVisibilityChanged Off)
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
      itemProps :: _ -> _ -> _ -> Int -> Array (HP.IProp (ItemProps itemProps) (HookM slots output m Unit))
      itemProps tVisibility tHighlightedIndex onSelectedIdxChanged index =
        [ HE.onMouseDown \ev -> Just (select tVisibility tHighlightedIndex onSelectedIdxChanged (Index index) (Just ev))
        , HE.onMouseOver \_ -> Just (highlight tVisibility tHighlightedIndex (Index index))
        ]

      -- | An array of `IProps` with a `MouseDown`
      -- | handler. It prevents clicking on an item within an enclosing HTML element
      -- | from bubbling up a blur event to the DOM. This should be used on the parent
      -- | element that contains your items.
      containerProps :: Array (HP.IProp (onMouseDown :: ME.MouseEvent | containerProps) (HookM slots output m Unit))
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
      inputProps :: _ -> _ -> _ -> _ -> _ -> _ -> Array (HP.IProp (InputProps inputProps) (HookM slots output m Unit))
      inputProps tVisibility tHighlightedIndex tSearch searchDebouncer onVisibilityChanged onSelectedIdxChanged =
        [ HE.onFocus \_ -> Just (setVisibility tVisibility tHighlightedIndex onVisibilityChanged On)
        , HE.onKeyDown \ev -> Just (key tVisibility tHighlightedIndex onVisibilityChanged onSelectedIdxChanged ev)
        , HE.onValueInput \str -> Just (handleSearch tSearch tVisibility tHighlightedIndex searchDebouncer onVisibilityChanged str)
        , HE.onMouseDown \_ -> Just (setVisibility tVisibility tHighlightedIndex onVisibilityChanged On)
        , HE.onBlur \_ -> Just (setVisibility tVisibility tHighlightedIndex onVisibilityChanged Off)
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

      setVisibility tVisibility tHighlightedIndex onVisibilityChanged v = do
        visibility <- Hooks.get tVisibility
        when (visibility /= v) do
          Hooks.put tVisibility v
          Hooks.put tHighlightedIndex (Just 0)
          onVisibilityChanged.push v

      clearSearch tSearch = do
        Hooks.put tSearch ""

      handleSearch tSearch tVisibility tHighlightedIndex searchDebouncer onVisibilityChanged str = do
        Hooks.put tSearch str
        void $ Hooks.fork $ setVisibility tVisibility tHighlightedIndex onVisibilityChanged On
        searchDebouncer str

      highlight tVisibility tHighlightedIndex target = do
        visibility <- Hooks.get tVisibility
        when (visibility == On) do
          itemCount <- inputRec.getItemCount
          Hooks.modify_ tHighlightedIndex \highlightedIndex -> Just (getTargetIndex highlightedIndex itemCount target)

      select tVisibility tHighlightedIndex onSelectedIdxChanged target mbEv = do
        for_ mbEv preventMouseEvent
        visibility <- Hooks.get tVisibility
        when (visibility == On) case target of
          Index ix -> onSelectedIdxChanged.push ix
          Next -> do
            itemCount <- inputRec.getItemCount
            highlightedIndex <- Hooks.get tHighlightedIndex
            onSelectedIdxChanged.push $ getTargetIndex highlightedIndex itemCount target
          Prev -> do
            itemCount <- inputRec.getItemCount
            highlightedIndex <- Hooks.get tHighlightedIndex
            onSelectedIdxChanged.push $ getTargetIndex highlightedIndex itemCount target

      toggleClick tVisibility tHighlightedIndex onVisibilityChanged ev = do
        preventMouseEvent ev
        visibility <- Hooks.get tVisibility
        case visibility of
          On -> do
            setFocus false
            setVisibility tVisibility tHighlightedIndex onVisibilityChanged Off
          Off -> do
            setFocus true
            setVisibility tVisibility tHighlightedIndex onVisibilityChanged On

      key tVisibility tHighlightedIndex onVisibilityChanged onSelectedIdxChanged ev = do
        void $ Hooks.fork $ setVisibility tVisibility tHighlightedIndex onVisibilityChanged On
        let preventIt = preventKeyEvent ev
        case KE.key ev of
          x | x == "ArrowUp" || x == "Up" ->
            preventIt *> highlight tVisibility tHighlightedIndex Prev
          x | x == "ArrowDown" || x == "Down" ->
            preventIt *> highlight tVisibility tHighlightedIndex Next
          x | x == "Escape" || x == "Esc" -> do
            inputElement <- Hooks.getHTMLElementRef $ H.RefLabel "select-input"
            preventIt
            for_ inputElement (H.liftEffect <<< HTMLElement.blur)
          "Enter" -> do
            highlightedIndex <- Hooks.get tHighlightedIndex
            preventIt
            for_ highlightedIndex \ix ->
              select tVisibility tHighlightedIndex onSelectedIdxChanged (Index ix) Nothing
          otherKey -> pure unit
