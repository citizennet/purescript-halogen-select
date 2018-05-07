-- | This module exposes helper functions necessary for the library to attach behaviors
-- | to your render functions. These allow you to write a render function for your
-- | `Select` UI and then augment it at relevant points with the properties defined
-- | below.
module Select.Utils.Setters where

import Prelude

import DOM.Event.FocusEvent as FE
import DOM.Event.MouseEvent as ME
import DOM.Event.Types as ET
import Data.Maybe (Maybe(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select (Query, Target(..), Visibility(..))
import Select as Select

-- | The properties that must be supported by the HTML element that serves
-- | as a menu toggle. This should be used with toggle-driven `Select` components.
type ToggleProps p =
  ( onFocus :: ET.FocusEvent
  , onKeyDown :: ET.KeyboardEvent
  , onMouseDown :: ET.MouseEvent
  , onClick :: ET.MouseEvent
  , onBlur :: ET.FocusEvent
  , tabIndex :: Int
  | p
  )

-- | A helper function that augments an array of `IProps` with `ToggleProps`. It
-- | ensures the toggle element can register key events for navigation or highlighting,
-- | record open and close events based on focus and blur, and can be focused with the
-- | the tab key.
-- |
-- | ```purescript
-- | renderToggle = div (setToggleProps [ class "btn-class" ]) [ ...html ]
-- | ```
setToggleProps
  :: ∀ o item eff p
   . Array (HP.IProp (ToggleProps p) (Query o item eff Unit))
  -> Array (HP.IProp (ToggleProps p) (Query o item eff Unit))
setToggleProps = flip (<>)
  [ HE.onFocus \ev -> Just do
      Select.captureRef $ FE.focusEventToEvent ev
      Select.setVisibility On
  , HE.onMouseDown \ev -> Just do
      Select.captureRef $ ME.mouseEventToEvent ev
      Select.preventClick ev
      Select.getVisibility >>= case _ of
        Select.On -> do
          Select.triggerBlur
          Select.setVisibility Select.Off
        Select.Off -> do
          Select.triggerFocus
          Select.setVisibility Select.On
  , HE.onKeyDown $ Just <<< Select.key
  , HE.onBlur $ Select.always $ Select.setVisibility Off
  , HP.tabIndex 0
  ]

-- | The properties that must be supported by the HTML element that serves
-- | as a text input. This should be used with input-driven `Select` components.
type InputProps p =
  ( onFocus :: ET.FocusEvent
  , onKeyDown :: ET.KeyboardEvent
  , onInput :: ET.Event
  , value :: String
  , onMouseDown :: ET.MouseEvent
  , onBlur :: ET.FocusEvent
  , tabIndex :: Int
  | p
  )

-- | A helper function that augments an array of `IProps` with `InputProps`. It
-- | ensures the input element can capture string values, register key events for
-- | navigation, record open and close events based on focus and blur, and can be
-- | focused with the tab key.
-- |
-- | ```purescript
-- | renderInput = input_ (setInputProps [ class "my-class" ])
-- | ```
setInputProps
  :: ∀ o item eff p
   . Array (HP.IProp (InputProps p) (Query o item eff Unit))
  -> Array (HP.IProp (InputProps p) (Query o item eff Unit))
setInputProps = flip (<>)
  [ HE.onFocus \ev -> Just do
      Select.captureRef $ FE.focusEventToEvent ev
      Select.setVisibility On
  , HE.onKeyDown $ Just <<< Select.key
  , HE.onValueInput $ Just <<< Select.search
  , HE.onMouseDown $ Select.always $ Select.setVisibility On
  , HE.onBlur $ Select.always $ Select.setVisibility Off
  , HP.tabIndex 0
  ]

-- | The properties that must be supported by the HTML element that acts as a
-- | selectable "item" in your UI. This should be attached to every item that
-- | can be selected.
type ItemProps p =
  ( onMouseDown :: ET.MouseEvent
  , onMouseOver :: ET.MouseEvent
  | p
  )

-- | A helper function that augments an array of `IProps` with `ItemProps`. It
-- | ensures items can be highlighted and selected.
-- |
-- | This expects an index for use in highlighting. It's useful in combination
-- | with `mapWithIndex`:
-- |
-- | ```purescript
-- | renderItem index itemHTML = HH.li (setItemProps index [ class "my-class" ]) [ itemHTML ]
-- |
-- | render = renderItem `mapWithIndex` itemsArray
-- | ```
setItemProps
  :: ∀ o item eff p
   . Int
  -> Array (HP.IProp (ItemProps p) (Query o item eff Unit))
  -> Array (HP.IProp (ItemProps p) (Query o item eff Unit))
setItemProps index = flip (<>)
  [ HE.onMouseDown \ev -> Just do
      Select.preventClick ev
      Select.select index
  , HE.onMouseOver $ Select.always $ Select.highlight (Index index)
  ]

-- | A helper function that augments an array of `IProps` with a `MouseDown`
-- | handler. It ensures that clicking on an item within an enclosing HTML element
-- | will not bubble up a blur event to the DOM. This should be used on the parent
-- | element that contains your items.
setContainerProps
  :: ∀ o item eff p
   . Array (HP.IProp (onMouseDown :: ET.MouseEvent | p) (Query o item eff Unit))
  -> Array (HP.IProp (onMouseDown :: ET.MouseEvent | p) (Query o item eff Unit))
setContainerProps = flip (<>)
  [ HE.onMouseDown $ Just <<< Select.preventClick ]
