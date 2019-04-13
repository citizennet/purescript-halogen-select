-- | This module exposes helper functions necessary for the library to attach behaviors
-- | to your render functions. These allow you to write a render function for your
-- | `Select` UI and then augment it at relevant points with the properties defined
-- | below.
module Select.Setters where

import Prelude (append, ($), (<<<))

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select
import Web.Event.Event as E
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

-- | A helper function that augments an array of `IProps` with `ToggleProps`. It
-- | allows the toggle element to register key events for navigation or highlighting,
-- | record open and close events based on focus and blur, and to be focused with
-- | the tab key.
-- |
-- | ```purescript
-- | renderToggle = div (setToggleProps [ class "btn-class" ]) [ ...html ]
-- | ```
setToggleProps
  :: forall props st act
   . State st
  -> Array (HP.IProp (ToggleProps props) (Action act))
  -> Array (HP.IProp (ToggleProps props) (Action act))
setToggleProps st = append
  [ HE.onFocus \_ -> Just $ SetVisibility On
  , HE.onMouseDown $ Just <<< ToggleClick
  , HE.onKeyDown $ Just <<< Key
  , HE.onBlur \_ -> Just $ SetVisibility Off
  , HP.tabIndex 0
  , HP.ref (H.RefLabel "select-input")
  ]

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

-- | A helper function that augments an array of `IProps` with `InputProps`. It
-- | allows the input element to capture string values, register key events for
-- | navigation, record open and close events based on focus and blur, and to be
-- | focused with the tab key.
-- |
-- | ```purescript
-- | renderInput = input_ (setInputProps [ class "my-class" ])
-- | ```
setInputProps
  :: forall props act
   . Array (HP.IProp (InputProps props) (Action act))
  -> Array (HP.IProp (InputProps props) (Action act))
setInputProps = append
  [ HE.onFocus \_ -> Just $ SetVisibility On
  , HE.onKeyDown $ Just <<< Key
  , HE.onValueInput $ Just <<< Search
  , HE.onMouseDown \_ -> Just $ SetVisibility On
  , HE.onBlur \_ -> Just $ SetVisibility Off
  , HP.tabIndex 0
  , HP.ref (H.RefLabel "select-input")
  ]

-- | The properties that must be supported by the HTML element that acts as a
-- | selectable "item" in your UI. This should be attached to every item that
-- | can be selected.
type ItemProps props =
  ( onMouseDown :: ME.MouseEvent
  , onMouseOver :: ME.MouseEvent
  | props
  )

-- | A helper function that augments an array of `IProps` with `ItemProps`. It
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
setItemProps
  :: forall props act
   . Int 
  -> Array (HP.IProp (ItemProps props) (Action act)) 
  -> Array (HP.IProp (ItemProps props) (Action act))
setItemProps index = append
  [ HE.onMouseDown \ev -> Just (Select (Index index) (Just ev))
  , HE.onMouseOver \_ -> Just $ Highlight (Index index)
  ]

-- | A helper function that augments an array of `IProps` with a `MouseDown`
-- | handler. It prevents clicking on an item within an enclosing HTML element
-- | from bubbling up a blur event to the DOM. This should be used on the parent
-- | element that contains your items.
setContainerProps
  :: forall props act
   . Array (HP.IProp (onMouseDown :: ME.MouseEvent | props) (Action act))
  -> Array (HP.IProp (onMouseDown :: ME.MouseEvent | props) (Action act))
setContainerProps = append
  [ HE.onMouseDown $ Just <<< PreventClick ]

