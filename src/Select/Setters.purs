-- | This module exposes helper functions necessary for the library to attach behaviors
-- | to your render functions. These allow you to write a render function for your
-- | `Select` UI and then augment it at relevant points with the properties defined
-- | below.
module Select.Setters where

import Prelude

import Halogen (RefLabel(..), action) as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select (Query(..), Target(..), Visibility(..))
import Select as Select
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent as FE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-- | The properties that must be supported by the HTML element that serves
-- | as a menu toggle. This should be used with toggle-driven `Select` components.
type ToggleProps p =
  ( onFocus :: FE.FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onMouseDown :: ME.MouseEvent
  , onClick :: ME.MouseEvent
  , onBlur :: FE.FocusEvent
  , tabIndex :: Int
  | p
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
  :: forall o item p
   . Select.State item
  -> Array (HP.IProp (ToggleProps p) (Query o item Unit))
  -> Array (HP.IProp (ToggleProps p) (Query o item Unit))
setToggleProps st = append
  [ HE.onFocus $ HE.input_ $ SetVisibility On 
  , HE.onMouseDown $ HE.input \ev ->
      H.action (PreventClick ev) `AndThen` H.action (mouseDownAction unit)
  , HE.onKeyDown $ HE.input Key
  , HE.onBlur $ HE.input_ $ SetVisibility Off
  , HP.tabIndex 0
  , HP.ref (H.RefLabel "select-input")
  ]
  where
  mouseDownAction = \_ -> case st.visibility of
    On -> 
      H.action (Focus false) `AndThen` H.action (SetVisibility Off)
    Off ->
      H.action (Focus true) `AndThen` H.action (SetVisibility On)

-- | The properties that must be supported by the HTML element that serves
-- | as a text input. This should be used with input-driven `Select` components.
type InputProps p =
  ( onFocus :: FE.FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onInput :: Event
  , value :: String
  , onMouseDown :: ME.MouseEvent
  , onBlur :: FE.FocusEvent
  , tabIndex :: Int
  | p
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
  :: ∀ o item p
   . Array (HP.IProp (InputProps p) (Query o item Unit))
  -> Array (HP.IProp (InputProps p) (Query o item Unit))
setInputProps = append
  [ HE.onFocus $ HE.input_ $ SetVisibility On
  , HE.onKeyDown $ HE.input $ Key
  , HE.onValueInput $ HE.input $ Search
  , HE.onMouseDown $ HE.input_ $ SetVisibility On
  , HE.onBlur $ HE.input_ $ Select.SetVisibility Off
  , HP.tabIndex 0
  , HP.ref (H.RefLabel "select-input")
  ]

-- | The properties that must be supported by the HTML element that acts as a
-- | selectable "item" in your UI. This should be attached to every item that
-- | can be selected.
type ItemProps p =
  ( onMouseDown :: ME.MouseEvent
  , onMouseOver :: ME.MouseEvent
  | p
  )

-- | A helper function that augments an array of `IProps` with `ItemProps`. It
-- | allows items to be highlighted and selected.
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
  :: ∀ o item p
   . Int
  -> Array (HP.IProp (ItemProps p) (Query o item Unit))
  -> Array (HP.IProp (ItemProps p) (Query o item Unit))
setItemProps index = append
  [ HE.onMouseDown $ HE.input \ev -> do
      H.action (PreventClick ev) `AndThen` H.action (Select index)
  , HE.onMouseOver $ HE.input_ $ Highlight (Index index)
  ]

-- | A helper function that augments an array of `IProps` with a `MouseDown`
-- | handler. It prevents clicking on an item within an enclosing HTML element
-- | from bubbling up a blur event to the DOM. This should be used on the parent
-- | element that contains your items.
setContainerProps
  :: ∀ o item p
   . Array (HP.IProp (onMouseDown :: ME.MouseEvent | p) (Query o item Unit))
  -> Array (HP.IProp (onMouseDown :: ME.MouseEvent | p) (Query o item Unit))
setContainerProps = append
  [ HE.onMouseDown $ HE.input PreventClick ]
