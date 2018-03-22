module Select.Utils.Setters where

import Prelude

import DOM.Event.FocusEvent as FE
import DOM.Event.MouseEvent as ME
import DOM.Event.Types as ET
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select (Query(..), Target(..), Visibility(..), andThen)

type ToggleProps p =
  ( onFocus :: ET.FocusEvent
  , onKeyDown :: ET.KeyboardEvent
  , onMouseDown :: ET.MouseEvent
  , onClick :: ET.MouseEvent
  , onBlur :: ET.FocusEvent
  , tabIndex :: Int
  | p
  )

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

type ItemProps p =
  ( onMouseDown :: ET.MouseEvent
  , onMouseOver :: ET.MouseEvent
  | p
  )

setToggleProps
  :: ∀ o item eff p
   . Array (HP.IProp (ToggleProps p) (Query o item eff Unit))
  -> Array (HP.IProp (ToggleProps p) (Query o item eff Unit))
setToggleProps = flip (<>)
  [ HE.onFocus $ HE.input $ \ev a ->
      (H.action $ CaptureFocus $ FE.focusEventToEvent ev)
      `andThen`
      SetVisibility On a
  , HE.onMouseDown $ HE.input $ \ev a ->
      (H.action $ CaptureFocus $ ME.mouseEventToEvent ev)
      `andThen`
      (H.action $ PreventClick ev)
      `andThen`
      (H.action TriggerFocus)
      `andThen`
      ToggleVisibility a
  , HE.onKeyDown $ HE.input Key
  , HE.onBlur $ HE.input_ $ SetVisibility Off
  , HP.tabIndex 0
  ]

setInputProps
  :: ∀ o item eff p
   . Array (HP.IProp (InputProps p) (Query o item eff Unit))
  -> Array (HP.IProp (InputProps p) (Query o item eff Unit))
setInputProps = flip (<>)
  [ HE.onFocus $ HE.input $ \ev a ->
      (H.action $ CaptureFocus $ FE.focusEventToEvent ev)
      `andThen`
      SetVisibility On a
  , HE.onKeyDown $ HE.input Key
  , HE.onValueInput $ HE.input Search
  , HE.onMouseDown $ HE.input_ $ SetVisibility On
  , HE.onBlur $ HE.input_ $ SetVisibility Off
  , HP.tabIndex 0
  ]

setContainerProps
  :: ∀ o item eff p
   . Array (HP.IProp (onMouseDown :: ET.MouseEvent | p) (Query o item eff Unit))
  -> Array (HP.IProp (onMouseDown :: ET.MouseEvent | p) (Query o item eff Unit))
setContainerProps = flip (<>)
  [ HE.onMouseDown $ HE.input PreventClick ]

setItemProps
  :: ∀ o item eff p
   . Int
  -> Array (HP.IProp (ItemProps p) (Query o item eff Unit))
  -> Array (HP.IProp (ItemProps p) (Query o item eff Unit))
setItemProps index = flip (<>)
  [ HE.onMouseDown $ HE.input  $ \ev a ->
      (H.action $ PreventClick ev)
      `andThen`
      Select index a
  , HE.onMouseOver $ HE.input_ $ Highlight (Index index)
  ]
