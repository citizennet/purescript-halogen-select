module Select.Props where

import Prelude

import Select.InputContainer (MouseState(..), Query(..), Target(Index))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

setChildProps = flip (<>)
  [ HE.onBlur      $ HE.input_ $ Blur
  , HP.tabIndex 0
  ]

setInputProps = flip (<>)
  [ HE.onFocus      $ HE.input  CaptureFocus
  , HE.onKeyDown    $ HE.input  Key
  , HE.onValueInput $ HE.input  TextInput
  , HE.onBlur       $ HE.input_ Blur
  , HP.tabIndex 0
  ]

setContainerProps = flip (<>)
  [ HE.onMouseDown $ HE.input_ $ Mouse Down
  , HE.onMouseUp   $ HE.input_ $ Mouse Up
  ]

setItemProps index = flip (<>)
  [ HE.onMouseDown $ HE.input_ $ Select index
  , HE.onMouseOver $ HE.input_ $ Highlight (Index index)
  , HE.onKeyDown   $ HE.input  $ Key
  , HE.onBlur      $ HE.input_ $ Blur
  ]
