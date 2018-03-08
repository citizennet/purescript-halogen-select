module Select.Utils.Setters where

import Prelude

import Select (Query(..), Target(..), Visibility(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

setToggleProps = flip (<>)
  [ HE.onFocus    $ HE.input CaptureFocus
  , HE.onKeyDown  $ HE.input Key
  , HE.onBlur $ HE.input_ $ SetVisibility Off
  , HP.tabIndex 0
  ]

setInputProps = flip (<>)
  [ HE.onFocus      $ HE.input CaptureFocus
  , HE.onKeyDown    $ HE.input Key
  , HE.onValueInput $ HE.input Search
  , HE.onMouseDown  $ HE.input_ $ SetVisibility On
  , HE.onBlur       $ HE.input_ $ SetVisibility Off
  , HP.tabIndex 0
  ]

setContainerProps = flip (<>)
  [ HE.onMouseDown $ HE.input PreventClick ]

setItemProps index = flip (<>)
  [ HE.onMouseDown $ HE.input  $ ItemClick index
  , HE.onMouseOver $ HE.input_ $ Highlight (Index index)
  ]
