module Select.Dropdown where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Query a = NoOp a
type State = Unit
type Input = Unit
data Message = N

component :: H.Component HH.HTML Query Input Message _
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render _ = HH.div_ []

    eval :: Query ~> H.ComponentDSL State Query Message _
    eval = case _ of
      NoOp a -> pure a
