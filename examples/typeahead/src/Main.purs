module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Select.Effects (Effects)
import Typeahead (component)

main :: Eff _ Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
