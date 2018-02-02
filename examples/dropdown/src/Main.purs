module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Dropdown (component)
import Select.Effects (Effects)

main :: ∀ e. Eff (Effects e) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
