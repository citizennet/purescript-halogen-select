module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Calendar (Effects, component)

main :: ∀ e. Eff (HA.HalogenEffects (Effects e)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
