module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Example.Components.Dropdown as Dropdown

main :: ∀ eff. Eff (HA.HalogenEffects (Dropdown.Effects eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Dropdown.component unit body
