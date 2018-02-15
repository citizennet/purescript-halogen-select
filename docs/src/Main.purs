module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Example.Content.Home as Home

main :: ∀ eff. Eff (HA.HalogenEffects (Home.Effects eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Home.component unit body
