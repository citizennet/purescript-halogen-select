module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Example.Typeahead.Parent (MyEffects, component)

main :: ∀ e. Eff (MyEffects e) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
