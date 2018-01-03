module Main where

import Prelude

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Calendar (component)

-- Type signature elided for convenience as effect types change
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
