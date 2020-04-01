{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-select"
, dependencies =
  [ "argonaut"
  , "console"
  , "debug"
  , "effect"
  , "halogen"
  , "psci-support"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
