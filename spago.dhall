{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-select"
, license = "Apache-2.0"
, repository = "https://github.com/citizennet/purescript-halogen-select"
, dependencies =
  [ "halogen"
  , "halogen-hooks"
  , "halogen-hooks-extra"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
