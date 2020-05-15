let
  config = ./spago.dhall
in config // {
  sources = config.sources # [ "examples/**/*.purs" ],
  dependencies = config.dependencies # [ "affjax", "argonaut", "psci-support" ]
}
