{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "refs"
  , "safe-coerce"
  , "spec"
  , "spec-discovery"
  , "transformers"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
