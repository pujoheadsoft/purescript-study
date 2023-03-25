{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "exists"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "refs"
  , "run"
  , "safe-coerce"
  , "spec"
  , "spec-discovery"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
