{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "catenable-lists"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exists"
  , "foldable-traversable"
  , "foreign"
  , "free"
  , "identity"
  , "maybe"
  , "monad-control"
  , "newtype"
  , "parallel"
  , "partial"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "spec"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "undefined"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
