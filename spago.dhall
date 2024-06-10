{ name = "cip95-typesafe"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "cip30"
  , "cip30-typesafe"
  , "cip95"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "maybe"
  , "prelude"
  , "spec"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
