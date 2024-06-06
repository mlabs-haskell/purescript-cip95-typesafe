{ name = "cip95-typesafe"
, dependencies =
  [ "cip95"
  , "console"
  , "effect"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
