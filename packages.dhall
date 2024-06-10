let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221013/packages.dhall
        sha256:21000b190e1ef14c92feb1400816022319bc40a30280d20f24c0dcacfb85e966

let additions =
      { cip95 =
        { dependencies =
          [ "aff", "aff-promise", "console", "effect", "prelude" ]
        , repo = "https://github.com/mlabs-haskell/purescript-cip95"
        , version = "db81941898a34fecee3064bf499ad63d3402f97d"
        }
      , cip30-typesafe =
        { dependencies =
          [ "aff"
          , "bifunctors"
          , "cip30"
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
        , repo = "https://github.com/mlabs-haskell/purescript-cip30-typesafe"
        , version = "83d4209413f792a12f6fb8a199320b2f0b2a8e36"
        }
      , cip30 =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "arrays"
          , "console"
          , "effect"
          , "literals"
          , "maybe"
          , "newtype"
          , "nullable"
          , "prelude"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cip30"
        , version = "8f1b34b48825fcec5e9c67f33e255770b1e0bc45"
        }
      }

in  upstream // additions
