let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221013/packages.dhall
        sha256:21000b190e1ef14c92feb1400816022319bc40a30280d20f24c0dcacfb85e966

let additions =
      { cip95 =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "console"
          , "effect"
          , "prelude"
          ] 
        , repo = "https://github.com/mlabs-haskell/purescript-cip95"
        , version = "db81941898a34fecee3064bf499ad63d3402f97d"
        }
      }

in upstream // additions
