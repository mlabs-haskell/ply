{ name = "ply-ctl"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "cardano-transaction-lib"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
