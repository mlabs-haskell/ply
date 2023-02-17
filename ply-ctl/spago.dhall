{ name = "ply-ctl"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "cardano-transaction-lib"
  , "bigints"
  , "aeson"
  , "either"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "Main.purs" ]
}
