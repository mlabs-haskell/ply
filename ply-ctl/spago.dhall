{ name = "ply-ctl"
, dependencies =
  [ "effect"
  , "prelude"
  , "cardano-transaction-lib"
  , "bigints"
  , "aeson"
  , "either"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "tuples"
  , "arrays"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "Main.purs" ]
}
