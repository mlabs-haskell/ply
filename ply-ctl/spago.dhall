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
  , "node-process"
  , "integers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "application-test/*.purs"]
}
