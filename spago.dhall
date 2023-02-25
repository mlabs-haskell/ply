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
, packages = ./ply-ctl/packages.dhall
, sources = [ "ply-ctl/src/**/*.purs", "ply-ctl/application-test/*.purs"]
}
