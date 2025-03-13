{-# LANGUAGE AllowAmbiguousTypes #-}

module Property.PlyArg (test) where

import qualified PlutusCore as PLC
import qualified PlutusLedgerApi.V1 as PlutusV1
import PlutusLedgerApi.V1.Orphans ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Ply.Core.Class (PlyArg (toSomeBuiltinArg))

prop :: forall a. PlyArg a => a -> Bool
prop x = toSomeBuiltinArg x == PLC.someValue (PlutusV1.toData x)

test :: TestTree
test =
  testGroup
    "plyarg"
    [ testProperty "tokenname" $ prop @PlutusV1.TokenName
    ]
