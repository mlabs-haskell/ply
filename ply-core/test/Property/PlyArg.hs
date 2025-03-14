{-# LANGUAGE AllowAmbiguousTypes #-}

module Property.PlyArg (test) where

import qualified PlutusCore as PLC
import qualified PlutusLedgerApi.V3 as PlutusV3
import PlutusLedgerApi.V3.Orphans ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import qualified PlutusTx.Ratio as PlutusTx
import Ply.Core.Class (PlyArg (toSomeBuiltinArg))

-- NOTE: This property doesn't apply to all PlyArg instances, even _if_ they have ToData.
-- Notably, AssetClass actually should encode to a builtin pair. No clue why Plutus decided to do it like this, but they did.
prop :: forall a. (PlutusV3.ToData a, PlyArg a) => a -> Bool
prop x = toSomeBuiltinArg x == PLC.someValue (PlutusV3.toData x)

test :: TestTree
test =
  testGroup
    "plyarg"
    [ testProperty "int" $ prop @Integer
    , testProperty "builtin-bytestring" $ prop @PlutusV3.BuiltinByteString
    , testProperty "rational" $ prop @PlutusTx.Rational
    , testProperty "value" $ prop @PlutusV3.Value
    , testProperty "credential" $ prop @PlutusV3.Credential
    , testProperty "staking-credential" $ prop @PlutusV3.StakingCredential
    , testProperty "address" $ prop @PlutusV3.Address
    , testProperty "currency-symbol" $ prop @PlutusV3.CurrencySymbol
    , testProperty "token-name" $ prop @PlutusV3.TokenName
    , testProperty "asset-class" $ \x@(AssetClass (cs, tk)) -> toSomeBuiltinArg x == PLC.someValue (PlutusV3.toData cs, PlutusV3.toData tk)
    , testProperty "pubkeyhash" $ prop @PlutusV3.PubKeyHash
    , testProperty "posixtime" $ prop @PlutusV3.POSIXTime
    , testProperty "interval" $ prop @(PlutusV3.Interval PlutusV3.POSIXTime)
    , testProperty "txid" $ prop @PlutusV3.TxId
    , testProperty "txoutref" $ prop @PlutusV3.TxOutRef
    , testProperty "script-hash" $ prop @PlutusV3.ScriptHash
    , testProperty "datum-hash" $ prop @PlutusV3.DatumHash
    , testProperty "redeemer-hash" $ prop @PlutusV3.RedeemerHash
    ]
