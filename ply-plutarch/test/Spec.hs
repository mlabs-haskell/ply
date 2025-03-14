{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.ByteString (ByteString)

import Plutarch.Internal.Term (PType, punsafeConstantInternal)
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Unsorted), PMap)
import qualified Plutarch.LedgerApi.V3 as PLedger.V3
import Plutarch.LedgerApi.Value (AmountGuarantees (NoGuarantees), PValue)
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (propEvalEqual)
import PlutusLedgerApi.Common (toBuiltin)
import PlutusLedgerApi.V3.Orphans ()
import Ply (PlyArg (toSomeBuiltinArg))
import Ply.Plutarch.Class (PlyArgOf)
import Test.QuickCheck (Arbitrary)
import Test.Tasty

prop :: forall (a :: PType). (Arbitrary (PlyArgOf a), Show (PlyArgOf a), PlyArg (PlyArgOf a), PLiftable a, AsHaskell a ~ PlyArgOf a) => TestName -> TestTree
prop name = propEvalEqual @(PlyArgOf a) name (pconstant @a) (punsafeConstantInternal . toSomeBuiltinArg)

tests :: TestTree
tests =
  testGroup
    "types correspondence"
    [ prop @(PAsData PInteger) "integer"
    , propEvalEqual @ByteString "bytestring" (pdata . pconstant @PByteString) (punsafeConstantInternal . toSomeBuiltinArg . toBuiltin)
    , prop @PLedger.V3.PRationalData "rational"
    , prop @(PAsData (PValue Unsorted NoGuarantees)) "value"
    , prop @(PAsData (PBuiltinList PLedger.V3.PTxOutRef)) "list"
    , prop @(PLedger.V3.PMaybeData PLedger.V3.PRationalData) "maybe"
    , prop @(PAsData (PMap Unsorted (PAsData PLedger.V3.PDatumHash) (PAsData PLedger.V3.PTokenName))) "map"
    , prop @PLedger.V3.PCredential "credential"
    , prop @PLedger.V3.PStakingCredential "staking-credential"
    , prop @PLedger.V3.PAddress "address"
    , prop @(PAsData PLedger.V3.PCurrencySymbol) "currency-symbol"
    , prop @(PAsData PLedger.V3.PTokenName) "token-name"
    , prop @(PAsData PLedger.V3.PPubKeyHash) "pubkeyhash"
    , prop @(PAsData PLedger.V3.PPosixTime) "posixtime"
    , prop @(PLedger.V3.PInterval (PAsData PLedger.V3.PPosixTime)) "interval"
    , prop @(PAsData PLedger.V3.PTxId) "txid"
    , prop @PLedger.V3.PTxOutRef "txoutref"
    , prop @(PAsData PLedger.V3.PScriptHash) "scripthash"
    , prop @(PAsData PLedger.V3.PDatumHash) "scripthash"
    , prop @(PAsData PLedger.V3.PRedeemerHash) "scripthash"
    ]

main :: IO ()
main = defaultMain tests
