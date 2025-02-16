{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.Text as Text

import Test.QuickCheck (quickCheck)

import qualified PlutusLedgerApi.V1 as PlutusV1
import PlutusLedgerApi.V1.Orphans ()
import qualified PlutusLedgerApi.V1.Value as PlutusV1
import qualified PlutusTx.Builtins as PlutusTx
import qualified PlutusTx.Builtins.HasBuiltin as PlutusTx
import qualified PlutusTx.IsData as PlutusTx
import qualified PlutusTx.Ratio as PlutusTx

import Ply.Core.Class
import Ply.Core.Schema

prop_toData :: forall a. (PlyArg a, ToDataConstraint a, PlutusTx.FromData a, Eq a) => a -> Bool
prop_toData x = PlutusTx.fromBuiltinData (PlutusTx.toBuiltinData $ toBuiltinArgData x) == Just x

type TestableBuiltin a = (PlyArg a, PlutusTx.HasToBuiltin a, PlutusTx.HasFromBuiltin (PlutusTx.ToBuiltin a), SchemaInstanceOf (UPLCSchema a) ~ PlutusTx.FromBuiltin (PlutusTx.ToBuiltin a), Eq (SchemaInstanceOf (UPLCSchema a)))

prop_toBuiltin :: forall a. TestableBuiltin a => a -> Bool
prop_toBuiltin x = toBuiltinArg x == PlutusTx.fromBuiltin (PlutusTx.toBuiltin x)

prop_toBuiltinPair :: forall a b. (TestableBuiltin a, TestableBuiltin b) => a -> b -> Bool
prop_toBuiltinPair a b = toBuiltinArg (a, b) == PlutusTx.fromBuiltin (PlutusTx.toBuiltin (a, b))

main :: IO ()
main = do
  quickCheck $ prop_toBuiltin @Integer
  quickCheck $ prop_toBuiltin @Bool
  quickCheck $ prop_toBuiltin @ByteString
  quickCheck $ \x -> prop_toBuiltin $ Text.pack x
  quickCheck $ prop_toBuiltin @()
  quickCheck $ prop_toBuiltinPair @Integer @Integer
  quickCheck $ prop_toBuiltinPair @Integer @Bool
  quickCheck $ \s -> prop_toBuiltinPair @_ @Bool (Text.pack s)
  quickCheck $ \s -> prop_toBuiltinPair @_ @Integer (Text.pack s)
  quickCheck $ \s -> prop_toBuiltinPair @_ @() (Text.pack s)
  quickCheck $ \a s -> prop_toBuiltinPair @() @_ a (Text.pack s)
  quickCheck $ \a s -> prop_toBuiltinPair @Integer @_ a (Text.pack s)
  quickCheck $ \a s -> prop_toBuiltinPair @Bool @_ a (Text.pack s)
  quickCheck $ \a s -> prop_toBuiltinPair @ByteString @_ a (Text.pack s)
  quickCheck $ \s' s -> prop_toBuiltinPair (Text.pack s') (Text.pack s)
  quickCheck $ prop_toBuiltinPair @ByteString @ByteString
  quickCheck $ prop_toBuiltinPair @ByteString @Integer
  quickCheck $ prop_toBuiltinPair @Integer @ByteString
  quickCheck $ prop_toBuiltinPair @Bool @ByteString
  quickCheck $ prop_toBuiltinPair @ByteString @Bool
  quickCheck $ prop_toData @PlutusV1.TxOutRef
  quickCheck $ prop_toData @PlutusV1.Credential
  quickCheck $ prop_toData @PlutusV1.Address
  quickCheck $ prop_toData @PlutusV1.StakingCredential
  quickCheck $ prop_toData @PlutusV1.CurrencySymbol
  quickCheck $ prop_toData @PlutusV1.TokenName
  quickCheck $ prop_toData @PlutusV1.AssetClass
  quickCheck $ prop_toData @PlutusV1.PubKeyHash
  quickCheck $ prop_toData @PlutusV1.ScriptHash
  quickCheck $ prop_toData @PlutusV1.DatumHash
  quickCheck $ prop_toData @PlutusV1.RedeemerHash
  quickCheck $ prop_toData @PlutusTx.Rational
  quickCheck $ prop_toData @PlutusV1.TxId
  quickCheck $ prop_toData @PlutusV1.DCert
  quickCheck $ prop_toData @PlutusV1.ScriptPurpose
