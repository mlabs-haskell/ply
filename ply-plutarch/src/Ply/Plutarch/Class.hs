{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.Class (PlyArgOf) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

import Plutarch.Internal.Term (PType)
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Unsorted), PMap)
import Plutarch.LedgerApi.Utils
import qualified Plutarch.LedgerApi.V3 as PLedger.V3
import Plutarch.LedgerApi.Value (AmountGuarantees (NoGuarantees), PValue)
import Plutarch.Prelude
import PlutusLedgerApi.V3 as Ledger.V3
import qualified PlutusTx.AssocMap as PlutusMap
import qualified PlutusTx.Ratio as PlutusTx

{- | 'PlyArgOf' yields the corresponding Haskell type with 'HasBlueprintSchema' instance for a given Plutarch type.
 This correspondence is ENTIRELY based on the blueprint schema.
 See the Haskell types HasBlueprintSchema instance and figure out which Plutarch type it corresponds to!
 For example, Integer's blueprint schema actually corresponds to PAsData PInteger.
-}
type PlyArgOf :: PType -> Type
type family PlyArgOf a = r | r -> a

type instance PlyArgOf (PAsData PInteger) = Integer

type instance PlyArgOf (PAsData PByteString) = ByteString

type instance PlyArgOf PData = BuiltinData

type instance PlyArgOf (PAsData (PBuiltinList a)) = [PlyArgOf a]

type instance PlyArgOf PRationalData = PlutusTx.Rational

type instance PlyArgOf (PMaybeData a) = Maybe (PlyArgOf a)

type instance PlyArgOf (PAsData (PMap Unsorted a b)) = PlutusMap.Map (PlyArgOf a) (PlyArgOf b)

type instance PlyArgOf (PAsData (PValue Unsorted NoGuarantees)) = Value

type instance PlyArgOf PLedger.V3.PCredential = Credential

type instance PlyArgOf PLedger.V3.PStakingCredential = StakingCredential

type instance PlyArgOf PLedger.V3.PAddress = Address

type instance PlyArgOf (PAsData PLedger.V3.PCurrencySymbol) = CurrencySymbol

type instance PlyArgOf (PAsData PLedger.V3.PTokenName) = TokenName

type instance PlyArgOf (PAsData PLedger.V3.PPubKeyHash) = PubKeyHash

type instance PlyArgOf (PAsData PLedger.V3.PScriptHash) = ScriptHash

type instance PlyArgOf (PAsData PLedger.V3.PPosixTime) = POSIXTime

type instance PlyArgOf (PLedger.V3.PExtended a) = Extended (PlyArgOf a)

type instance PlyArgOf (PLedger.V3.PUpperBound a) = UpperBound (PlyArgOf a)

type instance PlyArgOf (PLedger.V3.PLowerBound a) = LowerBound (PlyArgOf a)

type instance PlyArgOf (PLedger.V3.PInterval a) = Interval (PlyArgOf a)

type instance PlyArgOf (PAsData PLedger.V3.PTxId) = Ledger.V3.TxId

type instance PlyArgOf PLedger.V3.PTxOutRef = Ledger.V3.TxOutRef

type instance PlyArgOf PLedger.V3.PTxOut = Ledger.V3.TxOut

type instance PlyArgOf PLedger.V3.PDatum = Datum

type instance PlyArgOf PLedger.V3.PRedeemer = Redeemer

type instance PlyArgOf (PAsData PLedger.V3.PDatumHash) = DatumHash

type instance PlyArgOf (PAsData PLedger.V3.PRedeemerHash) = RedeemerHash
