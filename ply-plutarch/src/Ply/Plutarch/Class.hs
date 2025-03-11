{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.Class (PlyArgOf) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Text (Text)

import Plutarch.Internal.Term (PType)
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Unsorted), PMap)
import Plutarch.LedgerApi.Utils
import qualified Plutarch.LedgerApi.V3 as PLedger.V3
import Plutarch.LedgerApi.Value (AmountGuarantees (NoGuarantees), PValue)
import Plutarch.Prelude
import PlutusLedgerApi.V3 as Ledger.V3
import qualified PlutusTx.AssocMap as PlutusMap
import qualified PlutusTx.Ratio as PlutusTx

import Ply (AsData)

-- | 'PlyArgOf' yields the corresponding Haskell type for a given Plutarch type.
type PlyArgOf :: PType -> Type
type family PlyArgOf a = r | r -> a

type instance PlyArgOf PBool = Bool

type instance PlyArgOf PInteger = Integer

type instance PlyArgOf PUnit = ()

type instance PlyArgOf PByteString = ByteString

type instance PlyArgOf PString = Text

type instance PlyArgOf PData = Data

type instance PlyArgOf (PBuiltinPair a b) = (PlyArgOf a, PlyArgOf b)

type instance PlyArgOf (PBuiltinList a) = [PlyArgOf a]

type instance PlyArgOf PRationalData = PlutusTx.Rational

type instance PlyArgOf (PMaybeData a) = Maybe (PlyArgOf a)

type instance PlyArgOf (PMap Unsorted a b) = PlutusMap.Map (PlyArgOf a) (PlyArgOf b)

type instance PlyArgOf (PValue Unsorted NoGuarantees) = Value

type instance PlyArgOf PLedger.V3.PCredential = Credential

type instance PlyArgOf PLedger.V3.PStakingCredential = StakingCredential

type instance PlyArgOf PLedger.V3.PAddress = Address

type instance PlyArgOf PLedger.V3.PCurrencySymbol = CurrencySymbol

type instance PlyArgOf PLedger.V3.PTokenName = TokenName

type instance PlyArgOf PLedger.V3.PPubKeyHash = PubKeyHash

type instance PlyArgOf PLedger.V3.PScriptHash = ScriptHash

type instance PlyArgOf PLedger.V3.PPosixTime = POSIXTime

type instance PlyArgOf (PLedger.V3.PExtended a) = Extended (PlyArgOf a)

type instance PlyArgOf (PLedger.V3.PUpperBound a) = UpperBound (PlyArgOf a)

type instance PlyArgOf (PLedger.V3.PLowerBound a) = LowerBound (PlyArgOf a)

type instance PlyArgOf (PLedger.V3.PInterval a) = Interval (PlyArgOf a)

type instance PlyArgOf PLedger.V3.PTxId = Ledger.V3.TxId

type instance PlyArgOf PLedger.V3.PTxOutRef = Ledger.V3.TxOutRef

type instance PlyArgOf PLedger.V3.PTxOut = Ledger.V3.TxOut

type instance PlyArgOf PLedger.V3.PTxInInfo = Ledger.V3.TxInInfo

type instance PlyArgOf PLedger.V3.PTxInfo = Ledger.V3.TxInfo

type instance PlyArgOf PLedger.V3.PScriptPurpose = Ledger.V3.ScriptPurpose

type instance PlyArgOf PLedger.V3.PScriptContext = Ledger.V3.ScriptContext

type instance PlyArgOf PLedger.V3.PDatum = Datum

type instance PlyArgOf PLedger.V3.PRedeemer = Redeemer

type instance PlyArgOf PLedger.V3.PDatumHash = DatumHash

type instance PlyArgOf PLedger.V3.PRedeemerHash = RedeemerHash

type instance PlyArgOf (PAsData a) = AsData (PlyArgOf a)
