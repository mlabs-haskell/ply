{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.Class (PlyArgOf) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Plutarch.Api.V1
import Plutarch.Prelude
import qualified PlutusTx.AssocMap as PlutusMap
import Ply.LedgerExports

-- TODO: How to handle 'PAsData'?

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

type instance PlyArgOf (PMaybeData a) = Maybe (PlyArgOf a)

type instance PlyArgOf (PMap Sorted a b) = PlutusMap.Map (PlyArgOf a) (PlyArgOf b)

type instance PlyArgOf (PValue Sorted NonZero) = Value

type instance PlyArgOf PCredential = Credential

type instance PlyArgOf PStakingCredential = StakingCredential

type instance PlyArgOf PAddress = Address

type instance PlyArgOf PCurrencySymbol = CurrencySymbol

type instance PlyArgOf PTokenName = TokenName

type instance PlyArgOf PPubKeyHash = PubKeyHash

type instance PlyArgOf PPOSIXTime = POSIXTime

type instance PlyArgOf (PExtended a) = Extended (PlyArgOf a)

type instance PlyArgOf (PUpperBound a) = UpperBound (PlyArgOf a)

type instance PlyArgOf (PLowerBound a) = LowerBound (PlyArgOf a)

type instance PlyArgOf (PInterval a) = Interval (PlyArgOf a)

type instance PlyArgOf PDCert = DCert

type instance PlyArgOf PTxId = TxId

type instance PlyArgOf PTxOutRef = TxOutRef

type instance PlyArgOf PTxOut = TxOut

type instance PlyArgOf PTxInInfo = TxInInfo

type instance PlyArgOf PTxInfo = TxInfo

type instance PlyArgOf PScriptPurpose = ScriptPurpose

type instance PlyArgOf PScriptContext = ScriptContext

type instance PlyArgOf PDatum = Datum

type instance PlyArgOf PRedeemer = Redeemer

type instance PlyArgOf PValidatorHash = ValidatorHash

type instance PlyArgOf PDatumHash = DatumHash

type instance PlyArgOf PRedeemerHash = RedeemerHash
