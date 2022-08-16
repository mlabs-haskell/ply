{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ply.Core.Class (
  PlyArg (UPLCRep, toBuiltinArg),
  PlyArgData (toBuiltinArgData),
  someBuiltinArg,
  someBuiltinArgData,
) where

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import PlutusCore (DefaultUni, Includes, Some, ValueOf)
import qualified PlutusCore as PLC
import qualified PlutusTx.AssocMap as PlutusMap

import Ply.LedgerExports.Common
import qualified Ply.LedgerExports.V1 as LedgerV1

{- | Class of haskell types that can be applied as arguments to a Plutus script.

This class is meant to not only convert a type to its UPLC builtin representation, but _also verify_ any
associated invariants the type may have. It may also _normalize_ types.

As a result, 'toBuiltinArg' is partial for types that don't hold their invariants.

== Laws

 1. If 'toBuiltinArg' is applied over `x`, all contained 'PlyArg's within should also have
 their corresponding 'toBuiltinArg' applied over them.

 2. Assuming a lawful 'Eq' instance (i.e the 'Eq' instance is aware of any invariants):
    @fromBuiltin' (toBuiltinArg x) == x@

    Where `fromBuiltin'` is capable of parsing the _correct_ UPLC representation of a type into the type itself.
-}
class DefaultUni `Includes` UPLCRep a => PlyArg a where
  type UPLCRep a :: Type
  toBuiltinArg :: a -> UPLCRep a

{- | Class of haskell types that can be applied as data encoded arguments to a Plutus script.

Similar to 'PlyArg', this class should also verify the same invariants for each type.

If 'UPLCRep a ~ Data', this instance is free and does not need any explicit method definition.

== Laws

 1. If @UPLCRep a = Data@; @toBuiltinArgData = toBuiltinArg@

    Otherwise: @toBuiltinArgData = toData . toBuiltinArg@

    (assume 'toData @ByteString' simply delegates to 'toData @BuiltinByteString')

 2. Assuming a lawful 'Eq' instance (i.e the 'Eq' instance is aware of any invariants):
    @fromData (toBuiltinArgData x) == Just x@
-}
class PlyArg a => PlyArgData a where
  toBuiltinArgData :: a -> Data
  default toBuiltinArgData :: UPLCRep a ~ Data => a -> Data
  toBuiltinArgData = toBuiltinArg

-- | Create a PlutusCore 'Some' value from a 'PlyArg'.
someBuiltinArg :: PlyArg a => a -> Some (ValueOf DefaultUni)
someBuiltinArg = PLC.someValue . toBuiltinArg

-- | Create a PlutusCore 'Some' data value from a 'PlyArg'.
someBuiltinArgData :: PlyArgData a => a -> Some (ValueOf DefaultUni)
someBuiltinArgData = PLC.someValue . toBuiltinArgData

instance PlyArg Bool where
  type UPLCRep Bool = Bool
  toBuiltinArg = id
instance PlyArgData Bool where
  toBuiltinArgData = toData

instance PlyArg Integer where
  type UPLCRep Integer = Integer
  toBuiltinArg = id
instance PlyArgData Integer where
  toBuiltinArgData = toData

instance PlyArg () where
  type UPLCRep () = ()
  toBuiltinArg = id
instance PlyArgData () where
  toBuiltinArgData = toData

instance PlyArg BuiltinByteString where
  type UPLCRep BuiltinByteString = ByteString
  toBuiltinArg = fromBuiltin
instance PlyArgData BuiltinByteString where
  toBuiltinArgData = toData

instance PlyArg ByteString where
  type UPLCRep ByteString = ByteString
  toBuiltinArg = id
instance PlyArgData ByteString where
  toBuiltinArgData = toData . toBuiltin

instance PlyArg Text where
  type UPLCRep Text = Text
  toBuiltinArg = id

instance PlyArg Data where
  type UPLCRep Data = Data
  toBuiltinArg = id
instance PlyArgData Data

instance (PlyArg a, PlyArg b) => PlyArg (a, b) where
  type UPLCRep (a, b) = (UPLCRep a, UPLCRep b)
  toBuiltinArg (x, y) = (toBuiltinArg x, toBuiltinArg y)
instance (PlyArgData a, PlyArgData b) => PlyArgData (a, b) where
  toBuiltinArgData (x, y) = Constr 0 [toBuiltinArgData x, toBuiltinArgData y]

instance PlyArg a => PlyArg [a] where
  type UPLCRep [a] = [UPLCRep a]
  toBuiltinArg = map toBuiltinArg
instance PlyArgData a => PlyArgData [a] where
  toBuiltinArgData l = List $ map toBuiltinArgData l

instance PlyArgData a => PlyArg (Maybe a) where
  type UPLCRep (Maybe a) = Data
  toBuiltinArg Nothing = Constr 1 []
  toBuiltinArg (Just x) = Constr 0 [toBuiltinArgData x]
instance PlyArgData a => PlyArgData (Maybe a)

-- | This instance sorts the map.
instance
  ( PlyArgData k
  , Ord k
  , PlyArgData v
  ) =>
  PlyArg (PlutusMap.Map k v)
  where
  type UPLCRep (PlutusMap.Map k v) = [(Data, Data)]
  toBuiltinArg =
    map (bimap toBuiltinArgData toBuiltinArgData)
      . M.toAscList
      . M.fromList
      . PlutusMap.toList

instance
  ( PlyArgData k
  , Ord k
  , PlyArgData v
  ) =>
  PlyArgData (PlutusMap.Map k v)
  where
  toBuiltinArgData = Map . toBuiltinArg

-- | This instance sorts the 'Value' and removes all zero entries.
instance PlyArg Value where
  type UPLCRep Value = [(Data, Data)]
  toBuiltinArg (Value m) =
    map
      ( \(cs, tkMap) ->
          ( toBuiltinArgData cs
          , toBuiltinArgData
              . PlutusMap.fromList
              $ map (bimap toBuiltinArg toBuiltinArg) tkMap
          )
      )
      . M.toAscList
      $ M.map M.toAscList target
    where
      target =
        M.filter (not . M.null . M.filter (/= 0))
          . M.fromListWith (<>)
          . map (second (M.fromListWith (+) . PlutusMap.toList))
          $ PlutusMap.toList m

instance PlyArgData Value where
  toBuiltinArgData = Map . toBuiltinArg

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg Credential where
  type UPLCRep Credential = Data
  toBuiltinArg cred = case cred of
    PubKeyCredential h -> Constr 0 [toBuiltinArgData h]
    ScriptCredential h -> Constr 1 [toBuiltinArgData h]

instance PlyArgData Credential

instance PlyArg StakingCredential where
  type UPLCRep StakingCredential = Data
  toBuiltinArg (StakingHash cred) = Constr 0 [toBuiltinArg cred]
  toBuiltinArg x = toData x
instance PlyArgData StakingCredential

instance PlyArg Address where
  type UPLCRep Address = Data
  toBuiltinArg (Address cred stakeCred) =
    let credData = toBuiltinArg cred
        stakeCredData = toBuiltinArg stakeCred
     in Constr 0 [credData, stakeCredData]
instance PlyArgData Address

-- | This verifies the underlying bytestring is exactly 0 (for adaSymbol) or 28 bytes.
instance PlyArg CurrencySymbol where
  type UPLCRep CurrencySymbol = ByteString
  toBuiltinArg cs@(CurrencySymbol x)
    | cs == adaSymbol = bs
    | otherwise =
      if BS.length bs == 28
        then bs
        else error "toBuiltinArg(CurrencySymbol): Expected exactly 0 or 28 bytes"
    where
      bs = fromBuiltin x

instance PlyArgData CurrencySymbol where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is no longer than 32 bytes.
instance PlyArg TokenName where
  type UPLCRep TokenName = ByteString
  toBuiltinArg (TokenName x) =
    if BS.length bs <= 32
      then bs
      else error "toBuiltinArg(TokenName): Expected 32 bytes or less"
    where
      bs = fromBuiltin x

instance PlyArgData TokenName where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg AssetClass where
  type UPLCRep AssetClass = Data
  toBuiltinArg (AssetClass i) = toBuiltinArgData i
instance PlyArgData AssetClass

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg PubKeyHash where
  type UPLCRep PubKeyHash = ByteString
  toBuiltinArg (PubKeyHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(PubKeyHash): Expected 28 bytes"
    where
      bs = fromBuiltin x

instance PlyArgData PubKeyHash where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying integer is non-negative.
instance PlyArg POSIXTime where
  type UPLCRep POSIXTime = Integer
  toBuiltinArg (POSIXTime i) =
    if i >= 0
      then i
      else error "toBuiltinArg(POSIXTime): Expected non-negative"

instance PlyArgData POSIXTime where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying integer is non-negative.
instance PlyArg DiffMilliSeconds where
  type UPLCRep DiffMilliSeconds = Integer
  toBuiltinArg (DiffMilliSeconds i) =
    if i >= 0
      then i
      else error "toBuiltinArg(DiffMilliSeconds): Expected non-negative"

instance PlyArgData a => PlyArg (Extended a) where
  type UPLCRep (Extended a) = Data
  toBuiltinArg NegInf = Constr 0 []
  toBuiltinArg (Finite a) = Constr 0 [toBuiltinArgData a]
  toBuiltinArg PosInf = Constr 1 []
instance PlyArgData a => PlyArgData (Extended a)

instance PlyArgData a => PlyArg (UpperBound a) where
  type UPLCRep (UpperBound a) = Data
  toBuiltinArg (UpperBound ext cls) = Constr 0 [toBuiltinArgData ext, toBuiltinArgData cls]
instance PlyArgData a => PlyArgData (UpperBound a)

instance PlyArgData a => PlyArg (LowerBound a) where
  type UPLCRep (LowerBound a) = Data
  toBuiltinArg (LowerBound ext cls) = Constr 0 [toBuiltinArgData ext, toBuiltinArgData cls]
instance PlyArgData a => PlyArgData (LowerBound a)

instance PlyArgData a => PlyArg (Interval a) where
  type UPLCRep (Interval a) = Data
  toBuiltinArg (Interval lb ub) = Constr 0 [toBuiltinArgData lb, toBuiltinArgData ub]
instance PlyArgData a => PlyArgData (Interval a)

instance PlyArg DCert where
  type UPLCRep DCert = Data
  toBuiltinArg (DCertDelegRegKey ext) = Constr 0 [toBuiltinArgData ext]
  toBuiltinArg (DCertDelegDeRegKey ext) = Constr 1 [toBuiltinArgData ext]
  toBuiltinArg (DCertDelegDelegate ext pkh) = Constr 2 [toBuiltinArgData ext, toBuiltinArgData pkh]
  toBuiltinArg (DCertPoolRegister pkh1 pkh2) = Constr 3 [toBuiltinArgData pkh1, toBuiltinArgData pkh2]
  toBuiltinArg (DCertPoolRetire pkh i) = Constr 4 [toBuiltinArgData pkh, toBuiltinArgData i]
  toBuiltinArg DCertGenesis = Constr 5 []
  toBuiltinArg DCertMir = Constr 6 []
instance PlyArgData DCert

-- | This verifies the underlying bytestring is exactly 32 bytes.
instance PlyArg TxId where
  -- TxId is a Constr data instead of just a bytestring, for some reason.
  type UPLCRep TxId = Data
  toBuiltinArg (TxId x) =
    if BS.length bs == 32
      then Constr 0 [toBuiltinArgData bs]
      else error "toBuiltinArg(TxId): Expected 32 bytes"
    where
      bs = fromBuiltin x

instance PlyArgData TxId

-- | This verifies the txIdx is non-negative.
instance PlyArg TxOutRef where
  type UPLCRep TxOutRef = Data
  toBuiltinArg (TxOutRef txId txIdx) =
    if txIdx >= 0
      then Constr 0 [toBuiltinArgData txId, toBuiltinArgData txIdx]
      else error "toBuiltinArg(TxOutRef): Expected non-negative idx"

instance PlyArgData TxOutRef

instance PlyArg LedgerV1.TxOut where
  type UPLCRep LedgerV1.TxOut = Data
  toBuiltinArg (LedgerV1.TxOut addr val dh) =
    Constr
      0
      [ toBuiltinArgData addr
      , toBuiltinArgData val
      , toBuiltinArgData dh
      ]
instance PlyArgData LedgerV1.TxOut

instance PlyArg LedgerV1.TxInInfo where
  type UPLCRep LedgerV1.TxInInfo = Data
  toBuiltinArg (LedgerV1.TxInInfo ref out) = Constr 0 [toBuiltinArgData ref, toBuiltinArgData out]
instance PlyArgData LedgerV1.TxInInfo

instance PlyArg LedgerV1.TxInfo where
  type UPLCRep LedgerV1.TxInfo = Data
  toBuiltinArg
    ( LedgerV1.TxInfo
        inps
        outs
        fee
        mint
        dcert
        wdrl
        rnge
        signatories
        datumMap
        txId
      ) =
      Constr
        0
        [ toBuiltinArgData inps
        , toBuiltinArgData outs
        , toBuiltinArgData fee
        , toBuiltinArgData mint
        , toBuiltinArgData dcert
        , toBuiltinArgData wdrl
        , toBuiltinArgData rnge
        , toBuiltinArgData signatories
        , toBuiltinArgData datumMap
        , toBuiltinArgData txId
        ]
instance PlyArgData LedgerV1.TxInfo

instance PlyArg ScriptPurpose where
  type UPLCRep ScriptPurpose = Data
  toBuiltinArg (Minting cs) = Constr 0 [toBuiltinArgData cs]
  toBuiltinArg (Spending ref) = Constr 1 [toBuiltinArgData ref]
  toBuiltinArg (Rewarding stakeCred) = Constr 2 [toBuiltinArgData stakeCred]
  toBuiltinArg (Certifying dcert) = Constr 3 [toBuiltinArgData dcert]
instance PlyArgData ScriptPurpose

instance PlyArg LedgerV1.ScriptContext where
  type UPLCRep LedgerV1.ScriptContext = Data
  toBuiltinArg (LedgerV1.ScriptContext inf purp) = Constr 0 [toBuiltinArgData inf, toBuiltinArgData purp]
instance PlyArgData LedgerV1.ScriptContext

instance PlyArg Datum where
  type UPLCRep Datum = Data
  toBuiltinArg (Datum d) = toData d
instance PlyArgData Datum

instance PlyArg Redeemer where
  type UPLCRep Redeemer = Data
  toBuiltinArg (Redeemer d) = toData d
instance PlyArgData Redeemer

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg ScriptHash where
  type UPLCRep ScriptHash = ByteString
  toBuiltinArg (ScriptHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(ScriptHash): Expected 28 bytes"
    where
      bs = fromBuiltin x

instance PlyArgData ScriptHash where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg ValidatorHash where
  type UPLCRep ValidatorHash = ByteString
  toBuiltinArg (ValidatorHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(ValidatorHash): Expected 28 bytes"
    where
      bs = fromBuiltin x

instance PlyArgData ValidatorHash where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg MintingPolicyHash where
  type UPLCRep MintingPolicyHash = ByteString
  toBuiltinArg (MintingPolicyHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(MintingPolicyHash): Expected 28 bytes"
    where
      bs = fromBuiltin x

instance PlyArgData MintingPolicyHash where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg DatumHash where
  type UPLCRep DatumHash = ByteString
  toBuiltinArg (DatumHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(DatumHash): Expected 28 bytes"
    where
      bs = fromBuiltin x

instance PlyArgData DatumHash where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg RedeemerHash where
  type UPLCRep RedeemerHash = ByteString
  toBuiltinArg (RedeemerHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(RedeemerHash): Expected 28 bytes"
    where
      bs = fromBuiltin x

instance PlyArgData RedeemerHash where
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg
