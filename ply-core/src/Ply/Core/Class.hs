{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg), someBuiltinArg) where

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage (ShowType, Text))

import PlutusCore (DefaultUni, Includes, Some, ValueOf)
import qualified PlutusCore as PLC
import qualified PlutusTx.AssocMap as PlutusMap
import Ply.LedgerExports as LedgerV1

type BottomConstraint s t = 'Text s ~ ShowType t

{- | Class of haskell types that can be applied as arguments to a Plutus script.

This class is meant to not only convert a type to its UPLC builtin representation, but _also verify_ any
associated invariants the type may have. It may also _normalize_ types.

As a result, 'toBuiltinArg' is partial for types that don't hold their invariants.
-}
class (DefaultUni `Includes` UPLCRep a) => PlyArg a where
  type UPLCRep a :: Type
  type ToDataConstraint a :: Constraint
  type ToDataConstraint a = ()
  toBuiltinArg :: a -> UPLCRep a
  toBuiltinArgData :: ToDataConstraint a => a -> Data

someBuiltinArg :: PlyArg a => a -> Some (ValueOf DefaultUni)
someBuiltinArg = PLC.someValue . toBuiltinArg

instance PlyArg Bool where
  type UPLCRep Bool = Bool
  toBuiltinArg = id
  toBuiltinArgData = toData

instance PlyArg Integer where
  type UPLCRep Integer = Integer
  toBuiltinArg = id
  toBuiltinArgData = toData

instance PlyArg () where
  type UPLCRep () = ()
  toBuiltinArg = id
  toBuiltinArgData = toData

instance PlyArg BuiltinByteString where
  type UPLCRep BuiltinByteString = ByteString
  toBuiltinArg = fromBuiltin
  toBuiltinArgData = toData

instance PlyArg ByteString where
  type UPLCRep ByteString = ByteString
  toBuiltinArg = id
  toBuiltinArgData = toData . toBuiltin

instance PlyArg Text where
  type UPLCRep Text = Text
  type ToDataConstraint Text = BottomConstraint "toBuiltinArgData(Text): unsupported" Text
  toBuiltinArg = id
  toBuiltinArgData = error "toBuiltinArgData(Text): uncallable function was called"

instance PlyArg Data where
  type UPLCRep Data = Data
  toBuiltinArg = id
  toBuiltinArgData = id

instance (PlyArg a, PlyArg b) => PlyArg (a, b) where
  type UPLCRep (a, b) = (UPLCRep a, UPLCRep b)
  type ToDataConstraint (a, b) = (ToDataConstraint a, ToDataConstraint b)
  toBuiltinArg (x, y) = (toBuiltinArg x, toBuiltinArg y)
  toBuiltinArgData (x, y) = Constr 0 [toBuiltinArgData x, toBuiltinArgData y]

instance PlyArg a => PlyArg [a] where
  type UPLCRep [a] = [UPLCRep a]
  type ToDataConstraint [a] = ToDataConstraint a
  toBuiltinArg = map toBuiltinArg
  toBuiltinArgData l = List $ map toBuiltinArgData l

instance (PlyArg a, ToDataConstraint a) => PlyArg (Maybe a) where
  type UPLCRep (Maybe a) = Data
  toBuiltinArg Nothing = Constr 1 []
  toBuiltinArg (Just x) = Constr 0 [toBuiltinArgData x]
  toBuiltinArgData = toBuiltinArg

instance
  ( PlyArg k
  , ToDataConstraint k
  , Ord k
  , PlyArg v
  , ToDataConstraint v
  ) =>
  PlyArg (PlutusMap.Map k v)
  where
  type UPLCRep (PlutusMap.Map k v) = [(Data, Data)]
  toBuiltinArg =
    map (bimap toBuiltinArgData toBuiltinArgData)
      . M.toAscList
      . M.fromList
      . PlutusMap.toList
  toBuiltinArgData = Map . toBuiltinArg

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
  toBuiltinArgData = Map . toBuiltinArg

instance PlyArg Credential where
  type UPLCRep Credential = Data
  toBuiltinArg cred =
    let credHash = case cred of
          PubKeyCredential (PubKeyHash h) -> fromBuiltin h
          ScriptCredential (ValidatorHash h) -> fromBuiltin h
     in if BS.length credHash == 28
          then toData cred
          else error "toBuiltinArg(Credential): Expected 28 bytes"
  toBuiltinArgData = toBuiltinArg

instance PlyArg StakingCredential where
  type UPLCRep StakingCredential = Data
  toBuiltinArg (StakingHash cred) = Constr 0 [toBuiltinArg cred]
  toBuiltinArg x = toData x
  toBuiltinArgData = toBuiltinArg

instance PlyArg Address where
  type UPLCRep Address = Data
  toBuiltinArg (Address cred stakeCred) =
    let credData = toBuiltinArg cred
        stakeCredData = toBuiltinArg stakeCred
     in Constr 0 [credData, stakeCredData]
  toBuiltinArgData = toBuiltinArg

instance PlyArg CurrencySymbol where
  type UPLCRep CurrencySymbol = ByteString
  toBuiltinArg cs@(CurrencySymbol x)
    | cs == adaSymbol = bs
    | otherwise =
      if BS.length bs == 28
        then bs
        else error "toBuiltinArg(CurrencySymbol): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg TokenName where
  type UPLCRep TokenName = ByteString
  toBuiltinArg (TokenName x) =
    -- FIXME: Is this the correct invariant for token name?
    if BS.length bs <= 32
      then bs
      else error "toBuiltinArg(TokenName): Expected 32 bytes or less"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg AssetClass where
  type UPLCRep AssetClass = Data
  toBuiltinArg (AssetClass i) = toBuiltinArgData i
  toBuiltinArgData = toBuiltinArg

instance PlyArg PubKeyHash where
  type UPLCRep PubKeyHash = ByteString
  toBuiltinArg (PubKeyHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(PubKeyHash): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg POSIXTime where
  type UPLCRep POSIXTime = Integer
  toBuiltinArg (POSIXTime i) =
    if i >= 0
      then i
      else error "toBuiltinArg(POSIXTime): Expected non-negative"
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg DiffMilliSeconds where
  type UPLCRep DiffMilliSeconds = Integer
  toBuiltinArg (DiffMilliSeconds i) =
    if i >= 0
      then i
      else error "toBuiltinArg(DiffMilliSeconds): Expected non-negative"
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance (PlyArg a, ToDataConstraint a) => PlyArg (Extended a) where
  type UPLCRep (Extended a) = Data
  toBuiltinArg NegInf = Constr 0 []
  toBuiltinArg (Finite a) = Constr 0 [toBuiltinArgData a]
  toBuiltinArg PosInf = Constr 1 []
  toBuiltinArgData = toBuiltinArg

instance (PlyArg a, ToDataConstraint a) => PlyArg (UpperBound a) where
  type UPLCRep (UpperBound a) = Data
  toBuiltinArg (UpperBound ext cls) = Constr 0 [toBuiltinArgData ext, toBuiltinArgData cls]
  toBuiltinArgData = toBuiltinArg

instance (PlyArg a, ToDataConstraint a) => PlyArg (LowerBound a) where
  type UPLCRep (LowerBound a) = Data
  toBuiltinArg (LowerBound ext cls) = Constr 0 [toBuiltinArgData ext, toBuiltinArgData cls]
  toBuiltinArgData = toBuiltinArg

instance (PlyArg a, ToDataConstraint a) => PlyArg (Interval a) where
  type UPLCRep (Interval a) = Data
  toBuiltinArg (Interval lb ub) = Constr 0 [toBuiltinArgData lb, toBuiltinArgData ub]
  toBuiltinArgData = toBuiltinArg

instance PlyArg DCert where
  type UPLCRep DCert = Data
  toBuiltinArg (DCertDelegRegKey ext) = Constr 0 [toBuiltinArgData ext]
  toBuiltinArg (DCertDelegDeRegKey ext) = Constr 1 [toBuiltinArgData ext]
  toBuiltinArg (DCertDelegDelegate ext pkh) = Constr 2 [toBuiltinArgData ext, toBuiltinArgData pkh]
  toBuiltinArg (DCertPoolRegister pkh1 pkh2) = Constr 3 [toBuiltinArgData pkh1, toBuiltinArgData pkh2]
  toBuiltinArg (DCertPoolRetire pkh i) = Constr 4 [toBuiltinArgData pkh, toBuiltinArgData i]
  toBuiltinArg DCertGenesis = Constr 5 []
  toBuiltinArg DCertMir = Constr 6 []
  toBuiltinArgData = toBuiltinArg

instance PlyArg TxId where
  -- TxId is a Constr data instead of just a bytestring, for some reason.
  type UPLCRep TxId = Data
  toBuiltinArg (TxId x) =
    if BS.length bs == 32
      then Constr 0 [toBuiltinArgData bs]
      else error "toBuiltinArg(TxId): Expected 32 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg TxOutRef where
  type UPLCRep TxOutRef = Data
  toBuiltinArg (TxOutRef txId txIdx) =
    if txIdx >= 0
      then Constr 0 [toBuiltinArgData txId, toBuiltinArgData txIdx]
      else error "toBuiltinArg(TxOutRef): Expected non-negative idx"
  toBuiltinArgData = toBuiltinArg

instance PlyArg LedgerV1.TxOut where
  type UPLCRep LedgerV1.TxOut = Data
  toBuiltinArg (LedgerV1.TxOut addr val dh) =
    Constr
      0
      [ toBuiltinArgData addr
      , toBuiltinArgData val
      , toBuiltinArgData dh
      ]
  toBuiltinArgData = toBuiltinArg

instance PlyArg LedgerV1.TxInInfo where
  type UPLCRep LedgerV1.TxInInfo = Data
  toBuiltinArg (LedgerV1.TxInInfo ref out) = Constr 0 [toBuiltinArgData ref, toBuiltinArgData out]
  toBuiltinArgData = toBuiltinArg

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
  toBuiltinArgData = toBuiltinArg

instance PlyArg ScriptPurpose where
  type UPLCRep ScriptPurpose = Data
  toBuiltinArg (Minting cs) = Constr 0 [toBuiltinArgData cs]
  toBuiltinArg (Spending ref) = Constr 1 [toBuiltinArgData ref]
  toBuiltinArg (Rewarding stakeCred) = Constr 2 [toBuiltinArgData stakeCred]
  toBuiltinArg (Certifying dcert) = Constr 3 [toBuiltinArgData dcert]
  toBuiltinArgData = toBuiltinArg

instance PlyArg LedgerV1.ScriptContext where
  type UPLCRep LedgerV1.ScriptContext = Data
  toBuiltinArg (LedgerV1.ScriptContext inf purp) = Constr 0 [toBuiltinArgData inf, toBuiltinArgData purp]
  toBuiltinArgData = toBuiltinArg

instance PlyArg Datum where
  type UPLCRep Datum = Data
  toBuiltinArg (Datum d) = toData d
  toBuiltinArgData = toBuiltinArg

instance PlyArg Redeemer where
  type UPLCRep Redeemer = Data
  toBuiltinArg (Redeemer d) = toData d
  toBuiltinArgData = toBuiltinArg

instance PlyArg ScriptHash where
  type UPLCRep ScriptHash = ByteString
  toBuiltinArg (ScriptHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(ScriptHash): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg ValidatorHash where
  type UPLCRep ValidatorHash = ByteString
  toBuiltinArg (ValidatorHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(ValidatorHash): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg MintingPolicyHash where
  type UPLCRep MintingPolicyHash = ByteString
  toBuiltinArg (MintingPolicyHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(MintingPolicyHash): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg DatumHash where
  type UPLCRep DatumHash = ByteString
  toBuiltinArg (DatumHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(DatumHash): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance PlyArg RedeemerHash where
  type UPLCRep RedeemerHash = ByteString
  toBuiltinArg (RedeemerHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(RedeemerHash): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg
