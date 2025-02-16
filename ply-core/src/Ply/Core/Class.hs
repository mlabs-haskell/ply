{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ply.Core.Class (PlyArg (..), type ToDataConstraint, someBuiltinArg, someBuiltinArgData) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import qualified GHC.Generics as GHC
import qualified GHC.TypeLits as TLits

import Data.SOP
import Data.SOP.NS (trans_SOP)
import qualified Generics.SOP.GGP as GGP
import PlutusCore (DefaultUni, Some, ValueOf)
import qualified PlutusCore as PLC
import PlutusLedgerApi.V1 as LedgerCommon hiding (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut,
 )
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (DiffMilliSeconds))
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import qualified PlutusTx.Builtins as PlutusTx
import qualified PlutusTx.Ratio as PlutusTx

import Ply.Core.Schema

type BottomConstraint s t = 'TLits.Text s ~ TLits.ShowType t

{- | Class of haskell types that can be applied as arguments to a Plutus script.

This class is meant to not only convert a type to its UPLC builtin representation, but _also verify_ any
associated invariants the type may have. It may also _normalize_ types.

As a result, 'toBuiltinArg' is partial for types that don't hold their invariants.

== Example

This class supports default deriving for data encoded sum (PlyDS) types. All you have to do is describe 'UPLCDataSchema'.
Everything else can be auto derived.

@
data XSum = SumA ByteString | SumB Integer
  deriving stock GHC.Generic

instance PlyArg XSum where
  type UPLCDataSchema XSum = PlyDS '[ '[UPLCDataSchema ByteString], '[UPLCDataSchema Integer]]
@

Everything else will be auto derived such that `SumA bs` is represented by `Constr 0 [toData bs]`

== Laws

 1. If 'toBuiltinArg'/'toBuiltinArgData' is applied over `x`, all contained 'PlyArg's within should also have
 their corresponding 'toBuiltinArg'/'toBuiltinArgData' applied over them.

 2. If @UPLCRep a = Data@; @toBuiltinArgData = toBuiltinArg@

 3. Assuming a lawful 'Eq' instance (i.e the 'Eq' instance is aware of any invariants):
    @fromData (toBuiltinArgData x) == Just x@

 4. 'toBuiltinArgData' implementation must perform the same validations/normalizations as 'toBuiltinArg'.
-}
class (HasSchemaInstance (UPLCSchema a), ToPLCDefaultUni (UPLCSchema a)) => PlyArg a where
  type UPLCDataSchema a :: PlyDataSchema
  type UPLCDataSchema a = PlyDS (MapUPLCDataSchema2 (GGP.GCode a))
  type UPLCSchema a :: PlySchema
  type UPLCSchema a = PlyD (UPLCDataSchema a)
  type ToDataConstraint' a :: Constraint
  type ToDataConstraint' a = ()
  toBuiltinArg :: a -> SchemaInstanceOf (UPLCSchema a)
  default toBuiltinArg :: (ToDataConstraint' a, UPLCSchema a ~ PlyD (UPLCDataSchema a)) => a -> SchemaInstanceOf (UPLCSchema a)
  toBuiltinArg = getDataSchemaInstanceOf . toBuiltinArgData
  toBuiltinArgData :: ToDataConstraint' a => a -> DataSchemaInstanceOf (UPLCDataSchema a)
  default toBuiltinArgData ::
    ( GHC.Generic a
    , GGP.GFrom a
    , AllZip2 PlyArgPointwise (GGP.GCode a) (MapUPLCDataSchema2 (GGP.GCode a))
    , UPLCDataSchema a ~ PlyDS (MapUPLCDataSchema2 (GGP.GCode a))
    ) =>
    a ->
    DataSchemaInstanceOf (UPLCDataSchema a)
  toBuiltinArgData =
    wrapDataSum
      . trans_SOP (Proxy @PlyArgPointwise) (toBuiltinArgData . unI)
      . GGP.gfrom

type ToDataConstraint a =
  ( ToDataConstraint' a
  , ToPLCDefaultUni ( 'PlyD (UPLCDataSchema a))
  , DefaultUniTarget ( 'PlyD (UPLCDataSchema a)) ~ Data
  )

-- | Create a PlutusCore 'Some' value from a 'PlyArg'.
someBuiltinArg :: PlyArg a => a -> Some (ValueOf DefaultUni)
someBuiltinArg = PLC.someValue . toPLCDefaultUni . toBuiltinArg

-- | Create a PlutusCore 'Some' value from a 'PlyArg's data encoding form.
someBuiltinArgData :: (PlyArg a, ToDataConstraint a) => a -> Some (ValueOf DefaultUni)
someBuiltinArgData = PLC.someValue . toPLCDefaultUni . getDataSchemaInstanceOf . toBuiltinArgData

instance PlyArg Bool where
  type UPLCSchema Bool = PlyBool
  type UPLCDataSchema Bool = PlyDS ['[], '[]]
  toBuiltinArg = id

instance PlyArg Integer where
  type UPLCSchema Integer = PlyInt
  type UPLCDataSchema Integer = PlyDI
  toBuiltinArg = id
  toBuiltinArgData = wrapDataInt

instance PlyArg () where
  type UPLCSchema () = PlyUnit
  type UPLCDataSchema () = PlyDS '[ '[]]
  toBuiltinArg = id

instance PlyArg BuiltinByteString where
  type UPLCSchema BuiltinByteString = PlyByteStr
  type UPLCDataSchema BuiltinByteString = PlyDB
  toBuiltinArg = fromBuiltin
  toBuiltinArgData = wrapDataByteStr . PlutusTx.fromBuiltin

instance PlyArg ByteString where
  type UPLCSchema ByteString = PlyByteStr
  type UPLCDataSchema ByteString = PlyDB
  toBuiltinArg = id
  toBuiltinArgData = wrapDataByteStr

-- | 'toBuiltinArgData' is uncallable for this instance.
instance PlyArg Text where
  type UPLCSchema Text = PlyStr
  type ToDataConstraint' Text = BottomConstraint "toBuiltinArgData(Text): unsupported" Text
  type UPLCDataSchema Text = TLits.TypeError (TLits.Text "toBuiltinArgData(Text): unsupported")
  toBuiltinArg = id
  toBuiltinArgData = error "toBuiltinArgData(Text): uncallable function was called"

instance (PlyArg a, PlyArg b) => PlyArg (a, b) where
  type UPLCSchema (a, b) = PlyPairOf (UPLCSchema a) (UPLCSchema b)
  type ToDataConstraint' (a, b) = (ToDataConstraint' a, ToDataConstraint' b)
  type UPLCDataSchema (a, b) = PlyDS '[ [UPLCDataSchema a, UPLCDataSchema b]]
  toBuiltinArg (x, y) = (toBuiltinArg x, toBuiltinArg y)
  toBuiltinArgData (x, y) = wrapDataSum . SOP . Z $ toBuiltinArgData x :* toBuiltinArgData y :* Nil

instance PlyArg a => PlyArg [a] where
  type UPLCSchema [a] = PlyListOf (UPLCSchema a)
  type ToDataConstraint' [a] = ToDataConstraint' a
  type UPLCDataSchema [a] = PlyDL (UPLCDataSchema a)
  toBuiltinArg = map toBuiltinArg
  toBuiltinArgData = wrapDataList . map toBuiltinArgData

instance PlyArg PlutusTx.Rational where
  type UPLCDataSchema PlutusTx.Rational = PlyDS '[ [PlyDI, PlyDI]]
  toBuiltinArgData r = wrapDataSum . SOP . Z $ numerator :* denData :* Nil
    where
      numerator = toBuiltinArgData $ PlutusTx.numerator r
      den = PlutusTx.denominator r
      denData = if den /= 0 then toBuiltinArgData den else error "toBuiltinArg(PlutusTx.Rational): Expected positive denominator"

instance (PlyArg a, ToDataConstraint a, HasSchemaInstance (PlyD (UPLCDataSchema a))) => PlyArg (Maybe a) where
  type UPLCDataSchema (Maybe a) = PlyDS '[ '[UPLCDataSchema a], '[]]
  toBuiltinArgData Nothing = wrapDataSum . SOP . S $ Z Nil
  toBuiltinArgData (Just x) = wrapDataSum . SOP . Z $ toBuiltinArgData x :* Nil

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg Credential

instance PlyArg StakingCredential

instance PlyArg Address

-- | This verifies the underlying bytestring is exactly 0 (for adaSymbol) or 28 bytes.
instance PlyArg CurrencySymbol where
  type UPLCSchema CurrencySymbol = PlyByteStr
  type UPLCDataSchema CurrencySymbol = PlyDB
  toBuiltinArg cs@(CurrencySymbol x)
    | cs == adaSymbol = bs
    | otherwise =
      if BS.length bs == 28
        then bs
        else error "toBuiltinArg(CurrencySymbol): Expected exactly 0 or 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is no longer than 32 bytes.
instance PlyArg TokenName where
  type UPLCSchema TokenName = PlyByteStr
  type UPLCDataSchema TokenName = PlyDB
  toBuiltinArg (TokenName x) =
    if BS.length bs <= 32
      then bs
      else error "toBuiltinArg(TokenName): Expected 32 bytes or less"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

deriving newtype instance PlyArg AssetClass

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg PubKeyHash where
  type UPLCSchema PubKeyHash = PlyByteStr
  type UPLCDataSchema PubKeyHash = PlyDB
  toBuiltinArg (PubKeyHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(PubKeyHash): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is exactly 28 bytes.
instance PlyArg ScriptHash where
  type UPLCSchema ScriptHash = PlyByteStr
  type UPLCDataSchema ScriptHash = PlyDB
  toBuiltinArg (ScriptHash x) =
    if BS.length bs == 28
      then bs
      else error "toBuiltinArg(ScriptHash): Expected 28 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is exactly 32 bytes.
instance PlyArg DatumHash where
  type UPLCSchema DatumHash = PlyByteStr
  type UPLCDataSchema DatumHash = PlyDB
  toBuiltinArg (DatumHash x) =
    if BS.length bs == 32
      then bs
      else error "toBuiltinArg(DatumHash): Expected 32 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying bytestring is exactly 32 bytes.
instance PlyArg RedeemerHash where
  type UPLCSchema RedeemerHash = PlyByteStr
  type UPLCDataSchema RedeemerHash = PlyDB
  toBuiltinArg (RedeemerHash x) =
    if BS.length bs == 32
      then bs
      else error "toBuiltinArg(RedeemerHash): Expected 32 bytes"
    where
      bs = fromBuiltin x
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying integer is non-negative.
instance PlyArg POSIXTime where
  type UPLCSchema POSIXTime = PlyInt
  type UPLCDataSchema POSIXTime = PlyDI
  toBuiltinArg (POSIXTime i) =
    if i >= 0
      then i
      else error "toBuiltinArg(POSIXTime): Expected non-negative"
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

-- | This verifies the underlying integer is non-negative.
instance PlyArg DiffMilliSeconds where
  type UPLCSchema DiffMilliSeconds = PlyInt
  type UPLCDataSchema DiffMilliSeconds = PlyDI
  toBuiltinArg (DiffMilliSeconds i) =
    if i >= 0
      then i
      else error "toBuiltinArg(DiffMilliSeconds): Expected non-negative"
  toBuiltinArgData = toBuiltinArgData . toBuiltinArg

instance (PlyArg a, ToDataConstraint a, HasSchemaInstance (PlyD (UPLCDataSchema a))) => PlyArg (Extended a)

instance (PlyArg a, ToDataConstraint a, HasSchemaInstance (PlyD (UPLCDataSchema a))) => PlyArg (UpperBound a)

instance (PlyArg a, ToDataConstraint a, HasSchemaInstance (PlyD (UPLCDataSchema a))) => PlyArg (LowerBound a)

instance (PlyArg a, ToDataConstraint a, HasSchemaInstance (PlyD (UPLCDataSchema a))) => PlyArg (Interval a)

instance PlyArg DCert

-- | This verifies the underlying bytestring is exactly 32 bytes.
instance PlyArg TxId where
  -- TxId is a Constr data instead of just a bytestring, for some reason.
  type UPLCDataSchema TxId = PlyDS '[ '[PlyDB]]
  toBuiltinArgData (TxId x) =
    if BS.length bs == 32
      then wrapDataSum . SOP . Z $ toBuiltinArgData x :* Nil
      else error "toBuiltinArg(TxId): Expected 32 bytes"
    where
      bs = fromBuiltin x

-- | This verifies the txIdx is non-negative.
instance PlyArg TxOutRef where
  type UPLCDataSchema TxOutRef = PlyDS '[ '[UPLCDataSchema TxId, PlyDI]]
  toBuiltinArgData (TxOutRef txId txIdx) =
    if txIdx >= 0
      then wrapDataSum . SOP . Z $ toBuiltinArgData txId :* toBuiltinArgData txIdx :* Nil
      else error "toBuiltinArg(TxOutRef): Expected non-negative idx"

instance PlyArg ScriptPurpose

--------------------------------------
-- Utilities for generic derivation --
--------------------------------------

class (PlyArg a, ToDataConstraint a, UPLCDataSchema a ~ b) => PlyArgPointwise a b
instance (PlyArg a, ToDataConstraint a, UPLCDataSchema a ~ b) => PlyArgPointwise a b

type MapUPLCDataSchema2 :: [[Type]] -> [[PlyDataSchema]]
type family MapUPLCDataSchema2 xss where
  MapUPLCDataSchema2 '[] = '[]
  MapUPLCDataSchema2 (xs ': xss) = MapUPLCDataSchema xs ': MapUPLCDataSchema2 xss

type MapUPLCDataSchema :: [Type] -> [PlyDataSchema]
type family MapUPLCDataSchema xs where
  MapUPLCDataSchema '[] = '[]
  MapUPLCDataSchema (x ': xs) = UPLCDataSchema x ': MapUPLCDataSchema xs
