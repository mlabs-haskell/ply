{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ply.Core.Class (PlyArg (..), type ToDataConstraint, someBuiltinArg, someBuiltinArgData) where

import Data.ByteString (ByteString)
import Data.Kind (Constraint)
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage (ShowType, Text))

import Data.SOP
import PlutusCore (DefaultUni, Some, ValueOf)
import qualified GHC.TypeLits as TLits
import qualified PlutusCore as PLC
import PlutusLedgerApi.V1 as LedgerCommon hiding (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut,
 )
import qualified PlutusTx.Builtins as PlutusTx
import qualified PlutusTx.Ratio as PlutusTx

import Ply.Core.Schema

type BottomConstraint s t = 'Text s ~ ShowType t

{- | Class of haskell types that can be applied as arguments to a Plutus script.

This class is meant to not only convert a type to its UPLC builtin representation, but _also verify_ any
associated invariants the type may have. It may also _normalize_ types.

As a result, 'toBuiltinArg' is partial for types that don't hold their invariants.

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
  type UPLCSchema a :: PlySchema
  type UPLCSchema a = PlyD (UPLCDataSchema a)
  type ToDataConstraint' a :: Constraint
  type ToDataConstraint' a = ()
  toBuiltinArg :: a -> SchemaInstanceOf (UPLCSchema a)
  default toBuiltinArg :: (ToDataConstraint' a, UPLCSchema a ~ PlyD (UPLCDataSchema a)) => a -> SchemaInstanceOf (UPLCSchema a)
  toBuiltinArg = getDataSchemaInstanceOf . toBuiltinArgData
  toBuiltinArgData :: ToDataConstraint' a => a -> DataSchemaInstanceOf (UPLCDataSchema a)

type ToDataConstraint a = (ToDataConstraint' a, ToPLCDefaultUni ( 'PlyD (UPLCDataSchema a)), DefaultUniTarget ( 'PlyD (UPLCDataSchema a)) ~ Data)

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
  toBuiltinArgData False = wrapDataSum $ Z Nil
  toBuiltinArgData True = wrapDataSum . S $ Z Nil

instance PlyArg Integer where
  type UPLCSchema Integer = PlyInt
  type UPLCDataSchema Integer = PlyDI
  toBuiltinArg = id
  toBuiltinArgData = wrapDataInt

instance PlyArg () where
  type UPLCSchema () = PlyUnit
  type UPLCDataSchema () = PlyDS '[ '[]]
  toBuiltinArg = id
  toBuiltinArgData _ = wrapDataSum $ Z Nil

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
  toBuiltinArgData (x, y) = wrapDataSum . Z $ toBuiltinArgData x :* toBuiltinArgData y :* Nil

instance PlyArg a => PlyArg [a] where
  type UPLCSchema [a] = PlyListOf (UPLCSchema a)
  type ToDataConstraint' [a] = ToDataConstraint' a
  type UPLCDataSchema [a] = PlyDL (UPLCDataSchema a)
  toBuiltinArg = map toBuiltinArg
  toBuiltinArgData = wrapDataList . map toBuiltinArgData

instance PlyArg PlutusTx.Rational where
  type UPLCDataSchema PlutusTx.Rational = PlyDS '[ [PlyDI, PlyDI]]
  toBuiltinArgData r = wrapDataSum . Z $ denData :* denData :* Nil
    where
      den = PlutusTx.denominator r
      denData = if den /= 0 then toBuiltinArgData den else error "toBuiltinArg(PlutusTx.Rational): Expected positive denominator"

instance (PlyArg a, ToDataConstraint a, HasSchemaInstance (PlyD (UPLCDataSchema a))) => PlyArg (Maybe a) where
  type UPLCDataSchema (Maybe a) = PlyDS '[ '[UPLCDataSchema a], '[]]
  toBuiltinArgData Nothing = wrapDataSum . S $ Z Nil
  toBuiltinArgData (Just x) = wrapDataSum . Z $ toBuiltinArgData x :* Nil
