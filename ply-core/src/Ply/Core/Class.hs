{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Class (PlyArg (..)) where

import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))

import PlutusCore (DefaultUni, Some, ValueOf)
import qualified PlutusCore as PLC
import PlutusLedgerApi.Common (
  BuiltinByteString,
  BuiltinData,
  Data (B, Constr, I, List, Map),
 )
import qualified PlutusLedgerApi.V1.Value as Value
import qualified PlutusLedgerApi.V3 as V3
import PlutusTx.Blueprint (DefinitionsFor, HasBlueprintDefinition, HasBlueprintSchema, UnrollAll, definitionRef)
import qualified PlutusTx.IsData as PlutusTx
import qualified PlutusTx.Ratio as PlutusTx

import Ply.Core.Schema (
  SchemaDescription (ConstrType, ListType, MapType, PairType),
  deriveSchemaDescriptions,
  normalizeSchemaDescription,
 )
import Ply.Core.Schema.Description (SchemaDescription (DataListType, SimpleType), descriptionFromPlutus)

{- | Class of haskell types that can be applied as arguments to a Plutus script.

This converts the Haskell type into its corresponding Plutus Core presentation according to the associated schema.
As such, the produced Plutus Core value MUST hold the same structure as the schema dictates.

This class can be default derived for types that have their 'PlutusTx.ToData' instance auto-derived using PlutusTx TH helpers.
-}
class (DefinitionsFor (UnrollAll '[a]), HasBlueprintDefinition a, HasBlueprintSchema a (UnrollAll '[a])) => PlyArg a where
  toSomeBuiltinArg :: a -> Some (ValueOf DefaultUni)
  default toSomeBuiltinArg :: PlutusTx.ToData a => a -> Some (ValueOf DefaultUni)
  toSomeBuiltinArg x = matchSchemaToValue sch dat
    where
      dat = PlutusTx.toData x
      sch = getSchemaOrErr $ Proxy @a

matchSchemaToValue :: SchemaDescription -> Data -> Some (ValueOf DefaultUni)
matchSchemaToValue (SimpleType "#unit") (Constr 0 []) = PLC.someValue ()
matchSchemaToValue (SimpleType "#boolean") (Constr ix []) = PLC.someValue $ ix /= 0
matchSchemaToValue (SimpleType "#integer") (I i) = PLC.someValue i
matchSchemaToValue (SimpleType "#bytes") (B b) = PLC.someValue b
matchSchemaToValue (SimpleType "integer") (I i) = PLC.someValue $ I i
matchSchemaToValue (SimpleType "bytes") (B b) = PLC.someValue $ B b
matchSchemaToValue (ListType inner) (List items) = foldr (\x acc -> matchSchemaToValue inner x `seq` acc) (PLC.someValue [items]) items
matchSchemaToValue (PairType a b) (Constr ix els)
  | ix /= 0 = error "absurd: Constr index non-zero for pair"
  | otherwise = PLC.someValue $ case els of
    [f, s] -> (matchSchemaToValue a f `seq` f, matchSchemaToValue b s `seq` s)
    _ -> error "absurd: Constr number of elements not equal to 2 for pair"
matchSchemaToValue (DataListType inner) (List items) = foldr (\x acc -> matchSchemaToValue inner x `seq` acc) (PLC.someValue $ List items) items
matchSchemaToValue (MapType key val) (Map items) = foldr (\(keyDat, valDat) acc -> matchSchemaToValue key keyDat `seq` matchSchemaToValue val valDat `seq` acc) (PLC.someValue $ Map items) items
matchSchemaToValue (ConstrType (toList -> constituents)) (Constr (fromInteger -> ix) els)
  | ix < 0 || ix >= length constituents = error "absurd: Constr index beyond expected range"
  | otherwise = PLC.someValue . foldr (\(el, sch) acc -> matchSchemaToValue sch el `seq` acc) (Constr (toInteger ix) els) . zip els $ constituents !! ix
matchSchemaToValue sch val = error $ "absurd: matchSchemaToValue invalid schema/value combination\nSchema: " ++ show sch ++ "\nValue: " ++ show val

getSchemaOrErr :: forall a. (HasBlueprintDefinition a, DefinitionsFor (UnrollAll '[a])) => Proxy a -> SchemaDescription
getSchemaOrErr _ = runIdentity $ do
  defMap <- deriveSchemaDescriptions @'[a] errF
  let schemaRef = definitionRef @a @(UnrollAll '[a])
      schemaDescr = fromMaybe (errF schemaRef) $ descriptionFromPlutus schemaRef
  case normalizeSchemaDescription defMap schemaDescr of
    Left missing -> error $ "absurd: Schema definition not found in generated map: " ++ show missing
    Right x -> pure x
  where
    errF sch = error $ "absurd: Unsupported schema: " ++ show sch ++ "\n\nThis should have been caught during script reading"

instance PlyArg ()
instance PlyArg Bool
instance PlyArg BuiltinByteString
instance PlyArg BuiltinData
instance PlyArg Integer
instance PlyArg PlutusTx.Rational
instance PlyArg V3.Address
instance PlyArg V3.Credential
instance PlyArg V3.StakingCredential
instance PlyArg V3.LedgerBytes
instance PlyArg V3.PubKeyHash
instance PlyArg V3.Datum
instance PlyArg V3.DatumHash
instance PlyArg V3.Redeemer
instance PlyArg V3.RedeemerHash
instance PlyArg V3.ScriptHash
instance PlyArg (V3.Interval V3.POSIXTime)
instance PlyArg V3.POSIXTime
instance PlyArg Value.AssetClass
instance PlyArg V3.CurrencySymbol
instance PlyArg V3.TokenName
instance PlyArg V3.Value
instance PlyArg V3.OutputDatum
instance PlyArg V3.TxOut
instance PlyArg V3.TxId
instance PlyArg V3.TxOutRef
