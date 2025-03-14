{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Class (PlyArg (..)) where

import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)

import PlutusCore (DefaultUni, Some, ValueOf)
import qualified PlutusCore as PLC
import PlutusLedgerApi.V1 as LedgerCommon (
  Data (B, Constr, I, List, Map),
 )
import PlutusTx.Blueprint (DefinitionsFor, HasBlueprintDefinition, UnrollAll, definitionRef)
import qualified PlutusTx.IsData as PlutusTx

import Ply.Core.Schema (
  SchemaDescription (ConstrType, ListType, MapType),
  deriveSchemaDescriptions,
  normalizeSchemaDescription,
 )
import Ply.Core.Schema.Description (SchemaDescription (DataListType, SimpleType), descriptionFromPlutus)

-- | Class of haskell types that can be applied as arguments to a Plutus script.
class (PlutusTx.ToData a, HasBlueprintDefinition a, DefinitionsFor (UnrollAll '[a])) => PlyArg a where
  toSomeBuiltinArg :: a -> Some (ValueOf DefaultUni)

instance {-# OVERLAPPABLE #-} (PlutusTx.ToData a, HasBlueprintDefinition a, DefinitionsFor (UnrollAll '[a])) => PlyArg a where
  toSomeBuiltinArg x = matchSchemaToValue sch dat
    where
      dat = PlutusTx.toData x
      sch = getSchemaOrErr @a

matchSchemaToValue :: SchemaDescription -> Data -> Some (ValueOf DefaultUni)
matchSchemaToValue (SimpleType "#unit") (Constr 0 []) = PLC.someValue ()
matchSchemaToValue (SimpleType "#boolean") (Constr ix []) = PLC.someValue $ ix /= 0
matchSchemaToValue (SimpleType "#integer") (I i) = PLC.someValue i
matchSchemaToValue (SimpleType "#bytes") (B b) = PLC.someValue b
matchSchemaToValue (SimpleType "integer") (I i) = PLC.someValue $ I i
matchSchemaToValue (SimpleType "bytes") (B b) = PLC.someValue $ B b
matchSchemaToValue (ListType inner) (List items) = foldr (\x acc -> matchSchemaToValue inner x `seq` acc) (PLC.someValue [items]) items
matchSchemaToValue (DataListType inner) (List items) = foldr (\x acc -> matchSchemaToValue inner x `seq` acc) (PLC.someValue $ List items) items
matchSchemaToValue (MapType key val) (Map items) = foldr (\(keyDat, valDat) acc -> matchSchemaToValue key keyDat `seq` matchSchemaToValue val valDat `seq` acc) (PLC.someValue $ Map items) items
matchSchemaToValue (ConstrType (toList -> constituents)) (Constr (fromInteger -> ix) els) =
  PLC.someValue $
    if ix < 0 || ix >= length constituents
      then error "absurd: Constr index beyond expected range"
      else foldr (\(el, sch) acc -> matchSchemaToValue sch el `seq` acc) (Constr (toInteger ix) els) . zip els $ constituents !! ix
matchSchemaToValue sch val = error $ "absurd: matchSchemaToValue invalid schema/value combination\nSchema: " ++ show sch ++ "\nValue: " ++ show val

getSchemaOrErr :: forall a. (HasBlueprintDefinition a, DefinitionsFor (UnrollAll '[a])) => SchemaDescription
getSchemaOrErr = runIdentity $ do
  defMap <- deriveSchemaDescriptions @'[a] errF
  let schemaRef = definitionRef @a @(UnrollAll '[a])
      schemaDescr = fromMaybe (errF schemaRef) $ descriptionFromPlutus schemaRef
  case normalizeSchemaDescription defMap schemaDescr of
    Left missing -> error $ "absurd: Schema definition not found in generated map: " ++ show missing
    Right x -> pure x
  where
    errF sch = error $ "absurd: Unsupported schema: " ++ show sch ++ "\n\nThis should have been caught during script reading"
