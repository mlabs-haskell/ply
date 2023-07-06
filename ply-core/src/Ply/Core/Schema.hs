{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ply.Core.Schema (
  PlySchema (..),
  PlyDataSchema (..),
  SchemaDescription (..),
  HasSchemaInstance (..),
  DataSchemaInstanceOf (..),
  ToPLCDefaultUni (..),
  wrapDataInt,
  wrapDataByteStr,
  wrapDataList,
  wrapDataSum,
  schemaDescrOf,
) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Kind (Constraint, Type)
import Data.Text (Text)

import Data.SOP (All, All2, Compose, NS (..), Proxy (Proxy), SOP (..))
import Data.SOP.BasicFunctors (K (K))
import Data.SOP.NP (NP, cmap_NP, collapse_NP)
import Data.SOP.NS (ccata_NS, index_NS)

import qualified PlutusCore as PLC
import qualified PlutusCore.Data as PlutusData
import qualified PlutusLedgerApi.V1 as PlutusTx
import qualified PlutusTx.IsData as PlutusData

data PlySchema
  = PlyInt
  | PlyByteStr
  | PlyStr
  | PlyUnit
  | PlyBool
  | PlyListOf PlySchema
  | PlyPairOf PlySchema PlySchema
  | PlyD PlyDataSchema

data PlyDataSchema
  = PlyDS [[PlyDataSchema]]
  | PlyDM PlyDataSchema PlyDataSchema
  | PlyDL PlyDataSchema
  | PlyDI
  | PlyDB

data SchemaDescription
  = SimpleType String
  | ListType SchemaDescription
  | PairType SchemaDescription SchemaDescription
  | DataListType SchemaDescription
  | MapType SchemaDescription SchemaDescription
  | ConstrType [[SchemaDescription]]

schemaDescrOf :: PlySchema -> SchemaDescription
schemaDescrOf PlyInt = SimpleType "#integer"
schemaDescrOf PlyByteStr = SimpleType "#bytes"
schemaDescrOf PlyStr = SimpleType "#string"
schemaDescrOf PlyUnit = SimpleType "#unit"
schemaDescrOf PlyBool = SimpleType "#boolean"
schemaDescrOf (PlyListOf a) = ListType $ schemaDescrOf a
schemaDescrOf (PlyPairOf a b) = PairType (schemaDescrOf a) (schemaDescrOf b)
schemaDescrOf (PlyD (PlyDS xss)) = ConstrType $ map (schemaDescrOf . PlyD) <$> xss
schemaDescrOf (PlyD (PlyDM a b)) = MapType (schemaDescrOf $ PlyD a) (schemaDescrOf $ PlyD b)
schemaDescrOf (PlyD (PlyDL a)) = DataListType $ schemaDescrOf (PlyD a)
schemaDescrOf (PlyD PlyDI) = SimpleType "integer"
schemaDescrOf (PlyD PlyDB) = SimpleType "bytes"

newtype DataInt = DataInt Integer

newtype DataByteStr = DataByteStr ByteString

type DataList :: PlyDataSchema -> Type
newtype DataList a = DataList [DataSchemaInstanceOf a]

type DataSum :: [[PlyDataSchema]] -> Type
newtype DataSum xss = DataSum (SOP DataSchemaInstanceOf xss)

data DataSchemaInstanceOf a = DataSchemaInstanceOf {getDataSchemaInstanceOf :: SchemaInstanceOf (PlyD a)}

instance (ToPLCDefaultUni (PlyD a), DefaultUniTarget (PlyD a) ~ PlutusData.Data) => PlutusData.ToData (DataSchemaInstanceOf a) where
  toBuiltinData = PlutusTx.BuiltinData . toPLCDefaultUni . getDataSchemaInstanceOf

wrapDataInt :: Integer -> DataSchemaInstanceOf 'PlyDI
wrapDataInt = DataSchemaInstanceOf . DataInt

wrapDataByteStr :: ByteString -> DataSchemaInstanceOf 'PlyDB
wrapDataByteStr = DataSchemaInstanceOf . DataByteStr

wrapDataList :: [DataSchemaInstanceOf a] -> DataSchemaInstanceOf ( 'PlyDL a)
wrapDataList = DataSchemaInstanceOf . DataList

wrapDataSum :: NS (NP DataSchemaInstanceOf) xss -> DataSchemaInstanceOf ( 'PlyDS xss)
wrapDataSum = DataSchemaInstanceOf . DataSum . SOP

type HasSchemaInstance :: PlySchema -> Constraint
class HasSchemaInstance a where
  type SchemaInstanceOf a = (f :: Type) | f -> a

instance HasSchemaInstance PlyInt where
  type SchemaInstanceOf PlyInt = Integer

instance HasSchemaInstance PlyByteStr where
  type SchemaInstanceOf PlyByteStr = ByteString

instance HasSchemaInstance PlyStr where
  type SchemaInstanceOf PlyStr = Text

instance HasSchemaInstance PlyUnit where
  type SchemaInstanceOf PlyUnit = ()

instance HasSchemaInstance PlyBool where
  type SchemaInstanceOf PlyBool = Bool

instance HasSchemaInstance a => HasSchemaInstance (PlyListOf a) where
  type SchemaInstanceOf (PlyListOf a) = [SchemaInstanceOf a]

instance (HasSchemaInstance a, HasSchemaInstance b) => HasSchemaInstance (PlyPairOf a b) where
  type SchemaInstanceOf (PlyPairOf a b) = (SchemaInstanceOf a, SchemaInstanceOf b)

instance HasSchemaInstance (PlyD PlyDI) where
  type SchemaInstanceOf (PlyD PlyDI) = DataInt

instance HasSchemaInstance (PlyD PlyDB) where
  type SchemaInstanceOf (PlyD PlyDB) = DataByteStr

instance HasSchemaInstance (PlyD a) => HasSchemaInstance (PlyD (PlyDL a)) where
  type SchemaInstanceOf (PlyD (PlyDL a)) = DataList a

instance All2 (Compose HasSchemaInstance PlyD) a => HasSchemaInstance (PlyD (PlyDS a)) where
  type SchemaInstanceOf (PlyD (PlyDS a)) = DataSum a

type ToPLCDefaultUni :: PlySchema -> Constraint
class PLC.DefaultUni `PLC.HasTermLevel` DefaultUniTarget a => ToPLCDefaultUni a where
  type DefaultUniTarget a :: Type
  type DefaultUniTarget a = SchemaInstanceOf a
  toPLCDefaultUni :: SchemaInstanceOf a -> DefaultUniTarget a
  default toPLCDefaultUni :: SchemaInstanceOf a ~ DefaultUniTarget a => SchemaInstanceOf a -> DefaultUniTarget a
  toPLCDefaultUni = id

instance ToPLCDefaultUni PlyInt
instance ToPLCDefaultUni PlyByteStr
instance ToPLCDefaultUni PlyStr
instance ToPLCDefaultUni PlyUnit
instance ToPLCDefaultUni PlyBool

instance ToPLCDefaultUni a => ToPLCDefaultUni (PlyListOf a) where
  type DefaultUniTarget (PlyListOf a) = [DefaultUniTarget a]
  toPLCDefaultUni = map toPLCDefaultUni

instance (ToPLCDefaultUni a, ToPLCDefaultUni b) => ToPLCDefaultUni (PlyPairOf a b) where
  type DefaultUniTarget (PlyPairOf a b) = (DefaultUniTarget a, DefaultUniTarget b)
  toPLCDefaultUni = bimap toPLCDefaultUni toPLCDefaultUni

instance ToPLCDefaultUni (PlyD PlyDI) where
  type DefaultUniTarget (PlyD PlyDI) = PlutusData.Data
  toPLCDefaultUni (DataInt x) = PlutusData.I x

instance ToPLCDefaultUni (PlyD PlyDB) where
  type DefaultUniTarget (PlyD PlyDB) = PlutusData.Data
  toPLCDefaultUni (DataByteStr x) = PlutusData.B x

instance (ToPLCDefaultUni (PlyD a), DefaultUniTarget (PlyD a) ~ PlutusData.Data) => ToPLCDefaultUni (PlyD (PlyDL a)) where
  type DefaultUniTarget (PlyD (PlyDL a)) = PlutusData.Data
  toPLCDefaultUni (DataList x) = PlutusData.List $ map (toPLCDefaultUni . getDataSchemaInstanceOf) x

instance All2 (Compose PlutusData.ToData DataSchemaInstanceOf) xss => ToPLCDefaultUni (PlyD (PlyDS xss)) where
  type DefaultUniTarget (PlyD (PlyDS xss)) = PlutusData.Data
  toPLCDefaultUni (DataSum (SOP nsnp)) = PlutusData.Constr (toInteger $ index_NS nsnp) dataFields
    where
      K dataFields =
        ccata_NS
          (Proxy @(All (Compose PlutusData.ToData DataSchemaInstanceOf)))
          ( K
              . collapse_NP
              . cmap_NP
                (Proxy @(Compose PlutusData.ToData DataSchemaInstanceOf))
                (K . PlutusData.toData)
          )
          (\(K x) -> K x)
          nsnp
