{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ply.Core.Schema (
  PlySchema (..),
  PlyDataSchema (..),
  SchemaDescription (..),
  HasSchemaInstance (..),
  DataSchemaInstanceOf (..),
  ToPLCDefaultUni (..),
  HasSchemaDescription (..),
  HasDataSchemaDescription (..),
  normalizeSchemaDescription,
  wrapDataInt,
  wrapDataByteStr,
  wrapDataList,
  wrapDataSum,
  schemaDescrOf',
) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Kind (Constraint, Type)
import Data.Text (Text)

import Data.SOP (All, All2, Compose, Proxy (Proxy), SOP (SOP))
import Data.SOP.BasicFunctors (K (K))
import Data.SOP.NP (cmap_NP, collapse_NP)
import Data.SOP.NS (ccata_NS, index_NS)

import qualified PlutusCore as PLC
import qualified PlutusCore.Data as PlutusData
import qualified PlutusLedgerApi.V1 as PlutusTx
import qualified PlutusTx.IsData as PlutusData

import Ply.Core.Schema.Description (
  HasDataSchemaDescription (dataSchemaDescrOf),
  HasSchemaDescription (schemaDescrOf),
  SchemaDescription (ConstrType, DataListType, ListType, MapType, PairType, SchemaRef, SimpleType),
  schemaDescrOf',
 )
import Ply.Core.Schema.Normalize (normalizeSchemaDescription)
import Ply.Core.Schema.Types (PlyDataSchema (PlyDB, PlyDI, PlyDL, PlyDM, PlyDS), PlySchema (PlyBool, PlyByteStr, PlyD, PlyInt, PlyListOf, PlyPairOf, PlyStr, PlyUnit))

newtype DataInt = DataInt Integer

newtype DataByteStr = DataByteStr ByteString

type DataList :: PlyDataSchema -> Type
newtype DataList a = DataList [DataSchemaInstanceOf a]

type DataMap :: PlyDataSchema -> PlyDataSchema -> Type
newtype DataMap k v = DataMap [(DataSchemaInstanceOf k, DataSchemaInstanceOf v)]

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

wrapDataSum :: SOP DataSchemaInstanceOf xss -> DataSchemaInstanceOf ( 'PlyDS xss)
wrapDataSum = DataSchemaInstanceOf . DataSum

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

instance (HasSchemaInstance (PlyD k), HasSchemaInstance (PlyD v)) => HasSchemaInstance (PlyD (PlyDM k v)) where
  type SchemaInstanceOf (PlyD (PlyDM k v)) = DataMap k v

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

instance (ToPLCDefaultUni (PlyD k), DefaultUniTarget (PlyD k) ~ PlutusData.Data, ToPLCDefaultUni (PlyD v), DefaultUniTarget (PlyD v) ~ PlutusData.Data) => ToPLCDefaultUni (PlyD (PlyDM k v)) where
  type DefaultUniTarget (PlyD (PlyDM k v)) = PlutusData.Data
  toPLCDefaultUni (DataMap kvs) = PlutusData.Map $ map (\(k, v) -> (toPLCDefaultUni $ getDataSchemaInstanceOf k, toPLCDefaultUni $ getDataSchemaInstanceOf v)) kvs

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
