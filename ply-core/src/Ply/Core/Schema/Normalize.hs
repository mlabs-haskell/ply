module Ply.Core.Schema.Normalize (normalizeSchemaDescription) where

import Data.Map.Strict (Map)
import Data.Text (Text)

import qualified Data.Map as Map
import Ply.Core.Schema.Description (SchemaDescription (ConstrType, DataListType, ListType, MapType, PairType, SchemaRef, SimpleType))

{- | Resolves all references within given schema (using the given definitions map) and returns the normalized schema.

References are recursively resolved. i.e If a schema retrieved from the definitions map is also a reference, it'll be
resolved further.
-}
normalizeSchemaDescription :: Map Text SchemaDescription -> SchemaDescription -> Maybe SchemaDescription
normalizeSchemaDescription refMap (SchemaRef t) = t `Map.lookup` refMap >>= normalizeSchemaDescription refMap
normalizeSchemaDescription refMap schema = case schema of
  SimpleType t -> Just $ SimpleType t
  ListType t -> ListType <$> f t
  PairType t1 t2 -> PairType <$> f t1 <*> f t2
  DataListType t -> DataListType <$> f t
  MapType t1 t2 -> MapType <$> f t1 <*> f t2
  ConstrType ts -> ConstrType <$> ttraverse f ts
  where
    ttraverse f = traverse (traverse f)
    f = normalizeSchemaDescription refMap
