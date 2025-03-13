{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ply.Core.Schema (
  PlySchema (..),
  PlyDataSchema (..),
  SchemaDescription (..),
  descriptionFromPlutus,
  deriveSchemaDescriptions,
  normalizeSchemaDescription,
) where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import PlutusTx.Blueprint (
  Definition (MkDefinition),
  DefinitionId (definitionIdToText),
  Definitions (AddDefinition, NoDefinitions),
  DefinitionsFor,
  Schema,
  UnrollAll,
  deriveDefinitions,
 )

import Ply.Core.Schema.Description (
  SchemaDescription (ConstrType, DataListType, ListType, MapType, PairType, SchemaRef, SimpleType),
  descriptionFromPlutus,
 )
import Ply.Core.Schema.Normalize (normalizeSchemaDescription)
import Ply.Core.Schema.Types (PlyDataSchema (PlyDB, PlyDI, PlyDL, PlyDM, PlyDS), PlySchema (PlyBool, PlyByteStr, PlyD, PlyInt, PlyListOf, PlyPairOf, PlyStr, PlyUnit))

-- | Get a map with the definitions for the given types. Takes an erring function to handle description conversion errors.
deriveSchemaDescriptions :: forall (params :: [Type]) m. (DefinitionsFor (UnrollAll params), Monad m) => (forall x ts. Schema ts -> m x) -> m (Map Text SchemaDescription)
deriveSchemaDescriptions errF = sequence . definitionsToMap' (\sch -> maybe (errF sch) pure $ descriptionFromPlutus sch) $ deriveDefinitions @params

-- Modified version of upstream 'definitionsToMap' because that version forces higher ranked type even though Schema shouldn't need it.
definitionsToMap' :: (Schema ts -> v) -> Definitions ts -> Map Text v
definitionsToMap' _k NoDefinitions = Map.empty
definitionsToMap' k (AddDefinition (MkDefinition defId v) s) =
  Map.insert (definitionIdToText defId) (k v) (definitionsToMap' k s)
