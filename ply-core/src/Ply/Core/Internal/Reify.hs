{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Internal.Reify (ReifyVersion (reifyVersion), ReifyParamSchemas (reifyParamSchemas), ReifyDatumSchema (reifyDatumSchema), ReifyRedeemerSchema (reifyRedeemerSchema)) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))

import PlutusTx.Blueprint (HasBlueprintSchema (schema), PlutusVersion (PlutusV1, PlutusV2, PlutusV3), Schema)

import Ply.Core.Types (ScriptParameter (AsDatum, AsRedeemer, (:=)))

type ReifyVersion :: PlutusVersion -> Constraint
class ReifyVersion v where
  reifyVersion :: Proxy v -> PlutusVersion

instance ReifyVersion PlutusV1 where
  reifyVersion _ = PlutusV1

instance ReifyVersion PlutusV2 where
  reifyVersion _ = PlutusV2

instance ReifyVersion PlutusV3 where
  reifyVersion _ = PlutusV3

type ReifyParamSchemas :: [Type] -> [ScriptParameter] -> Constraint
class ReifyParamSchemas referencedTypes ts where
  reifyParamSchemas :: Proxy referencedTypes -> Proxy ts -> [Schema referencedTypes]

instance ReifyParamSchemas referencedTypes [AsDatum any0, AsRedeemer any1] where
  reifyParamSchemas _ _ = []

instance ReifyParamSchemas referencedTypes '[AsRedeemer any] where
  reifyParamSchemas _ _ = []

instance (HasBlueprintSchema x referencedTypes, ReifyParamSchemas referencedTypes xs) => ReifyParamSchemas referencedTypes (any := x : xs) where
  reifyParamSchemas _ _ = schema @x : reifyParamSchemas (Proxy @referencedTypes) (Proxy @xs)

type ReifyDatumSchema :: [Type] -> [ScriptParameter] -> Constraint
class ReifyDatumSchema referencedTypes ts where
  reifyDatumSchema :: Proxy referencedTypes -> Proxy ts -> Maybe (Schema referencedTypes)

instance HasBlueprintSchema x referencedTypes => ReifyDatumSchema referencedTypes [AsDatum x, AsRedeemer any] where
  reifyDatumSchema _ _ = Just $ schema @x

instance ReifyDatumSchema referencedTypes '[AsRedeemer any] where
  reifyDatumSchema _ _ = Nothing

instance ReifyDatumSchema referencedTypes xs => ReifyDatumSchema referencedTypes (any := x : xs) where
  reifyDatumSchema _ _ = reifyDatumSchema (Proxy @referencedTypes) (Proxy @xs)

type ReifyRedeemerSchema :: [Type] -> [ScriptParameter] -> Constraint
class ReifyRedeemerSchema referencedTypes ts where
  reifyRedeemerSchema :: Proxy referencedTypes -> Proxy ts -> Schema referencedTypes

instance HasBlueprintSchema x referencedTypes => ReifyRedeemerSchema referencedTypes [AsDatum any, AsRedeemer x] where
  reifyRedeemerSchema _ _ = schema @x

instance HasBlueprintSchema x referencedTypes => ReifyRedeemerSchema referencedTypes '[AsRedeemer x] where
  reifyRedeemerSchema _ _ = schema @x

instance ReifyRedeemerSchema referencedTypes xs => ReifyRedeemerSchema referencedTypes (any := x : xs) where
  reifyRedeemerSchema _ _ = reifyRedeemerSchema (Proxy @referencedTypes) (Proxy @xs)
