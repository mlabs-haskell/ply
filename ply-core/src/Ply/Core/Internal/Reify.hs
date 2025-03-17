{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Internal.Reify (ReifyVersion (reifyVersion), ReifySchemas (reifySchemas)) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))

import PlutusTx.Blueprint (HasBlueprintSchema (schema), PlutusVersion (PlutusV1, PlutusV2, PlutusV3), Schema)

type ReifyVersion :: PlutusVersion -> Constraint
class ReifyVersion v where
  reifyVersion :: Proxy v -> PlutusVersion

instance ReifyVersion PlutusV1 where
  reifyVersion _ = PlutusV1

instance ReifyVersion PlutusV2 where
  reifyVersion _ = PlutusV2

instance ReifyVersion PlutusV3 where
  reifyVersion _ = PlutusV3

type ReifySchemas :: [Type] -> [Type] -> Constraint
class ReifySchemas referencedTypes ts where
  reifySchemas :: Proxy referencedTypes -> Proxy ts -> [Schema referencedTypes]

instance ReifySchemas referencedTypes '[] where
  reifySchemas _ _ = []

instance (HasBlueprintSchema x referencedTypes, ReifySchemas referencedTypes xs) => ReifySchemas referencedTypes (x : xs) where
  reifySchemas _ _ = schema @x : reifySchemas (Proxy @referencedTypes) (Proxy @xs)
