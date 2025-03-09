{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Internal.Reify (ReifyRole (reifyRole), ReifySchemas (reifySchemas)) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))

import PlutusTx.Blueprint (HasBlueprintSchema (schema), Schema)
import Ply.Core.Types (
  ScriptRole (MintingPolicyRole, ValidatorRole),
 )

type ReifyRole :: ScriptRole -> Constraint
class ReifyRole s where
  reifyRole :: Proxy s -> ScriptRole

type ReifySchemas :: [Type] -> [Type] -> Constraint
class ReifySchemas referencedTypes ts where
  reifySchemas :: Proxy referencedTypes -> Proxy ts -> [Schema referencedTypes]

instance ReifyRole ValidatorRole where
  reifyRole _ = ValidatorRole

instance ReifyRole MintingPolicyRole where
  reifyRole _ = MintingPolicyRole

instance ReifySchemas referencedTypes '[] where
  reifySchemas _ _ = []

instance (HasBlueprintSchema x referencedTypes, ReifySchemas referencedTypes xs) => ReifySchemas referencedTypes (x : xs) where
  reifySchemas _ _ = schema @x : reifySchemas (Proxy @referencedTypes) (Proxy @xs)
