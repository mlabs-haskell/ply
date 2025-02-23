{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Internal.Reify (ReifyVersion (reifyVersion), ReifyRole (reifyRole), ReifySchemas (reifySchemas)) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))

import Ply.Core.Class (PlyArg (UPLCDataSchema), ToDataConstraint)
import Ply.Core.Schema.Description (
  HasDataSchemaDescription,
  schemaDescrOf',
 )
import Ply.Core.Schema.Types (PlySchema (PlyD))
import Ply.Core.Types (
  SchemaDescription,
  ScriptRole (MintingPolicyRole, ValidatorRole),
  ScriptVersion (ScriptV1, ScriptV2),
 )

type ReifyVersion :: ScriptVersion -> Constraint
class ReifyVersion s where
  reifyVersion :: Proxy s -> ScriptVersion

type ReifyRole :: ScriptRole -> Constraint
class ReifyRole s where
  reifyRole :: Proxy s -> ScriptRole

type ReifySchemas :: [Type] -> Constraint
class ReifySchemas ts where
  reifySchemas :: Proxy ts -> [SchemaDescription]

instance ReifyVersion ScriptV1 where
  reifyVersion _ = ScriptV1

instance ReifyVersion ScriptV2 where
  reifyVersion _ = ScriptV2

instance ReifyRole ValidatorRole where
  reifyRole _ = ValidatorRole

instance ReifyRole MintingPolicyRole where
  reifyRole _ = MintingPolicyRole

instance ReifySchemas '[] where
  reifySchemas _ = []

instance (PlyArg x, ToDataConstraint x, HasDataSchemaDescription (UPLCDataSchema x), ReifySchemas xs) => ReifySchemas (x : xs) where
  reifySchemas _ = schemaDescrOf' @(PlyD (UPLCDataSchema x)) : reifySchemas (Proxy @xs)
