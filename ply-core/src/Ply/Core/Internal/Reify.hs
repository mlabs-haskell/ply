module Ply.Core.Internal.Reify (ReifyVersion (reifyVersion), ReifyRole (reifyRole), ReifyTypenames (reifyTypenames)) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Type.Reflection (Typeable)

import Ply.Core.Typename (typeName)
import Ply.Core.Types

type ReifyVersion :: ScriptVersion -> Constraint
class ReifyVersion s where
  reifyVersion :: Proxy s -> ScriptVersion

type ReifyRole :: ScriptRole -> Constraint
class ReifyRole s where
  reifyRole :: Proxy s -> ScriptRole

type ReifyTypenames :: [Type] -> Constraint
class ReifyTypenames ts where
  reifyTypenames :: Proxy ts -> [Typename]

instance ReifyVersion ScriptV1 where
  reifyVersion _ = ScriptV1

instance ReifyVersion ScriptV2 where
  reifyVersion _ = ScriptV2

instance ReifyRole ValidatorRole where
  reifyRole _ = ValidatorRole

instance ReifyRole MintingPolicyRole where
  reifyRole _ = MintingPolicyRole

instance ReifyTypenames '[] where
  reifyTypenames _ = []

instance (Typeable x, ReifyTypenames xs) => ReifyTypenames (x : xs) where
  reifyTypenames _ = typeName @x : reifyTypenames (Proxy @xs)
