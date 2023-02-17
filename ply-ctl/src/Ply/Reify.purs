module Ply.Reify (
  class ReifyParams,
  reifyParams,
  class ReifyRole,
  reifyRole,
  reifyTypedScript
  ) where

import Prelude

import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Either (Either(..))
import Data.Array ((:))
import Ply.Typename (class PlyTypeName, plyTypeName)
import Type.Proxy (Proxy (..))
import Ply.Types
  ( ScriptRole (..)
  , MintingPolicyRole
  , ValidatorRole
  , TypedScriptEnvelope (..)
  , TypedScript (..)
  , PlyError (..)
  )
import Ply.TypeList (TyList, Cons, Nil)

class ReifyParams :: TyList Type -> Constraint
class ReifyParams params where
  reifyParams :: Proxy params -> Array String

instance (ReifyParams rest, PlyTypeName param) => ReifyParams (Cons param rest) where
  reifyParams _ =
    plyTypeName (Proxy :: Proxy param) : reifyParams (Proxy :: Proxy rest)

instance ReifyParams Nil where
  reifyParams _ = mempty

class ReifyRole :: ScriptRole -> Constraint
class ReifyRole role where
  reifyRole :: Proxy role -> ScriptRole

instance ReifyRole ValidatorRole where
  reifyRole _ = ValidatorRole

instance ReifyRole MintingPolicyRole where
  reifyRole _ = MintingPolicyRole

reifyTypedScript
  :: forall (role :: ScriptRole) (params :: TyList Type)
   . ReifyRole role
  => ReifyParams params
  => TypedScriptEnvelope
  -> TypedScript role params
reifyTypedScript (TypedScriptEnvelope tse) = TypedScriptConstr $ do
  let expRole = reifyRole (Proxy :: Proxy role)
      expParams = reifyParams (Proxy :: Proxy params)

  when (expRole /= tse.role) $
    Left (RoleMismatch {expected: expRole, actual: tse.role})

  when (expParams /= tse.params) $
    Left (ParamsMismatch {expected: expParams, actual: tse.params})

  pure $ tse.script
