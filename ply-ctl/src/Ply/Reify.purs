module Ply.Reify where

import Prelude

import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Either
import Data.Array ((:))
import Ply.Typename
import Type.Proxy
import Ply.Types
import Ply.TypeList

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

data ReificationError
  = RoleMismatch {expected :: ScriptRole, actual :: ScriptRole}
  | ParamsMismatch {expected :: Array String, actual :: Array String}

derive instance Generic ReificationError _
instance Show ReificationError where show = genericShow

reifyTypedScript
  :: forall (role :: ScriptRole) (params :: TyList Type)
   . ReifyRole role
  => ReifyParams params
  => TypedScriptEnvelope
  -> Either ReificationError (TypedScript role params)
reifyTypedScript (TypedScriptEnvelope tse) = do
  let expRole = reifyRole (Proxy :: Proxy role)
      expParams = reifyParams (Proxy :: Proxy params)

  when (expRole /= tse.role) $
    Left (RoleMismatch {expected: expRole, actual: tse.role})

  when (expParams /= tse.params) $
    Left (ParamsMismatch {expected: expParams, actual: tse.params})

  pure $ TypedScriptConstr tse.version tse.script
