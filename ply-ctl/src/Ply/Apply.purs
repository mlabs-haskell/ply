module Ply.Apply (applyParam, (#)) where

import Prelude

import Data.Either (Either (..))
import Contract.PlutusData (class ToData, toData)
import Contract.Scripts (applyArgs, ApplyArgsError)
import Ply.Types (ScriptRole, TypedScript(..), PlyError (..))
import Ply.TypeList (TyList, Cons)

-- Does not give any optimization as CTL only provides `applyArgs`
applyParam
  :: forall (role :: ScriptRole) (param :: Type) (paramRest :: TyList Type)
   . ToData param
  => TypedScript role (Cons param paramRest)
  -> param
  -> TypedScript role paramRest
applyParam (TypedScriptConstr script') p = TypedScriptConstr $ do
  script <- script'
  case applyArgs script [(toData p)] of
    Right applied -> Right applied
    Left err -> Left $ ApplicationError err

infixl 8 applyParam as #
