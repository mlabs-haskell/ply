module Ply.Apply where

import Prelude

import Data.Either
import Contract.PlutusData
import Contract.Scripts
import Ply.Types
import Ply.TypeList

-- Does not give any optimization as CTL only provides `applyArgs`
applyParam
  :: forall (role :: ScriptRole) (param :: Type) (paramRest :: TyList Type)
   . ToData param
  => TypedScript role (Cons param paramRest)
  -> param
  -> Either ApplyArgsError (TypedScript role paramRest)
applyParam (TypedScriptConstr script) p = do
  applied <- applyArgs script [(toData p)]
  pure $ TypedScriptConstr applied

infixl 8 applyParam as #
