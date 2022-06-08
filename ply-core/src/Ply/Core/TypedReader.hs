{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.TypedReader (TypedReader, readTypedScript, mkTypedScript) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)

import Plutus.V1.Ledger.Scripts (Script (Script))

import Ply.Core.Deserialize (readEnvelope)
import Ply.Core.Types (
  ScriptReaderException (ScriptRoleError, ScriptTypeError),
  ScriptRole (MintingPolicyRole, ValidatorRole),
  TypedScript (TypedScript),
  TypedScriptEnvelope (TypedScriptEnvelope),
  Typename,
  typeName,
 )

class TypedReader_ r params where
  mkTypedScript :: TypedScriptEnvelope -> Either ScriptReaderException (TypedScript r params)

class TypedReader_ r params => TypedReader r params
instance TypedReader_ r params => TypedReader r params

{- | Read and verify a 'TypedScript' from given filepath.

The user is responsible for choosing the "correct" 'ScriptRole' and script parameters, probably
with type applications. The reader will then use this information to parse the file and verify
the serialized script has the correct role and type.
-}
readTypedScript :: TypedReader r params => FilePath -> IO (TypedScript r params)
readTypedScript p = readEnvelope p >>= either throwIO pure . mkTypedScript

instance MkTypenames params => TypedReader_ 'ValidatorRole params where
  mkTypedScript (TypedScriptEnvelope _ rol params _ (Script prog)) = do
    unless (rol == ValidatorRole) . Left $ ScriptRoleError ValidatorRole rol
    let expectedParams = mkTypenames $ Proxy @params
    unless (expectedParams == params) . Left $ ScriptTypeError expectedParams params
    pure $ TypedScript prog

instance MkTypenames params => TypedReader_ 'MintingPolicyRole params where
  mkTypedScript (TypedScriptEnvelope _ rol params _ (Script prog)) = do
    unless (rol == MintingPolicyRole) . Left $ ScriptRoleError MintingPolicyRole rol
    let expectedParams = mkTypenames $ Proxy @params
    unless (expectedParams == params) . Left $ ScriptTypeError expectedParams params
    pure $ TypedScript prog

type MkTypenames :: [Type] -> Constraint
class MkTypenames a where
  mkTypenames :: Proxy a -> [Typename]

instance MkTypenames '[] where
  mkTypenames _ = []

instance (Typeable x, MkTypenames xs) => MkTypenames (x ': xs) where
  mkTypenames _ = typeName @x : mkTypenames (Proxy @xs)
