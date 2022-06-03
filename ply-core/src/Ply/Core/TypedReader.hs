module Ply.Core.TypedReader (readTypedScript, mkTypedScript) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)

import Plutus.V1.Ledger.Scripts (Script (Script))

import Ply.Core.Deserialize (readEnvelope)
import Ply.Core.Types (
  ScriptReaderException (ScriptRoleError, ScriptTypeError),
  ScriptRole (MintingPolicyScript, ValidatorScript),
  TypedScript (TypedScript),
  TypedScriptEnvelope (TypedScriptEnvelope),
  Typename,
  typeName,
 )

class TypedReader r params where
  mkTypedScript :: TypedScriptEnvelope -> Either ScriptReaderException (TypedScript r params)

readTypedScript :: TypedReader r params => FilePath -> IO (TypedScript r params)
readTypedScript p = readEnvelope p >>= either throwIO pure . mkTypedScript

instance MkTypenames params => TypedReader 'ValidatorScript params where
  mkTypedScript (TypedScriptEnvelope _ rol params _ (Script prog)) = do
    unless (rol == ValidatorScript) . Left $ ScriptRoleError ValidatorScript rol
    let expectedParams = mkTypenames $ Proxy @params
    unless (expectedParams == params) . Left $ ScriptTypeError expectedParams params
    pure $ TypedScript prog

instance MkTypenames params => TypedReader 'MintingPolicyScript params where
  mkTypedScript (TypedScriptEnvelope _ rol params _ (Script prog)) = do
    unless (rol == MintingPolicyScript) . Left $ ScriptRoleError MintingPolicyScript rol
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
