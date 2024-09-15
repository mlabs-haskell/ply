{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.TypedReader (TypedReader, readTypedScript, mkTypedScript, typedScriptToEnvelope) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)

import Ply.Core.Deserialize (readEnvelope)
import Ply.Core.Internal.Reify
import Ply.Core.Types (
  ScriptReaderException (ScriptRoleError, ScriptTypeError),
  TypedScript (TypedScriptConstr),
  TypedScriptEnvelope (TypedScriptEnvelope, tsDescription, tsParamTypes, tsRole, tsScript, tsVersion),
 )
import Ply.Core.Unsafe (unsafeUnTypedScript)

{- | Class of 'TypedScript' parameters that are supported and can be read.

See: 'mkTypedScript'.
-}
type TypedReader rl params =
  ( ReifyRole rl
  , ReifyTypenames params
  )

-- | Pure function to parse a 'TypedScript' from a 'TypedScriptEnvelope'.
mkTypedScript ::
  forall rl params.
  TypedReader rl params =>
  TypedScriptEnvelope ->
  Either ScriptReaderException (TypedScript rl params)
mkTypedScript (TypedScriptEnvelope ver rol params _ prog) = do
  let expectedRole = reifyRole $ Proxy @rl
  unless (rol == expectedRole) . Left $ ScriptRoleError expectedRole rol
  let expectedParams = reifyTypenames $ Proxy @params
  unless (expectedParams == params) . Left $ ScriptTypeError expectedParams params
  pure $ TypedScriptConstr ver prog

{- | Read and verify a 'TypedScript' from given filepath.

The user is responsible for choosing the "correct" 'ScriptRole' and script parameters, probably
with type applications. The reader will then use this information to parse the file and verify
the serialized script has the correct role and type.
-}
readTypedScript ::
  TypedReader r params =>
  -- | File path where the typed script file is located.
  FilePath ->
  IO (TypedScript r params)
readTypedScript p = readEnvelope p >>= either throwIO pure . mkTypedScript

-- | Converting a 'TypedScript' into a 'TypedScriptEnvelope', given the description and the script.
typedScriptToEnvelope ::
  forall rl params.
  TypedReader rl params =>
  Text ->
  TypedScript rl params ->
  TypedScriptEnvelope
typedScriptToEnvelope descr (unsafeUnTypedScript -> (# ver, scrpt #)) =
  TypedScriptEnvelope
    { tsVersion = ver
    , tsRole = reifyRole $ Proxy @rl
    , tsParamTypes = reifyTypenames $ Proxy @params
    , tsDescription = descr
    , tsScript = scrpt
    }
