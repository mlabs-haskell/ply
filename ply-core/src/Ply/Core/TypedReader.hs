{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.TypedReader (TypedReader, readTypedScript, mkTypedScript) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Proxy (Proxy (Proxy))

import Ply.Core.Deserialize (readEnvelope)
import Ply.Core.Internal.Reify
import Ply.Core.Types (
  ScriptReaderException (ScriptRoleError, ScriptTypeError),
  ScriptRole (ValidatorRole),
  TypedScript (TypedScript),
  TypedScriptEnvelope (TypedScriptEnvelope),
 )
import Ply.LedgerExports.Common (deserialiseUPLC)

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
  unless (rol == reifyRole (Proxy @rl)) . Left $ ScriptRoleError ValidatorRole rol
  let expectedParams = reifyTypenames $ Proxy @params
  unless (expectedParams == params) . Left $ ScriptTypeError expectedParams params
  pure $ TypedScript ver $ deserialiseUPLC prog

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
