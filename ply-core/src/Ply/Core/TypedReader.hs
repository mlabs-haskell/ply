{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.TypedReader (
  TypedReader,
  readTypedScript,
  readTypedScriptWith,
) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)

import Ply.Core.Deserialize (readEnvelope)
import Ply.Core.Internal.Reify
import Ply.Core.Types (
  ScriptReaderException (ScriptRoleError, ScriptTypeError),
  ScriptRole (ValidatorRole),
  TypedScript (TypedScriptConstr),
  TypedScriptEnvelope (TypedScriptEnvelope),
  Typename (Typename),
 )

{- | Class of 'TypedScript' parameters that are supported and can be read.

See: 'mkTypedScript'.
-}
type TypedReader rl params =
  ( ReifyRole rl
  , ReifyTypenames params
  )

mkTypedScriptWith ::
  forall rl params.
  TypedReader rl params =>
  (Text -> Text) ->
  TypedScriptEnvelope ->
  Either ScriptReaderException (TypedScript rl params)
mkTypedScriptWith mapping (TypedScriptEnvelope ver rol params _ prog) = do
  unless (rol == reifyRole (Proxy @rl)) . Left $ ScriptRoleError ValidatorRole rol
  let expectedParams = reifyTypenames $ Proxy @params
  let actualParams = coerce mapping <$> params
  unless (expectedParams == actualParams) . Left $ ScriptTypeError expectedParams actualParams
  pure $ TypedScriptConstr ver prog

{- | Read and verify a 'TypedScript' from given filepath. If you need to adjust type names use
'readTypedScriptWith' instead.

The user is responsible for choosing the "correct" 'ScriptRole' and script parameters, probably
with type applications. The reader will then use this information to parse the file and verify
the serialized script has the correct role and type.
-}
readTypedScript ::
  TypedReader r params =>
  -- | File path where the typed script file is located.
  FilePath ->
  IO (TypedScript r params)
readTypedScript = readTypedScriptWith id

{- | Like 'readTypedScript', but takes a mapper @(Text -> Text) @ for type names of a script
parameters. It might be useful if your script have been serialized with an older version of
@ply@, which would write type constructor name, or using old-style ledger types. Mapping
applies to @Typename@'s coming in the envelop.
-}
readTypedScriptWith ::
  TypedReader r params =>
  -- | Mapping for type names of script parameters.
  (Text -> Text) ->
  -- | File path where the typed script file is located.
  FilePath ->
  IO (TypedScript r params)
readTypedScriptWith mapping p = readEnvelope p >>= either throwIO pure . mkTypedScriptWith mapping
