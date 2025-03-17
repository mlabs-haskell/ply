{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.TypedReader (TypedReader, getTypedScript) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExcept, throwE)
import Data.Foldable (find, for_)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)

import PlutusTx.Blueprint (
  DefinitionsFor,
  PlutusVersion (PlutusV1, PlutusV2, PlutusV3),
  UnrollAll,
 )

import Ply.Core.Internal.Reify (ReifySchemas (reifySchemas), ReifyVersion (reifyVersion))
import Ply.Core.Schema (SchemaDescription, deriveSchemaDescriptions, normalizeSchemaDescription)
import Ply.Core.Schema.Description (descriptionFromPlutus)
import Ply.Core.Types (
  PlutusVersionJSON (PlutusVersionJSON),
  ScriptReaderException (ScriptTypeError, ScriptVersionError, UndefinedReference, UnsupportedSchema, definitionsMap, referenceName, targetSchema),
  TypedBlueprint (TypedBlueprint, tbDefinitions, tbPreamble, tbValidators),
  TypedBlueprintPreamble (TypedBlueprintPreamble, tbpPlutusVersion),
  TypedScript (TypedScriptConstr),
  TypedScriptBlueprint (TypedScriptBlueprint, tsbCompiledCode, tsbParameters, tsbTitle),
  TypedScriptBlueprintParameter (TypedScriptBlueprintParameter),
  UPLCProgramJSON (UPLCProgramJSON),
 )

{- | Class of 'TypedScript' parameters that are supported and can be read.

See: 'getTypedScript'.
-}
type TypedReader r params =
  ( ReifyVersion r
  , ReifySchemas (UnrollAll params) params
  , DefinitionsFor (UnrollAll params)
  )

-- | Pure function to parse a 'TypedScript' from a 'TypedScriptBlueprint'.
mkTypedScript ::
  forall r params.
  TypedReader r params =>
  Map Text SchemaDescription ->
  PlutusVersion ->
  TypedScriptBlueprint ->
  Either ScriptReaderException (TypedScript r params)
mkTypedScript refMap ver TypedScriptBlueprint {tsbParameters, tsbCompiledCode = UPLCProgramJSON script} = runExcept $ do
  let expectedVersion = reifyVersion $ Proxy @r
  unless (expectedVersion `eqVersion` ver)
    . throwE
    $ ScriptVersionError expectedVersion ver
  let expectedPlutusParams = reifySchemas (Proxy @(UnrollAll params)) $ Proxy @params
  expectedParams <- traverse descriptionFromPlutus' expectedPlutusParams
  expectedDefinitionsMap <- deriveSchemaDescriptions @params (throwE . UnsupportedSchema)

  for_ (zip expectedParams tsbParameters) $ \(expectedParam, TypedScriptBlueprintParameter param) -> do
    normalizedExpected <- normalizeOrThrow expectedDefinitionsMap expectedParam
    normalizedActual <- normalizeOrThrow refMap param
    unless (normalizedExpected == normalizedActual)
      . throwE
      $ ScriptTypeError normalizedExpected normalizedActual

  pure $ TypedScriptConstr script
  where
    throwUndefRef m sch refName = throwE $ UndefinedReference {definitionsMap = m, referenceName = refName, targetSchema = sch}
    normalizeOrThrow m sch = either (throwUndefRef m sch) pure $ normalizeSchemaDescription m sch
    descriptionFromPlutus' plutusSchema = do
      case descriptionFromPlutus plutusSchema of
        Nothing -> throwE $ UnsupportedSchema plutusSchema
        Just x -> pure x
    eqVersion PlutusV1 PlutusV1 = True
    eqVersion PlutusV2 PlutusV2 = True
    eqVersion PlutusV3 PlutusV3 = True
    eqVersion _ _ = False

{- | Verify and then obtain the 'TypedScript' with the corresponding title from given 'TypedBlueprint'.

The user is responsible for choosing the "correct" script parameters, probably
with type applications. The reader will then use this information to parse the file and verify
the serialized script has the type.
-}
getTypedScript ::
  forall r params.
  TypedReader r params =>
  TypedBlueprint ->
  Text ->
  IO (TypedScript r params)
getTypedScript TypedBlueprint {tbValidators, tbPreamble = TypedBlueprintPreamble {tbpPlutusVersion = PlutusVersionJSON ver}, tbDefinitions} title =
  case find (\TypedScriptBlueprint {tsbTitle} -> tsbTitle == title) tbValidators of
    -- Using 'show' here to get the title quoted.
    Nothing -> throwIO . userError $ "Validator with title " ++ show title ++ " not found in blueprint"
    Just ts -> either throwIO pure $ mkTypedScript tbDefinitions ver ts
