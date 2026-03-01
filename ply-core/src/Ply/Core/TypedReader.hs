{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.TypedReader (TypedReader, getTypedScript) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Data.Foldable (find, for_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Txt

import PlutusTx.Blueprint (
  DefinitionsFor,
  PlutusVersion (PlutusV1, PlutusV2, PlutusV3),
  UnrollAll,
 )

import Ply.Core.Internal.Reify (
  ReifyDatumSchema (reifyDatumSchema),
  ReifyParamSchemas (reifyParamSchemas),
  ReifyRedeemerSchema (reifyRedeemerSchema),
  ReifyVersion (reifyVersion),
 )
import Ply.Core.Schema (SchemaDescription, deriveSchemaDescriptions, normalizeSchemaDescription)
import Ply.Core.Schema.Description (descriptionFromPlutus)
import Ply.Core.Types (
  PlutusVersionJSON (PlutusVersionJSON),
  ScriptParameter (AsDatum, AsRedeemer, (:=)),
  ScriptReaderException (ScriptVerificationException, exceptionDetail, scriptTitle),
  ScriptSchemaError (
    MissingDatum,
    ParameterLengthMismatch,
    ScriptTypeError,
    ScriptVersionError,
    UndefinedReference,
    UnexpectedDatum,
    UnsupportedSchema,
    definitionsMap,
    referenceName,
    targetSchema
  ),
  ScriptTypeKind (ScriptDatum, ScriptParameter, ScriptRedeemer),
  TypedBlueprint (TypedBlueprint, tbDefinitions, tbPreamble, tbValidators),
  TypedBlueprintPreamble (TypedBlueprintPreamble, tbpPlutusVersion),
  TypedScript (TypedScriptConstr),
  TypedScriptBlueprint (TypedScriptBlueprint, tsbCompiledCode, tsbDatum, tsbParameters, tsbRedeemer, tsbTitle),
  TypedScriptBlueprintParameter (TypedScriptBlueprintParameter),
  UPLCProgramJSON (UPLCProgramJSON),
 )

{- | Class of 'TypedScript' parameters that are supported and can be read.

See: 'getTypedScript'.
-}
type TypedReader r params =
  ( ReifyVersion r
  , ReifyParamSchemas (UnrollAll (UnwrapParameters params)) params
  , ReifyDatumSchema (UnrollAll (UnwrapParameters params)) params
  , ReifyRedeemerSchema (UnrollAll (UnwrapParameters params)) params
  , DefinitionsFor (UnrollAll (UnwrapParameters params))
  )

type UnwrapParameters :: [ScriptParameter] -> [Type]
type family UnwrapParameters ps where
  UnwrapParameters '[] = '[]
  UnwrapParameters (_ := x : xs) = x : UnwrapParameters xs
  UnwrapParameters (AsDatum x : xs) = x : UnwrapParameters xs
  UnwrapParameters (AsRedeemer x : xs) = x : UnwrapParameters xs

-- | Pure function to parse a 'TypedScript' from a 'TypedScriptBlueprint'.
mkTypedScript ::
  forall r params.
  TypedReader r params =>
  Map Text SchemaDescription ->
  PlutusVersion ->
  TypedScriptBlueprint ->
  Either ScriptReaderException (TypedScript r params)
mkTypedScript
  refMap
  ver
  TypedScriptBlueprint
    { tsbTitle
    , tsbDatum
    , tsbRedeemer
    , tsbParameters
    , tsbCompiledCode = UPLCProgramJSON script
    } = runExcept $ do
    let expectedVersion = reifyVersion $ Proxy @r
    unless (expectedVersion `eqVersion` ver)
      . throwE'
      $ ScriptVersionError expectedVersion ver
    let expectedPlutusParams = reifyParamSchemas (Proxy @(UnrollAll (UnwrapParameters params))) $ Proxy @params
        expectedPlutusDatum = reifyDatumSchema (Proxy @(UnrollAll (UnwrapParameters params))) $ Proxy @params
        expectedPlutusRedeemer = reifyRedeemerSchema (Proxy @(UnrollAll (UnwrapParameters params))) $ Proxy @params
    expectedParams <- traverse descriptionFromPlutus' expectedPlutusParams
    expectedPlutusDatum <- traverse descriptionFromPlutus' expectedPlutusDatum
    expectedPlutusRedeemer <- descriptionFromPlutus' expectedPlutusRedeemer
    expectedDefinitionsMap <- deriveSchemaDescriptions @(UnwrapParameters params) (throwE' . UnsupportedSchema)

    -- Ensure that the extra parameters are as expected.
    let expectedLength = length expectedParams
        actualLength = length tsbParameters
    unless (expectedLength == actualLength)
      . throwE'
      $ ParameterLengthMismatch expectedLength actualLength
    for_ (zip expectedParams tsbParameters) . uncurry $ assertExpectedType ScriptParameter expectedDefinitionsMap
    -- Ensure that the datum (if any) is as expected.
    case (expectedPlutusDatum, tsbDatum) of
      (Nothing, Nothing) -> pure ()
      (Just expected, Just actual) -> assertExpectedType ScriptDatum expectedDefinitionsMap expected actual
      (Just expected, Nothing) -> throwE' $ MissingDatum expected
      (Nothing, Just (TypedScriptBlueprintParameter actual)) -> throwE' $ UnexpectedDatum actual
    -- Ensure that the redeemer is as expected.
    assertExpectedType ScriptRedeemer expectedDefinitionsMap expectedPlutusRedeemer tsbRedeemer

    pure $ TypedScriptConstr script
    where
      assertExpectedType typeKind expectedDefinitionsMap expectedParam (TypedScriptBlueprintParameter param) = do
        normalizedExpected <- normalizeOrThrow expectedDefinitionsMap expectedParam
        normalizedActual <- normalizeOrThrow refMap param
        unless (normalizedExpected == normalizedActual)
          . throwE'
          $ ScriptTypeError typeKind normalizedExpected normalizedActual
      throwUndefRef m sch refName = throwE' $ UndefinedReference {definitionsMap = m, referenceName = refName, targetSchema = sch}
      normalizeOrThrow :: Map Text SchemaDescription -> SchemaDescription -> Except ScriptReaderException SchemaDescription
      normalizeOrThrow m sch = either (throwUndefRef m sch) pure $ normalizeSchemaDescription m sch
      descriptionFromPlutus' plutusSchema = do
        case descriptionFromPlutus plutusSchema of
          Nothing -> throwE' $ UnsupportedSchema plutusSchema
          Just x -> pure x
      eqVersion PlutusV1 PlutusV1 = True
      eqVersion PlutusV2 PlutusV2 = True
      eqVersion PlutusV3 PlutusV3 = True
      eqVersion _ _ = False
      -- Helper to throw 'ScriptSchemaError's for this specific script.
      throwE' :: ScriptSchemaError -> Except ScriptReaderException a
      throwE' err =
        throwE
          ScriptVerificationException
            { scriptTitle = Txt.unpack tsbTitle
            , exceptionDetail = err
            }

{- | Verify and then obtain the 'TypedScript' with the corresponding title from given 'TypedBlueprint'.

The user is responsible for choosing the "correct" script parameters, probably
with type applications. The reader will then use this information to parse the file and verify
the serialized script has the type.

Throws 'ScriptReaderException' during script parsing. Throws a user error if a validator with given title is not found.
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
