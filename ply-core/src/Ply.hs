module Ply (
  TypedScript,
  ScriptRole (..),
  ScriptReaderException (..),
  TypedScriptEnvelope (..),
  ScriptVersion (..),
  Typename,
  typeName,
  PlyArg,
  getPlutusVersion,
  toValidator,
  toScript,
  toMintingPolicy,
  readTypedScript,
  (#),
  (#$),
  (#!),
  (#$!),
) where

import Data.Coerce (coerce)

import Ply.LedgerExports.Common (
  MintingPolicy (MintingPolicy),
  Script (Script),
  Validator (Validator),
 )

import Ply.Core.Apply ((#), (#!), (#$), (#$!))
import Ply.Core.Class (PlyArg)
import Ply.Core.TypedReader (readTypedScript)
import Ply.Core.Typename (typeName)
import Ply.Core.Types (
  ScriptReaderException (..),
  ScriptRole (MintingPolicyRole, ValidatorRole),
  ScriptVersion (..),
  TypedScript (TypedScript),
  TypedScriptEnvelope (..),
  Typename,
 )

-- | Obtain the Plutus script (ledger) version associated with given 'TypedScript'.
getPlutusVersion :: TypedScript r params -> ScriptVersion
getPlutusVersion (TypedScript ver _) = ver

-- | Obtain a 'Validator' from a 'TypedScript'.
toValidator :: TypedScript 'ValidatorRole '[] -> Validator
toValidator (TypedScript _ s) = coerce s

-- | Obtain a 'MintingPolicy' from a 'TypedScript'.
toMintingPolicy :: TypedScript 'MintingPolicyRole '[] -> MintingPolicy
toMintingPolicy (TypedScript _ s) = coerce s

-- | Unconditionally obtain the raw untyped 'Script' from a 'TypedScript'.
toScript :: TypedScript r params -> Script
toScript (TypedScript _ s) = coerce s
