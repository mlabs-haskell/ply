module Ply (
  TypedScript,
  ScriptRole (..),
  ScriptReaderException (..),
  TypedScriptEnvelope (..),
  ScriptVersion (..),
  Typename,
  typeName,
  PlyArg,
  toValidator,
  toScript,
  toMintingPolicy,
  readTypedScript,
  (#),
  (#$),
) where

import Data.Coerce (coerce)

import Plutus.V1.Ledger.Scripts (
  MintingPolicy (MintingPolicy),
  Script (Script),
  Validator (Validator),
 )

import Ply.Core.Apply ((#), (#$))
import Ply.Core.Class (PlyArg)
import Ply.Core.TypedReader (readTypedScript)
import Ply.Core.Types (
  ScriptReaderException (..),
  ScriptRole (..),
  ScriptVersion (..),
  TypedScript (..),
  TypedScriptEnvelope (..),
  Typename,
  typeName,
 )

-- | Obtain a 'Validator' from a 'TypedScript'.
toValidator :: TypedScript 'ValidatorScript '[] -> Validator
toValidator (TypedScript s) = coerce s

-- | Obtain a 'MintingPolicy' from a 'TypedScript'.
toMintingPolicy :: TypedScript 'MintingPolicyScript '[] -> MintingPolicy
toMintingPolicy (TypedScript s) = coerce s

-- | Unconditionally obtain the raw untyped 'Script' from a 'TypedScript'.
toScript :: TypedScript r params -> Script
toScript (TypedScript s) = coerce s
