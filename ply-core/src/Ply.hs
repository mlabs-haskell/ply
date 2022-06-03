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
  toMintingPolicy,
  applyParam,
  readTypedScript,
) where

import Data.Coerce (coerce)

import Plutus.V1.Ledger.Scripts (
  MintingPolicy (MintingPolicy),
  Script (Script),
  Validator (Validator),
 )

import Ply.Core.Apply (applyParam)
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

toValidator :: TypedScript 'ValidatorScript '[] -> Validator
toValidator (TypedScript s) = coerce s

toMintingPolicy :: TypedScript 'MintingPolicyScript '[] -> MintingPolicy
toMintingPolicy (TypedScript s) = coerce s
