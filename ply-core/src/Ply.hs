{-# LANGUAGE AllowAmbiguousTypes #-}

module Ply (
  TypedScript (TypedScript, TypedScript'),
  ReifyVersion (reifyVersion),
  PlutusVersion (..),
  ScriptReaderException (..),
  TypedBlueprintPreamble (..),
  TypedBlueprint (..),
  TypedScriptBlueprint (..),
  PlyArg (toSomeBuiltinArg),
  readBlueprint,
  getTypedScript,
  getPlutusVersion,
  (#),
  (#$),
  (#!),
  (#$!),
) where

import Data.Proxy (Proxy (Proxy))

import PlutusTx.Blueprint (PlutusVersion (PlutusV1, PlutusV2, PlutusV3))

import Ply.Core.Apply ((#), (#!), (#$), (#$!))
import Ply.Core.Class (PlyArg (toSomeBuiltinArg))
import Ply.Core.Deserialize (readBlueprint)
import Ply.Core.Internal.Reify (ReifyVersion, reifyVersion)
import Ply.Core.TypedReader (getTypedScript)
import Ply.Core.Types (
  ScriptReaderException (AesonDecodeError, ScriptTypeError, UndefinedReference, UnsupportedSchema, actualType, definitionsMap, expectedType, referenceName, targetSchema),
  TypedBlueprint (TypedBlueprint, tbDefinitions, tbPreamble, tbValidators),
  TypedBlueprintPreamble (TypedBlueprintPreamble, tbpPlutusVersion),
  TypedScript (TypedScriptConstr),
  TypedScriptBlueprint (TypedScriptBlueprint, tsbCompiledCode, tsbTitle),
  UPLCProgram,
 )

-- | Extract the inner script (for scripts with datum)
pattern TypedScript :: UPLCProgram -> TypedScript r '[datum, redeemer]
pattern TypedScript s <- TypedScriptConstr s

{-# COMPLETE TypedScript #-}

-- | Extract the inner script (for scripts with no datum)
pattern TypedScript' :: UPLCProgram -> TypedScript r '[redeemer]
pattern TypedScript' s <- TypedScriptConstr s

{-# COMPLETE TypedScript' #-}

-- | Obtain the Plutus script (ledger) version associated with given 'TypedScript'.
getPlutusVersion :: forall r params. ReifyVersion r => TypedScript r params -> PlutusVersion
getPlutusVersion _ = reifyVersion $ Proxy @r
