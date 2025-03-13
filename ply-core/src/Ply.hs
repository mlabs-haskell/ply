module Ply (
  TypedScript (TypedScript),
  ScriptRole (..),
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

import PlutusTx.Blueprint (PlutusVersion)

import Ply.Core.Apply ((#), (#!), (#$), (#$!))
import Ply.Core.Class (PlyArg (toSomeBuiltinArg))
import Ply.Core.Deserialize (readBlueprint)
import Ply.Core.TypedReader (getTypedScript)
import Ply.Core.Types (
  ScriptReaderException (AesonDecodeError, ScriptTypeError, UndefinedReference, UnsupportedSchema, actualType, definitionsMap, expectedType, referenceName, targetSchema),
  ScriptRole (MintingPolicyRole, ValidatorRole),
  TypedBlueprint (TypedBlueprint, tbDefinitions, tbPreamble, tbValidators),
  TypedBlueprintPreamble (TypedBlueprintPreamble, tbpPlutusVersion),
  TypedScript (TypedScriptConstr),
  TypedScriptBlueprint (TypedScriptBlueprint, tsbCompiledCode, tsbTitle),
  UPLCProgram,
 )

-- Note: Extraction of the inner script is only allowed once the 'TypedScript' is fully applied.
pattern TypedScript :: PlutusVersion -> UPLCProgram -> TypedScript r '[]
pattern TypedScript ver s <- TypedScriptConstr ver s
{-# COMPLETE TypedScript #-}

{- | Obtain the Plutus script (ledger) version associated with given 'TypedScript'.

You can utilize this function to hook up offchain utilities that attach scripts to
a contract. Because 'TypedScript's carry around their Plutus version - you can safely
use 'getPlutusVersion' to determine whether you should declare the validator/minting policy
as V1 or V2 before attaching it to the Contract.

For example, if using 'plutus-apps' - you can create a function that determines whether to use
`plutusV1OtherScript` or `plutusV2OtherScript` in your script lookups:

@
unifiedOtherScript :: TypedScript ValidatorRole '[] -> ScriptLookups a
unifiedOtherScript (TypedScript ver s) = (if ver == ScriptV1 then plutusV1OtherScript else plutusV2OtherScript) vald
  where
    ver = Ply.getPlutusVersion ts
    vald = Validator ts
@
-}
getPlutusVersion :: TypedScript r params -> PlutusVersion
getPlutusVersion (TypedScriptConstr ver _) = ver
