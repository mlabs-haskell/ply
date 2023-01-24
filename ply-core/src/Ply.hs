module Ply (
  TypedScript (TypedScript),
  ScriptRole (..),
  ScriptReaderException (..),
  TypedScriptEnvelope (..),
  ScriptVersion (..),
  Typename,
  typeName,
  PlyArg,
  readTypedScript,
  readTypedScriptWith,
  getPlutusVersion,
  (#),
  (#$),
  (#!),
  (#$!),
) where

import Ply.Core.Apply ((#), (#!), (#$), (#$!))
import Ply.Core.Class (PlyArg)
import Ply.Core.TypedReader (readTypedScript, readTypedScriptWith)
import Ply.Core.Typename (typeName)
import Ply.Core.Types (
  ScriptReaderException (..),
  ScriptRole (MintingPolicyRole, ValidatorRole),
  ScriptVersion (..),
  TypedScript (TypedScriptConstr),
  TypedScriptEnvelope (..),
  Typename,
  UPLCProgram,
 )

-- Note: Extraction of the inner script is only allowed once the 'TypedScript' is fully applied.
pattern TypedScript :: ScriptVersion -> UPLCProgram -> TypedScript r '[]
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
getPlutusVersion :: TypedScript r params -> ScriptVersion
getPlutusVersion (TypedScriptConstr ver _) = ver
