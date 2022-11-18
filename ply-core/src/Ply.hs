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
  toUPLC,
  readTypedScript,
  (#),
  (#$),
  (#!),
  (#$!),
) where

import Data.Coerce (coerce)

import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

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

{- | Obtain the Plutus script (ledger) version associated with given 'TypedScript'.

You can utilize this function to hook up offchain utilities that attach scripts to
a contract. Because 'TypedScript's carry around their Plutus version - you can safely
use 'getPlutusVersion' to determine whether you should declare the validator/minting policy
as V1 or V2 before attaching it to the Contract.

For example, if using 'plutus-apps' - you can create a function that determines whether to use
`plutusV1OtherScript` or `plutusV2OtherScript` in your script lookups:

@
unifiedOtherScript :: TypedScript ValidatorRole '[] -> ScriptLookups a
unifiedOtherScript ts = (if ver == ScriptV1 then plutusV1OtherScript else plutusV2OtherScript) vald
  where
    ver = Ply.getPlutusVersion ts
    vald = Ply.toValidator ts
@
-}
getPlutusVersion :: TypedScript r params -> ScriptVersion
getPlutusVersion (TypedScript ver _) = ver

-- | Unconditionally obtain the raw UPLC program from a 'TypedScript'.
toUPLC :: TypedScript r params -> Program DeBruijn DefaultUni DefaultFun ()
toUPLC (TypedScript _ s) = coerce s
