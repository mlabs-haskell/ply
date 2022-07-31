{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.TypedWriter (
  TypedWriter,
  type ParamsOf,
  type RoleOf,
  type PlyParamsOf,
  writeTypedScript,
  typedWriterInfo,
) where

import Control.Exception (throwIO)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Txt
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

import Plutarch (ClosedTerm, Config, PType, compile, type (:-->))
import Plutarch.Api.V1 (PMintingPolicy, PValidator)
import PlutusLedgerApi.V1.Scripts (Script)

import Ply (ScriptRole (MintingPolicyRole, ValidatorRole), ScriptVersion (ScriptV1), Typename)
import Ply.Core.Internal.Reify (
  ReifyRole,
  ReifyTypenames,
  reifyRole,
  reifyTypenames,
 )
import Ply.Core.Serialize (writeEnvelope)
import Ply.Plutarch.Class (PlyArgOf)

{- | Write a parameterized Plutarch validator or minting policy into the filesystem.

The result can be read by 'readTypedScript'.

Please also see: 'typedWriterInfo'.
-}
writeTypedScript ::
  TypedWriter pt =>
  -- | Plutarch compiler configuration which will be used to compile the script.
  Config ->
  -- | Description to be associated with the compiled script file, semantically irrelevant.
  Text ->
  -- | File path to save the file to.
  FilePath ->
  -- | The parameterized Plutarch validator/minting policy.
  ClosedTerm pt ->
  IO ()
writeTypedScript conf descr fp target =
  either (throwIO . userError . Txt.unpack) (writeEnvelope descr fp ScriptV1 rl paramTypes) scrpt
  where
    (rl, paramTypes, scrpt) = typedWriterInfo conf target

{- | Class of Plutarch function types that can be written to the filesystem as 'TypedScript's.

See: 'typedWriterInfo'.
-}
type TypedWriter ptype =
  ( ReifyRole (RoleOf ptype)
  , ReifyTypenames (PlyParamsOf (ParamsOf ptype))
  )

{- | The core `ply-plutarch` function: obtain all the necessary information about a Plutarch script.

For a description of extra parameters are determined, see: 'PlyParamsOf' and 'ParamsOf' type families.

For a description of 'ScriptVersion' is determined, see: 'VersionOf' type family.

For a description of 'ScriptRole' is determined, see: 'RoleOf' type family.
-}
typedWriterInfo ::
  forall ptype.
  TypedWriter ptype =>
  Config ->
  ClosedTerm ptype ->
  (ScriptRole, [Typename], Either Text Script)
typedWriterInfo conf pterm = (rl, paramTypes, scrpt)
  where
    scrpt = compile conf pterm
    rl = reifyRole $ Proxy @(RoleOf ptype)
    paramTypes = reifyTypenames $ Proxy @(PlyParamsOf (ParamsOf ptype))

{- | Given a Plutarch function type ending in 'PValidator' or 'PMintingPolicy', determine its extra parameters.

>>> :k! ParamsOf (PData :--> PData :--> PScriptContext :--> POpaque)
[]

>>> :k! ParamsOf (PData :--> PScriptContext :--> POpaque)
[]

>>> :k! ParamsOf (PByteString :--> PData :--> PScriptContext :--> POpaque)
[PByteString]

=== Note
Indeed, there is a possibility for ambiguity here. Is `PData :--> PData :--> PScriptContext :--> POpaque` a
minting policy with an extra 'PData' parameter? Or is it a validator?

Currently, the Validator choice is given precedence. If you wanted to use the alternative meaning, use:
`PAsData PData :--> PData :--> PScriptContext :--> POPaque` instead.
-}
type ParamsOf :: PType -> [PType]
type family ParamsOf a where
  ParamsOf PValidator = '[]
  ParamsOf PMintingPolicy = '[]
  ParamsOf (a :--> rest) = a : ParamsOf rest
  ParamsOf wrong =
    TypeError
      ( 'Text "Expected given Plutarch function type to end with: " :<>: ShowType PValidator
          :$$: 'Text "Or with: " :<>: ShowType PMintingPolicy
          :$$: 'Text "But reached: " :<>: ShowType wrong
      )

{- | Given a Plutarch function type ending in 'PValidator' or 'PMintingPolicy', determine its 'ScriptRole'.

>>> :k! RoleOf (PData :--> PData :--> PScriptContext :--> POpaque)
ValidatorRole

>>> :k! RoleOf (PData :--> PScriptContext :--> POpaque)
MintingPolicyRole

>>> :k! RoleOf (PByteString :--> PData :--> PScriptContext :--> POpaque)
MintingPolicyRole

=== Note
Indeed, there is a possibility for ambiguity here. Is `PData :--> PData :--> PScriptContext :--> POpaque` a
minting policy with an extra 'PData' parameter? Or is it a validator?

Currently, the Validator choice is given precedence. If you wanted to use the alternative meaning, use:
`PAsData PData :--> PData :--> PScriptContext :--> POPaque` instead.
-}
type RoleOf :: PType -> ScriptRole
type family RoleOf a where
  RoleOf PValidator = ValidatorRole
  RoleOf PMintingPolicy = MintingPolicyRole
  RoleOf (_ :--> rest) = RoleOf rest
  RoleOf wrong =
    TypeError
      ( 'Text "Expected given Plutarch function type to end with: " :<>: ShowType PValidator
          :$$: 'Text "Or with: " :<>: ShowType PMintingPolicy
          :$$: 'Text "But reached: " :<>: ShowType wrong
      )

-- | Map 'PlyArgOf' over a list of 'PType's.
type PlyParamsOf :: [PType] -> [Type]
type family PlyParamsOf pts = r | r -> pts where
  PlyParamsOf '[] = '[]
  PlyParamsOf (x : xs) = PlyArgOf x : PlyParamsOf xs
