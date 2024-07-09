{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.TypedWriter (
  TypedWriter,
  type ParamsOf,
  type RoleOf,
  type VersionOf,
  type PlyParamsOf,
  writeTypedScript,
  toTypedScript,
  mkEnvelope,
) where

import Control.Exception (throwIO)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Txt
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

import Plutarch (Config, compile)
import Plutarch.LedgerApi.V2 (PScriptContext)
import Plutarch.Prelude
import Plutarch.Script (unScript)

import Ply (
  ScriptRole (MintingPolicyRole, ValidatorRole),
  ScriptVersion (ScriptV2),
  TypedScript,
  TypedScriptEnvelope (..),
 )
import Ply.Core.Internal.Reify (
  ReifyRole,
  ReifyTypenames,
  ReifyVersion,
  reifyVersion,
 )
import Ply.Core.Serialize (writeEnvelope)
import Ply.Core.TypedReader (typedScriptToEnvelope)
import Ply.Core.Unsafe (unsafeTypedScript)
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
  either (throwIO . userError . Txt.unpack) (writeEnvelope fp) envelope
  where
    envelope = mkEnvelope conf descr target

{- | Class of Plutarch function types that can be written to the filesystem as 'TypedScript's.

See: 'typedWriterInfo'.
-}
type TypedWriter ptype =
  ( ReifyVersion (VersionOf ptype)
  , ReifyRole (RoleOf ptype)
  , ReifyTypenames (PlyParamsOf (ParamsOf ptype))
  )

-- | Wrapper around 'toTypedScript' that builds a 'TypedScriptEnvelope' to serialize into the file system.
mkEnvelope ::
  forall ptype.
  TypedWriter ptype =>
  Config ->
  Text ->
  ClosedTerm ptype ->
  Either Text TypedScriptEnvelope
mkEnvelope conf descr pterm = typedScriptToEnvelope descr <$> toTypedScript conf pterm

{- | The core `ply-plutarch` function: obtain all the necessary information about a Plutarch script, and turn it into 'TypedScript'.

For a description of extra parameters are determined, see: 'PlyParamsOf' and 'ParamsOf' type families.

For a description of 'ScriptVersion' is determined, see: 'VersionOf' type family.

For a description of 'ScriptRole' is determined, see: 'RoleOf' type family.
-}
toTypedScript ::
  forall ptype.
  TypedWriter ptype =>
  Config ->
  ClosedTerm ptype ->
  Either Text (TypedScript (RoleOf ptype) (PlyParamsOf (ParamsOf ptype)))
toTypedScript conf pterm = unsafeTypedScript ver <$> scrptEith
  where
    scrptEith = unScript <$> compile conf pterm
    ver = reifyVersion $ Proxy @(VersionOf ptype)

{- | Given a Plutarch function type ending in
  'PData :--> PData :--> PScriptContext :--> POpaque' or
  'PData :--> PScriptContext :--> POpaque', determine its extra parameters.

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
  ParamsOf (PData :--> PData :--> PScriptContext :--> POpaque) = '[]
  ParamsOf (PData :--> PScriptContext :--> POpaque) = '[]
  ParamsOf (a :--> rest) = a : ParamsOf rest
  ParamsOf wrong =
    TypeError
      ( 'Text "Expected given Plutarch function type to end with: " :<>: ShowType (PData :--> PData :--> PScriptContext :--> POpaque)
          :$$: 'Text "Or with: " :<>: ShowType (PData :--> PScriptContext :--> POpaque)
          :$$: 'Text "But reached: " :<>: ShowType wrong
      )

{- | Given a Plutarch function type ending in
  'PData :--> PData :--> PScriptContext :--> POpaque' or
  'PData :--> PScriptContext :--> POpaque', determine its 'ScriptRole'.

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
  RoleOf (PData :--> PData :--> PScriptContext :--> POpaque) = ValidatorRole
  RoleOf (PData :--> PScriptContext :--> POpaque) = MintingPolicyRole
  RoleOf (_ :--> rest) = RoleOf rest
  RoleOf wrong =
    TypeError
      ( 'Text "Expected given Plutarch function type to end with: " :<>: ShowType (PData :--> PData :--> PScriptContext :--> POpaque)
          :$$: 'Text "Or with: " :<>: ShowType (PData :--> PScriptContext :--> POpaque)
          :$$: 'Text "But reached: " :<>: ShowType wrong
      )

{- | Given a Plutarch function type ending in
  'PData :--> PData :--> PScriptContext :--> POpaque' or
  'PData :--> PScriptContext :--> POpaque'
-}
type VersionOf :: PType -> ScriptVersion
type family VersionOf a where
  VersionOf (PData :--> PData :--> PScriptContext :--> POpaque) = ScriptV2
  VersionOf (PData :--> PScriptContext :--> POpaque) = ScriptV2
  VersionOf (_ :--> rest) = VersionOf rest
  VersionOf wrong =
    TypeError
      ( 'Text "Expected given Plutarch function type to end with: " :<>: ShowType (PData :--> PData :--> PScriptContext :--> POpaque)
          :$$: 'Text "Or with: " :<>: ShowType (PData :--> PScriptContext :--> POpaque)
          :$$: 'Text "But reached: " :<>: ShowType wrong
      )

-- | Map 'PlyArgOf' over a list of 'PType's.
type PlyParamsOf :: [PType] -> [Type]
type family PlyParamsOf pts = r | r -> pts where
  PlyParamsOf '[] = '[]
  PlyParamsOf (x : xs) = PlyArgOf x : PlyParamsOf xs
