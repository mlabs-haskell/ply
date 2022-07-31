{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.TypedWriter (TypedWriter, writeTypedScript, typedWriterInfo) where

import Control.Exception (throwIO)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Typeable (Typeable)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

import Plutarch (ClosedTerm, Config, PType, compile, type (:-->))
import Plutarch.Api.V1 (PMintingPolicy, PValidator)
import PlutusLedgerApi.V1.Scripts (Script)

import Ply (ScriptRole (MintingPolicyRole, ValidatorRole), ScriptVersion (ScriptV1), Typename, typeName)
import Ply.Core.Serialize (writeEnvelope)
import Ply.Plutarch.Class (PlyArgOf)

{- | Write a parameterized Plutarch validator or minting policy into the filesystem.

The result can be read by 'readTypedScript'.
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

type TypedWriter_ :: PType -> Constraint
class
  ( ReifyRole (RoleOf ptype)
  , ReifyTypenames (PlyParamsOf (ParamsOf ptype))
  ) =>
  TypedWriter_ ptype
  where
  -- | The core `ply-plutarch` function: obtain all the necessary information about a Plutarch script.
  typedWriterInfo :: Config -> ClosedTerm ptype -> (ScriptRole, [Typename], Either Text Script)

class TypedWriter_ ptype => TypedWriter ptype
instance TypedWriter_ ptype => TypedWriter ptype

instance
  ( ReifyRole (RoleOf ptype)
  , ReifyTypenames (PlyParamsOf (ParamsOf ptype))
  ) =>
  TypedWriter_ ptype
  where
  typedWriterInfo conf pterm = (rl, paramTypes, scrpt)
    where
      scrpt = compile conf pterm
      rl = reifyRole $ Proxy @(RoleOf ptype)
      paramTypes = reifyTypenames $ Proxy @(PlyParamsOf (ParamsOf ptype))

type ReifyRole :: ScriptRole -> Constraint
class ReifyRole s where
  reifyRole :: Proxy s -> ScriptRole

type ReifyTypenames :: [Type] -> Constraint
class ReifyTypenames ts where
  reifyTypenames :: Proxy ts -> [Typename]

instance ReifyRole 'ValidatorRole where
  reifyRole _ = ValidatorRole

instance ReifyRole 'MintingPolicyRole where
  reifyRole _ = MintingPolicyRole

instance ReifyTypenames '[] where
  reifyTypenames _ = []

instance (Typeable x, ReifyTypenames xs) => ReifyTypenames (x : xs) where
  reifyTypenames _ = typeName @x : reifyTypenames (Proxy @xs)

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
