{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.TypedWriter (TypedWriter, writeTypedScript, typeWriterInfo) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.TypeLits

import Plutarch
import Plutarch.Api.V1
import Plutus.V1.Ledger.Scripts

import Ply
import Ply.Core.Serialize
import Ply.Plutarch.Class

{- | Write a parameterized Plutarch validator or minting policy into the filesystem.

The result can be read by 'readTypedScript'.
-}
writeTypedScript :: TypedWriter pt => FilePath -> Text -> ClosedTerm pt -> IO ()
writeTypedScript fp descr target = writeEnvelope fp descr rl paramTypes scrpt
  where
    (rl, paramTypes, scrpt) = typeWriterInfo target

type TypedWriter_ :: PType -> Constraint
class
  ( ReifyRole (RoleOf ptype)
  , ReifyTypenames (PlyParamsOf (ParamsOf ptype))
  ) =>
  TypedWriter_ ptype
  where
  typeWriterInfo :: ClosedTerm ptype -> (ScriptRole, [Typename], Script)

class TypedWriter_ ptype => TypedWriter ptype

instance
  ( ReifyRole (RoleOf ptype)
  , ReifyTypenames (PlyParamsOf (ParamsOf ptype))
  ) =>
  TypedWriter_ ptype
  where
  typeWriterInfo pterm = (rl, paramTypes, scrpt)
    where
      scrpt = compile pterm
      rl = reifyRole $ Proxy @(RoleOf ptype)
      paramTypes = reifyTypenames $ Proxy @(PlyParamsOf (ParamsOf ptype))

type ReifyRole :: ScriptRole -> Constraint
class ReifyRole s where
  reifyRole :: Proxy s -> ScriptRole

type ReifyTypenames :: [Type] -> Constraint
class ReifyTypenames ts where
  reifyTypenames :: Proxy ts -> [Typename]

instance ReifyRole 'ValidatorScript where
  reifyRole _ = ValidatorScript

instance ReifyRole 'MintingPolicyScript where
  reifyRole _ = MintingPolicyScript

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

=== Note ===
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
ValidatorScript

>>> :k! RoleOf (PData :--> PScriptContext :--> POpaque)
MintingPolicyScript

>>> :k! RoleOf (PByteString :--> PData :--> PScriptContext :--> POpaque)
MintingPolicyScript

=== Note ===
Indeed, there is a possibility for ambiguity here. Is `PData :--> PData :--> PScriptContext :--> POpaque` a
minting policy with an extra 'PData' parameter? Or is it a validator?

Currently, the Validator choice is given precedence. If you wanted to use the alternative meaning, use:
`PAsData PData :--> PData :--> PScriptContext :--> POPaque` instead.
-}
type RoleOf :: PType -> ScriptRole
type family RoleOf a where
  RoleOf PValidator = ValidatorScript
  RoleOf PMintingPolicy = MintingPolicyScript
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
