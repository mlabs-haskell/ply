{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.TypedWriter (
  type HasDefinitions,
  type ParamsOf,
  type VersionOf,
  type PlyParamsOf,
  type ReferencedTypesOf,
  HasArgDefinition (pdefinitionRef),
  mkParamSchemas,
  derivePDefinitions,
) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

import Generics.SOP (All, K (K), NP)
import Generics.SOP.NP (collapse_NP, cpure_NP)
import Plutarch.Internal.Term (PType)
import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.Prelude
import PlutusTx.Blueprint (Definitions, DefinitionsFor, HasBlueprintDefinition, PlutusVersion (PlutusV3), Schema, UnrollAll, definitionRef)
import PlutusTx.Blueprint.Definition (deriveDefinitions)

import Ply.Plutarch.Class (PlyArgOf)

{- | Get all the referenced types for a given list of Plutarch types.

This can be used in conjuction with 'ParamsOf' to obtain the type parameter to be passed to 'Definitions'.

=== Example

Given a Plutarch function type for a validator: (PTxOutRef :--> PData :--> PData :--> PScriptContext)

We can get the referenced types for its parameter(s) - i.e just 'PTxOutRef' in this case:

@
ReferencedTypesOf (ParamsOf (PTxOutRef :--> PData :--> PData :--> PScriptContext))
@

However, you'll also need the datum and redeemer types for the blueprints definitions field.
Assuming the datum is 'PTxId' and the redeemer is 'PAsData PTokenName', you can do:

@
ReferencedTypesOf (PTxId : PAsData PTokenName : ParamsOf (PTxOutRef :--> PData :--> PData :--> PScriptContext))
@

This can be used as the parameter to 'Definitions'. See: 'derivePDefinitions'
-}
type ReferencedTypesOf ptypes = UnrollAll (MapPlyArgOf ptypes)

{- | Handy constraint for ensuring all given Plutarch types satisfy constraints for having Blueprint definitions and therefore can be used
with 'pdefinitionRef' and 'derivePDefinitions'.
-}
type HasDefinitions ptypes = (DefinitionsFor (ReferencedTypesOf ptypes), All (HasArgDefinition (ReferencedTypesOf ptypes)) ptypes)

type HasArgDefinition :: [Type] -> PType -> Constraint
class HasBlueprintDefinition (PlyArgOf ptype) => HasArgDefinition referencedTypes ptype where
  -- | Plutarch version of 'definitionRef'
  pdefinitionRef :: Schema referencedTypes

-- We can't just use 'Compose HasBlueprintDefinition PlyArgOf' instead of this class
-- because PlyArgOf is a type family and cannot be passed unapplied!
instance HasBlueprintDefinition (PlyArgOf ptype) => HasArgDefinition referencedTypes ptype where
  pdefinitionRef = definitionRef @(PlyArgOf ptype)

{- | Given a list of Plutarch types, create the associated (as determined by 'PlyArgOf') Plutus blueprint
 schemas in order. These schemas can then be attached to blueprint creation functions from "PlutusTx.Blueprint".

 This is meant to be used for only the _parameters_ to a script. Not the datum/redeemer. As such, the 'ptypes' argument should be obtained
 via 'ParamsOf' on a Plutarch function type. Whereas the 'referencedTypes' argument should be obtained using the usual 'ReferencedTypesOf'.

=== Example

Assuming a Plutarch function type for a validator script: (PTxOutRef :--> PData :--> PData :--> PScriptContext)
With datum: PTxId
With redeemer: PAsData PTokenName

use:

@
mkParamSchemas
  @(ReferencedTypesOf (PTxId : PAsData PTokenName : ParamsOf (PTxOutRef :--> PData :--> PData :--> PScriptContext)))
  @(ParamsOf (PTxOutRef :--> PData :--> PData :--> PScriptContext))
@
-}
mkParamSchemas :: forall (referencedTypes :: [Type]) (ptypes :: [PType]). All (HasArgDefinition referencedTypes) ptypes => [Schema referencedTypes]
mkParamSchemas = collapse_NP np
  where
    np :: NP (K (Schema referencedTypes)) ptypes
    np = cpure_NP (Proxy @(HasArgDefinition referencedTypes)) f
    -- The 'a' type parameter has to be applied explicitly, so we can't just inline this in place of 'f' and expect 'a' to be inferred based on expected type.
    f :: forall (a :: PType). HasArgDefinition referencedTypes a => K (Schema referencedTypes) a
    f = K $ pdefinitionRef @referencedTypes @a

{- | Derive the schema definitions for the parameters, datum and redeemer of a script.
 Use it in conjuction with 'ReferencedTypesOf' to obtain the definitions needed for a script
-}
derivePDefinitions :: forall (ptypes :: [PType]). HasDefinitions ptypes => Definitions (ReferencedTypesOf ptypes)
derivePDefinitions = deriveDefinitions @(MapPlyArgOf ptypes)

type MapPlyArgOf :: [PType] -> [Type]
type family MapPlyArgOf xs where
  MapPlyArgOf '[] = '[]
  MapPlyArgOf (x ': xs) = PlyArgOf x ': MapPlyArgOf xs

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
  ParamsOf (PScriptContext :--> POpaque) = '[]
  ParamsOf (a :--> rest) = a : ParamsOf rest
  ParamsOf wrong =
    TypeError
      ( 'Text "Expected given Plutarch function type to end with: " :<>: ShowType (PScriptContext :--> POpaque)
          :$$: 'Text "But reached: " :<>: ShowType wrong
      )

{- | Given a Plutarch function type ending in
  'PData :--> PData :--> PScriptContext :--> POpaque' or
  'PData :--> PScriptContext :--> POpaque'
-}
type VersionOf :: PType -> PlutusVersion
type family VersionOf a where
  VersionOf (PScriptContext :--> POpaque) = PlutusV3
  VersionOf (_ :--> rest) = VersionOf rest
  VersionOf wrong =
    TypeError
      ( 'Text "Expected given Plutarch function type to end with: " :<>: ShowType (PScriptContext :--> POpaque)
          :$$: 'Text "But reached: " :<>: ShowType wrong
      )

-- | Map 'PlyArgOf' over a list of 'PType's.
type PlyParamsOf :: [PType] -> [Type]
type family PlyParamsOf pts = r | r -> pts where
  PlyParamsOf '[] = '[]
  PlyParamsOf (x : xs) = PlyArgOf x : PlyParamsOf xs
