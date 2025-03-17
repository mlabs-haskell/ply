{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.TypedWriter (
  type TypedWriter,
  type TypedWriter',
  type ParamsOf,
  type VersionOf,
  type PlyParamsOf,
  type ReferencedTypesOf,
  type ReferencedTypesOf',
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

type ReferencedTypesOf datum redeemer ptype = UnrollAll (PlyArgOf datum : PlyArgOf redeemer : MapPlyArgOf (ParamsOf ptype))

type ReferencedTypesOf' redeemer ptype = UnrollAll (PlyArgOf redeemer : MapPlyArgOf (ParamsOf ptype))

type TypedWriter datum redeemer ptype = (DefinitionsFor (ReferencedTypesOf datum redeemer ptype), All (HasArgDefinition (ReferencedTypesOf datum redeemer ptype)) (ParamsOf ptype))

type TypedWriter' redeemer ptype = (HasBlueprintDefinition (PlyArgOf redeemer), DefinitionsFor (ReferencedTypesOf' redeemer ptype), All (HasArgDefinition (ReferencedTypesOf' redeemer ptype)) (ParamsOf ptype))

type HasArgDefinition :: [Type] -> PType -> Constraint
class HasArgDefinition referencedTypes ptype where
  -- | Plutarch version of 'definitionRef'
  pdefinitionRef :: Schema referencedTypes

-- We can't just use 'Compose HasBlueprintDefinition PlyArgOf' instead of this class
-- because PlyArgOf is a type family and cannot be passed unapplied!
instance HasBlueprintDefinition (PlyArgOf ptype) => HasArgDefinition referencedTypes ptype where
  pdefinitionRef = definitionRef @(PlyArgOf ptype)

{- | Given a list of Plutarch types, create the associated (as determined by 'PlyArgOf') Plutus blueprint
 schemas in order. These schemas can then be attached to blueprint creation functions from "PlutusTx.Blueprint".
-}
mkParamSchemas :: forall (referencedTypes :: [Type]) (ptypes :: [PType]). All (HasArgDefinition referencedTypes) ptypes => [Schema referencedTypes]
mkParamSchemas = collapse_NP np
  where
    np :: NP (K (Schema referencedTypes)) ptypes
    np = cpure_NP (Proxy @(HasArgDefinition referencedTypes)) f
    -- The 'a' type parameter has to be applied explicitly, so we can't just inline this in place of 'f' and expect 'a' to be inferred based on expected type.
    f :: forall (a :: PType). HasArgDefinition referencedTypes a => K (Schema referencedTypes) a
    f = K $ pdefinitionRef @referencedTypes @a

-- | Derive the schema definitions for the parameters, datum and redeemer of a script.
derivePDefinitions ::
  forall (ptypes :: [PType]).
  (DefinitionsFor (UnrollAll (MapPlyArgOf ptypes))) =>
  Definitions (UnrollAll (MapPlyArgOf ptypes))
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
  'PData :--> PScriptContext :--> POpaque'
-}
type VersionOf :: PType -> PlutusVersion
type family VersionOf a where
  VersionOf (PData :--> PData :--> PScriptContext :--> POpaque) = PlutusV3
  VersionOf (PData :--> PScriptContext :--> POpaque) = PlutusV3
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
