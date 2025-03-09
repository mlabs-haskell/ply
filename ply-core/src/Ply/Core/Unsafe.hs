{-# LANGUAGE UnboxedTuples #-}

module Ply.Core.Unsafe (
  unsafeTypedScript,
  unsafeUnTypedScript,
  unsafeUnTypedScript',
) where

import PlutusTx.Blueprint (PlutusVersion)

import Ply.Core.Types (
  TypedScript (TypedScriptConstr),
  UPLCProgram,
 )

-- | Unconditionally build a 'TypedScript. It is up to the caller to verify the type parameters are correct.
unsafeTypedScript :: PlutusVersion -> UPLCProgram -> TypedScript r a
unsafeTypedScript = TypedScriptConstr

-- | Unconditionally extract from 'TypedScript', thereby forgetting all its type information.
unsafeUnTypedScript :: TypedScript r a -> (# PlutusVersion, UPLCProgram #)
unsafeUnTypedScript (TypedScriptConstr ver scrpt) = (# ver, scrpt #)

-- | Unconditionally extract from 'TypedScript', thereby forgetting all its type information.
unsafeUnTypedScript' :: TypedScript r a -> (PlutusVersion, UPLCProgram)
unsafeUnTypedScript' (TypedScriptConstr ver scrpt) = (ver, scrpt)
