module Ply.Core.Unsafe (
  unsafeTypedScript,
  unsafeUnTypedScript,
) where

import Ply.Core.Types (
  TypedScript (TypedScriptConstr),
  UPLCProgram,
 )

-- | Unconditionally build a 'TypedScript. It is up to the caller to verify the type parameters are correct.
unsafeTypedScript :: UPLCProgram -> TypedScript r a
unsafeTypedScript = TypedScriptConstr

-- | Unconditionally extract from 'TypedScript', thereby forgetting all its type information.
unsafeUnTypedScript :: TypedScript r a -> UPLCProgram
unsafeUnTypedScript (TypedScriptConstr scrpt) = scrpt
