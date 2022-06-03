module Ply.Core.Apply (applyParam, (#)) where

import Ply.Core.Class (PlyArg, someBuiltinArg)
import Ply.Core.Types (TypedScript (TypedScript))
import Ply.Core.UPLC (applyConstant)

-- | Apply a parameter with a known type to given 'TypedScript'.
applyParam :: PlyArg x => TypedScript r (x ': xs) -> x -> TypedScript r xs
applyParam (TypedScript prog) x = TypedScript $ prog `applyConstant` someBuiltinArg x

-- | Operator version of 'applyParam'.
(#) :: PlyArg x => TypedScript r (x ': xs) -> x -> TypedScript r xs
(#) = applyParam
