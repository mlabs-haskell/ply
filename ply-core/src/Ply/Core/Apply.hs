module Ply.Core.Apply (applyParam, (#), (#$), (#!), (#$!)) where

import Ply.Core.Class (PlyArg, someBuiltinArg)
import Ply.Core.Types (TypedScript (TypedScript))
import Ply.Core.UPLC (applyConstant, applyConstant')

{- | Apply a parameter with a known type to given 'TypedScript'.

_NOTE_: If you just want a _pure application_, no optimizations, to produce a predictable UPLC AST each time
you apply the parameter - you should use 'applyParam'' instead. Some protocols require deterministic parameterization.
-}
applyParam :: PlyArg x => TypedScript r (x : xs) -> x -> TypedScript r xs
applyParam (TypedScript ver prog) x = TypedScript ver $ prog `applyConstant` someBuiltinArg x

-- | Like 'applyParam' but does not perform any optimizations, making the final AST predictable.
applyParam' :: PlyArg x => TypedScript r (x : xs) -> x -> TypedScript r xs
applyParam' (TypedScript ver prog) x = TypedScript ver $ prog `applyConstant'` someBuiltinArg x

{- | Operator version of 'applyParam', to be used as juxtaposition.

> scrpt # 42

_NOTE_: If you just want a _pure application_, no optimizations, to produce a predictable UPLC AST each time
you apply the parameter - you should use '(#!)' instead. Some protocols require deterministic parameterization.
-}
(#) :: PlyArg x => TypedScript r (x : xs) -> x -> TypedScript r xs
(#) = applyParam

infixl 8 #

{- | Operator version of 'applyParam'', to be used as juxtaposition.

> scrpt #! 42
-}
(#!) :: PlyArg x => TypedScript r (x : xs) -> x -> TypedScript r xs
(#!) = applyParam'

infixl 8 #!

{- | Low fixity version of '(#)', similar to '($)'.

> scrpt #$ foo $ 42

_NOTE_: If you just want a _pure application_, no optimizations, to produce a predictable UPLC AST each time
you apply the parameter - you should use '(#$!)' instead. Some protocols require deterministic parameterization.
-}
(#$) :: PlyArg x => TypedScript r (x : xs) -> x -> TypedScript r xs
(#$) = applyParam

infixr 0 #$

{- | Low fixity version of '(#$!)', similar to '($)'.

> scrpt #$! 42
-}
(#$!) :: PlyArg x => TypedScript r (x : xs) -> x -> TypedScript r xs
(#$!) = applyParam'

infixr 0 #$!
