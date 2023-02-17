module Ply.TypeList (TyList, Cons, Nil) where

data TyList :: forall k. k -> Type
data TyList k

foreign import data Cons :: forall k. k -> TyList k -> TyList k
foreign import data Nil :: forall k. TyList k
