-- | $module UPLC helpers.
module Ply.Core.UPLC (applyConstant, applyConstant') where

import Data.String (IsString)

import PlutusCore (Some (Some), ValueOf (ValueOf))
import qualified PlutusCore as PLC
import qualified PlutusCore.Version as PLC
import UntypedPlutusCore (
  DeBruijn (DeBruijn),
  DefaultFun,
  DefaultUni,
  Index,
  Program (Program),
  Term (Apply, Builtin, Constant, Delay, Error, Force, LamAbs, Var, Constr, Case),
  Version,
 )

pattern DefaultVersion :: Version
pattern DefaultVersion <-
  ((== PLC.plcVersion100) -> True)
  where
    DefaultVersion = PLC.plcVersion100

{- | Apply a 'DefaultUni' constant to given UPLC program, inlining if necessary.
 TODO: Subst optimizations when 'Apply'ing over non 'LamAbs' stuff as well, e.g chain of 'Apply'ies.
-}
applyConstant ::
  Program DeBruijn DefaultUni DefaultFun () ->
  Some (ValueOf DefaultUni) ->
  Program DeBruijn DefaultUni DefaultFun ()
applyConstant (Program () DefaultVersion f@(LamAbs () _ body)) c =
  Program () DefaultVersion $
    let arg = Constant () c
     in if isSmallConstant c then subst 1 (const body) f else Apply () f arg
applyConstant (Program () DefaultVersion f) c = Program () DefaultVersion . Apply () f $ Constant () c
applyConstant (Program () v _) _ =
  error $
    "applyConstant: unsupported program; expected version: "
      ++ show DefaultVersion
      ++ "\nactual version: "
      ++ show v

-- | Like 'applyConstant' but does not perform any optimizations.
applyConstant' ::
  Program DeBruijn DefaultUni DefaultFun () ->
  Some (ValueOf DefaultUni) ->
  Program DeBruijn DefaultUni DefaultFun ()
applyConstant' (Program () DefaultVersion f) c = Program () DefaultVersion . Apply () f $ Constant () c
applyConstant' (Program () v _) _ =
  error $
    "applyConstant: unsupported program; expected version: "
      ++ show DefaultVersion
      ++ "\nactual version: "
      ++ show v

-- | Name of UPLC terms, for usage in friendly error messages.
_termIdOf :: IsString p => Term name uni fun () -> p
_termIdOf (Constant () _) = "Constant"
_termIdOf (Builtin () _) = "Builtin"
_termIdOf (Error ()) = "Error"
_termIdOf (Var () _) = "Var"
_termIdOf (Apply () _ _) = "Apply"
_termIdOf (LamAbs () _ _) = "LamAbs"
_termIdOf (Delay () _) = "Delay"
_termIdOf (Force () _) = "Force"
_termIdOf (Constr () _ _) = "Constr"
_termIdOf (Case () _ _) = "Case"

isSmallConstant :: Some (ValueOf DefaultUni) -> Bool
isSmallConstant c = case c of
  -- These constants are smaller than variable references.
  Some (ValueOf PLC.DefaultUniBool _) -> True
  Some (ValueOf PLC.DefaultUniUnit _) -> True
  Some (ValueOf PLC.DefaultUniInteger n) | n < 256 -> True
  _ -> False

-- From Plutarch, with slight modifications.
subst ::
  Index ->
  (Index -> Term DeBruijn DefaultUni DefaultFun ()) ->
  Term DeBruijn DefaultUni DefaultFun () ->
  Term DeBruijn DefaultUni DefaultFun ()
subst idx x (Apply () yx yy) = Apply () (subst idx x yx) (subst idx x yy)
subst idx x (LamAbs () name y) = LamAbs () name (subst (idx + 1) x y)
subst idx x (Delay () y) = Delay () (subst idx x y)
subst idx x (Force () y) = Force () (subst idx x y)
subst idx x (Var () (DeBruijn idx')) | idx == idx' = x idx
subst idx _ y@(Var () (DeBruijn idx')) | idx > idx' = y
subst idx _ (Var () (DeBruijn idx')) | idx < idx' = Var () (DeBruijn $ idx' - 1)
subst _ _ y = y
