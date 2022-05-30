module Ply () where

import Data.Coerce
import Data.Kind

import Plutus.V1.Ledger.Scripts
import UntypedPlutusCore
import qualified PlutusCore as PLC
import PlutusCore (Some (Some), ValueOf (ValueOf), Includes)
import Data.String (IsString)

type role TypedScript nominal nominal
type TypedScript :: ScriptRole -> [Type] -> Type
newtype TypedScript r a = TypedScript (Program DeBruijn DefaultUni DefaultFun ())

data ScriptRole = ValidatorScript | MintingPolicyScript

toValidator :: TypedScript 'ValidatorScript '[] -> Validator
toValidator (TypedScript s) = coerce s

toMintingPolicy :: TypedScript 'MintingPolicyScript '[] -> MintingPolicy
toMintingPolicy (TypedScript s) = coerce s

applyParam :: DefaultUni `Includes` x => x -> TypedScript r (x ': xs) -> TypedScript r xs
applyParam x (TypedScript (Program () DefaultVersion f@(LamAbs () _ body))) =
  TypedScript . Program () DefaultVersion $
  let c = PLC.someValue x
      arg = Constant () c
  in if isSmallConstant c then subst 1 (const body) f else Apply () f arg
applyParam _ (TypedScript (Program () v t)) = error
  $ "applyParam: unsupported program; expected version: " ++ show DefaultVersion ++
    "; expected term: LamAbs\n" ++
    "actual version: " ++ show v ++
    "; actual term: " ++ termIdOf t

termIdOf :: IsString p => Term name uni fun () -> p
termIdOf (Constant () _) = "Constant"
termIdOf (Builtin () _)  = "Builtin"
termIdOf (Error ())      = "Error"
termIdOf (Var () _)      = "Var"
termIdOf (Apply () _ _)  = "Apply"
termIdOf (LamAbs () _ _) = "LamAbs"
termIdOf (Delay () _)    = "Delay"
termIdOf (Force () _)    = "Force"

pattern DefaultVersion :: Version ()
pattern DefaultVersion <- ((==PLC.defaultVersion ()) -> True) where
  DefaultVersion = PLC.defaultVersion ()

uplcConstantOf :: DefaultUni `Includes` x => x -> Term name DefaultUni fun ()
uplcConstantOf x = Constant () $ PLC.someValue x

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
