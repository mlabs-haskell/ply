{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg), someBuiltinArg) where

import Data.Kind (Type)

import PlutusCore (DefaultUni, Includes, Some, ValueOf)
import qualified PlutusCore as PLC

class DefaultUni `Includes` UPLCRep a => PlyArg a where
  type UPLCRep a :: Type
  toBuiltinArg :: a -> UPLCRep a

someBuiltinArg :: PlyArg a => a -> Some (ValueOf DefaultUni)
someBuiltinArg = PLC.someValue . toBuiltinArg
