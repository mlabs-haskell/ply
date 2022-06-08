{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg), ToData', someBuiltinArg) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.Text (Text)

import Plutus.V1.Ledger.Api
import PlutusCore (DefaultUni, Includes, Some, ValueOf)
import qualified PlutusCore as PLC

{- | Class of haskell types that can be applied as arguments to a Plutus script.

This class is meant to not only convert a type to its UPLC builtin representation, but _also verify_ any
associated invariants the type may have. It may also _normalize_ types.

As a result, 'toBuiltinArg' is partial for types that don't hold their invariants.
-}
class (DefaultUni `Includes` UPLCRep a) => PlyArg a where
  type UPLCRep a :: Type
  toBuiltinArg :: a -> UPLCRep a

someBuiltinArg :: PlyArg a => a -> Some (ValueOf DefaultUni)
someBuiltinArg = PLC.someValue . toBuiltinArg

instance PlyArg Bool where
  type UPLCRep Bool = Bool
  toBuiltinArg = id

instance PlyArg Integer where
  type UPLCRep Integer = Integer
  toBuiltinArg = id

instance PlyArg () where
  type UPLCRep () = ()
  toBuiltinArg = id

instance PlyArg ByteString where
  type UPLCRep ByteString = ByteString
  toBuiltinArg = id

instance PlyArg Text where
  type UPLCRep Text = Text
  toBuiltinArg = id

instance PlyArg Data where
  type UPLCRep Data = Data
  toBuiltinArg = id

instance (PlyArg a, PlyArg b) => PlyArg (a, b) where
  type UPLCRep (a, b) = (UPLCRep a, UPLCRep b)
  toBuiltinArg (x, y) = (toBuiltinArg x, toBuiltinArg y)

instance PlyArg a => PlyArg [a] where
  type UPLCRep [a] = [UPLCRep a]
  toBuiltinArg l = map toBuiltinArg l

instance PlyArg TxOutRef where
  type UPLCRep TxOutRef = Data
  toBuiltinArg = toData

instance PlyArg Credential where
  type UPLCRep Credential = Data
  toBuiltinArg cred =
    let credHash = case cred of
          PubKeyCredential (PubKeyHash h) -> fromBuiltin h
          ScriptCredential (ValidatorHash h) -> fromBuiltin h
     in if BS.length credHash == 32
          then toData cred
          else error "toBuiltinArg(Credential): Expected 32 bytes"

instance PlyArg StakingCredential where
  type UPLCRep StakingCredential = Data
  toBuiltinArg cred = case cred of
    StakingHash cred -> Constr 0 [toBuiltinArg cred]
    x -> toData x

instance (PlyArg a, ToData' (UPLCRep a)) => PlyArg (Maybe a) where
  type UPLCRep (Maybe a) = Data
  toBuiltinArg Nothing = Constr 1 []
  toBuiltinArg (Just x) = Constr 0 [toData' $ toBuiltinArg x]

instance PlyArg Address where
  type UPLCRep Address = Data
  toBuiltinArg (Address cred stakeCred) =
    let credData = toBuiltinArg cred
        stakeCredData = toBuiltinArg stakeCred
     in Constr 0 [credData, stakeCredData]

instance PlyArg CurrencySymbol where
  type UPLCRep CurrencySymbol = ByteString
  toBuiltinArg x =
    if BS.length bs == 32
      then bs
      else error "toBuiltinArg(CurrencySymbol): Expected 32 bytes"
    where
      bs = fromBuiltin $ unCurrencySymbol x

-- This silly little machinery is just here to support toData on 'Data' and 'ByteString's.
-- I didn't want to make orphan instances.

class ToData' d where
  toData' :: d -> Data

instance {-# OVERLAPPING #-} ToData' Data where
  toData' = id

instance {-# OVERLAPPING #-} ToData' ByteString where
  toData' = toData . toBuiltin

instance ToData a => ToData' a where
  toData' = toData
