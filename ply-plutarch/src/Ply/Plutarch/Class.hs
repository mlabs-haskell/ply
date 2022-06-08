{-# LANGUAGE UndecidableInstances #-}

module Ply.Plutarch.Class (PlyArgOf) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Plutarch.Api.V1 (PCurrencySymbol, PTxOutRef)
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (CurrencySymbol, Data, TxOutRef)

-- TODO: How to handle 'PAsData'?

-- | 'PlyArgOf' yields the corresponding Haskell type for a given Plutarch type.
type PlyArgOf :: PType -> Type
type family PlyArgOf a = r | r -> a

type instance PlyArgOf PBool = Bool

type instance PlyArgOf PInteger = Integer

type instance PlyArgOf PUnit = ()

type instance PlyArgOf PByteString = ByteString

type instance PlyArgOf PString = Text

type instance PlyArgOf PData = Data

type instance PlyArgOf (PBuiltinPair a b) = (PlyArgOf a, PlyArgOf b)

type instance PlyArgOf (PBuiltinList a) = [PlyArgOf a]

type instance PlyArgOf PTxOutRef = TxOutRef

type instance PlyArgOf PCurrencySymbol = CurrencySymbol
