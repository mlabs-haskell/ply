{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Typename (
  NumTyArgs,
  PlyTypeName',
  PlyTypeName,
  plyTypeName',
  plyTypeName,
  Typename (Typename),
) where

import Control.Monad (when)
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (String))
import Data.Aeson.Types (unexpected)
import Data.Kind (Constraint)
import Data.Text (Text)
import qualified Data.Text as Txt
import GHC.TypeLits
import Type.Reflection (Typeable, tyConModule, tyConName, typeRep, typeRepTyCon)

type NumTyArgs :: k -> Nat
type family NumTyArgs a where
  NumTyArgs (f x) = 1 + NumTyArgs f
  NumTyArgs _ = 0

type PlyTypeName :: k -> Constraint
type PlyTypeName a = PlyTypeName' (NumTyArgs a) a

class PlyTypeName' numArgs a where
  plyTypeName' :: Typename

instance Typeable a => PlyTypeName' 0 a where
  plyTypeName' =
    Typename
      . Txt.pack
      . toNewGHCIntegerModuleName
      $ srcModuleName ++ ':' : tyConName tyCon
    where
      srcModuleName = toNewLedgerModuleName $ tyConModule tyCon
      tyCon = typeRepTyCon $ typeRep @a

instance (PlyTypeName a, PlyTypeName b) => PlyTypeName' c (a b) where
  plyTypeName' = plyTypeName @a <> Typename "#" <> plyTypeName @b

-- | Fully qualified type names.
newtype Typename = Typename Text
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON)

{- | Obtain the 'Typename' for a given type.

No specific guarantees are given as per the representation of the 'Typename', as it is an internal detail.
However, typenames of 2 different types (different module) won't be the same.
-}
plyTypeName :: forall a. PlyTypeName a => Typename
plyTypeName = plyTypeName' @(NumTyArgs a) @a

-- FIXME: Must have stricter parsing rules.
instance FromJSON Typename where
  parseJSON (String t) = do
    when (Txt.null t) $ fail "Typename cannot be empty"
    pure $ Typename t
  parseJSON x = unexpected x

{- This horrible hack below is due to the fact that
an out of date offchain will not be able to recognize
the newer ledger api typenames.

Since type names are qualified
on their module name: e.g CurrencySymbol parameter will be put
in as PlutusLedgerApi.V1.Value:CurrencySymbol in the `.plutus` envelope
created within the onchain project. However, in the outdated offchain, the
type name of CurrencySymbol would be Plutus.V1.Ledger.Value:CurrencySymbol!

So we replace old ledger module names with newer module names....
-}
toNewLedgerModuleName :: String -> String
toNewLedgerModuleName (splitAt oldLedgerModuleNameLen -> (!prefx, !sufx))
  | prefx `elem` [oldLedgerV1ModuleName, oldLedgerV2ModuleName] =
    getNewModuleName prefx
      ++ if sufx == ".Api"
        then "" -- The Plutus.Vx.Ledger.Api module got renamed to just PlutusLedgerApi.Vx
        else sufx
  | otherwise = prefx ++ sufx

-- Older GHCs export 'Integer' at "GHC.Num.Integer", but older ones have it at "GHC.Integer.Type"
toNewGHCIntegerModuleName :: String -> String
toNewGHCIntegerModuleName "GHC.Integer.Type:Integer" = "GHC.Num.Integer:Integer"
toNewGHCIntegerModuleName x = x

oldLedgerModuleNameLen :: Int
oldLedgerModuleNameLen = length oldLedgerV1ModuleName

oldLedgerV1ModuleName :: String
oldLedgerV1ModuleName = "Plutus.V1.Ledger"

oldLedgerV2ModuleName :: String
oldLedgerV2ModuleName = "Plutus.V2.Ledger"

getNewModuleName :: String -> String
getNewModuleName x
  | x == oldLedgerV1ModuleName = newLedgerV1ModuleName
  | x == oldLedgerV2ModuleName = newLedgerV2ModuleName
  | otherwise = error "getNewModuleName: bad arg"

newLedgerV1ModuleName :: String
newLedgerV1ModuleName = "PlutusLedgerApi.V1"

newLedgerV2ModuleName :: String
newLedgerV2ModuleName = "PlutusLedgerApi.V2"
