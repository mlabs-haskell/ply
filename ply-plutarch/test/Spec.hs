{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Plutarch.Internal.Term (PType, punsafeConstantInternal)
import Plutarch.LedgerApi.V3 (PTokenName)
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (propEvalEqual)
import PlutusLedgerApi.V3.Orphans ()
import Test.QuickCheck (Arbitrary)
import Test.Tasty

import Ply (PlyArg (toSomeBuiltinArg))
import Ply.Plutarch.Class (PlyArgOf)

prop :: forall (a :: PType). (Arbitrary (PlyArgOf a), Show (PlyArgOf a), PlyArg (PlyArgOf a), PLiftable a, AsHaskell a ~ PlyArgOf a) => TestName -> TestTree
prop name = propEvalEqual @(PlyArgOf a) name (pconstant @a) (punsafeConstantInternal . toSomeBuiltinArg)

tests :: TestTree
tests =
  testGroup
    "types correspondence"
    [ prop @(PAsData PTokenName) "tokenname"
    ]

main :: IO ()
main = defaultMain tests
