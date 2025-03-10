{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Plutarch.Internal.Term (PType)
import Plutarch.Prelude
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "types - TODO"
    []

-- testCase "@PValidator/@PMintingPolicy" baselineTest
-- , testCase "@(PBool :--> _)" $
--     testHelper @'[PBool]
-- , testCase "@(PInteger :--> PUnit :--> PByteString :--> _)" $
--     testHelper @'[PBool, PUnit, PByteString]
-- , testCase
--     ( "@(PBuiltinPair PValue PCredential "
--         ++ ":--> PCurrencySymbol :--> PPosixTime :--> PInterval PInteger :--> _)"
--     )
--     $ testHelper
--       @'[ PBuiltinPair (PValue Sorted NonZero) PCredential
--         , PCurrencySymbol
--         , PPosixTime
--         , PInterval PInteger
--         ]
-- , testCase
--     ( "@(PBuiltinList PTxInInfo "
--         ++ ":--> PTxOutRef :--> PExtended PInteger :--> PPubKeyHash "
--         ++ ":--> PMaybeData PByteString :--> PMap PDatumHash PDatum :--> _)"
--     )
--     $ testHelper
--       @'[ PBuiltinList PTxInInfo
--         , PTxOutRef
--         , PExtended PInteger
--         , PPubKeyHash
--         , PMaybeData PByteString
--         , PMap Sorted PDatumHash PDatum
--         ]

main :: IO ()
main = defaultMain tests

type PTypeWith :: PType -> [PType] -> PType
type family PTypeWith e xs = r where
  PTypeWith e '[] = e
  PTypeWith e (x : xs) = x :--> PTypeWith e xs
