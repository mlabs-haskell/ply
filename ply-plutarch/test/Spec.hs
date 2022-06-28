{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Api.V1
import Plutarch.Prelude
import Plutus.V1.Ledger.Api
import qualified PlutusTx.AssocMap as PlutusMap

import Ply (ScriptRole (MintingPolicyRole, ValidatorRole), Typename, typeName)
import Ply.Plutarch.TypedWriter (TypedWriter, typeWriterInfo)

-- | Ensure 'typeWriterInfo @ptype' yields the expected 'ScriptRole' and '[Typename]'.
testHelper ::
  forall ptypeList.
  (TypedWriter (PTypeWith PValidator ptypeList), TypedWriter (PTypeWith PMintingPolicy ptypeList)) =>
  [Typename] ->
  Assertion
testHelper expectedTypes = do
  let (actualRole0, actualTypes0, _) = typeWriterInfo @(PTypeWith PValidator ptypeList) undefined
  actualRole0 @?= ValidatorRole
  let (actualRole1, actualTypes1, _) = typeWriterInfo @(PTypeWith PMintingPolicy ptypeList) undefined
  actualRole1 @?= MintingPolicyRole
  actualTypes0 @?= actualTypes1
  actualTypes0 @?= expectedTypes

baselineTest :: Assertion
baselineTest = testHelper @'[] []

tests :: TestTree
tests =
  testGroup
    "typeWriterInfo works as expected"
    [ testCase "@PValidator/@PMintingPolicy" baselineTest
    , testCase "@(PBool :--> _)" $
        testHelper @'[PBool] [typeName @Bool]
    , testCase "@(PInteger :--> PUnit :--> PByteString :--> _)" $
        testHelper @'[PBool, PUnit, PByteString]
          [ typeName @Bool
          , typeName @()
          , typeName @ByteString
          ]
    , testCase
        ( "@(PBuiltinPair PValue PCredential "
            ++ ":--> PCurrencySymbol :--> PPOSIXTime :--> PInterval PInteger :--> _)"
        )
        $ testHelper @'[PBuiltinPair PValue PCredential, PCurrencySymbol, PPOSIXTime, PInterval PInteger]
          [ typeName @(Value, Credential)
          , typeName @CurrencySymbol
          , typeName @POSIXTime
          , typeName @(Interval Integer)
          ]
    , testCase
        ( "@(PBuiltinList PTxInInfo "
            ++ ":--> PTxOutRef :--> PExtended PInteger :--> PPubKeyHash "
            ++ ":--> PMaybeData PByteString :--> PMap PDatumHash PDatum)"
        )
        $ testHelper
          @'[ PBuiltinList PTxInInfo
            , PTxOutRef
            , PExtended PInteger
            , PPubKeyHash
            , PMaybeData PByteString
            , PMap PDatumHash PDatum
            ]
          [ typeName @[TxInInfo]
          , typeName @TxOutRef
          , typeName @(Extended Integer)
          , typeName @PubKeyHash
          , typeName @(Maybe ByteString)
          , typeName @(PlutusMap.Map DatumHash Datum)
          ]
    ]

main :: IO ()
main = defaultMain tests

type PTypeWith :: PType -> [PType] -> PType
type family PTypeWith e xs = r where
  PTypeWith e '[] = e
  PTypeWith e (x : xs) = x :--> PTypeWith e xs
