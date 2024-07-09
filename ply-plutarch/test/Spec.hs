{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.ByteString (ByteString)

import Plutarch (Config (Tracing), LogLevel (LogInfo), TracingMode (DetTracing))
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V2
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2
import qualified PlutusTx.AssocMap as PlutusMap
import Test.Tasty
import Test.Tasty.HUnit

import Ply (
  ScriptRole (MintingPolicyRole, ValidatorRole),
  ScriptVersion (ScriptV2),
  TypedScriptEnvelope (TypedScriptEnvelope),
  Typename,
  plyTypeName,
 )
import Ply.Plutarch.TypedWriter (TypedWriter, mkEnvelope)

-- | Ensure 'typedWriterInfo @ptype' yields the expected 'ScriptRole' and '[Typename]'.
testHelper ::
  forall ptypeList.
  ( TypedWriter (PTypeWith (PData :--> PData :--> PScriptContext :--> POpaque) ptypeList)
  , TypedWriter (PTypeWith (PData :--> PScriptContext :--> POpaque) ptypeList)
  ) =>
  [Typename] ->
  Assertion
testHelper _expectedTypes = do
  let (actualVersion2, actualRole2, _, _) =
        unEnvelope $ mkEnvelope @(PTypeWith (PData :--> PData :--> PScriptContext :--> POpaque) ptypeList) conf mempty (punsafeCoerce $ plam id)
  actualRole2 @?= ValidatorRole
  let (actualVersion3, actualRole3, _, _) =
        unEnvelope $ mkEnvelope @(PTypeWith (PData :--> PScriptContext :--> POpaque) ptypeList) conf mempty (punsafeCoerce $ plam id)
  actualRole3 @?= MintingPolicyRole
  actualVersion2 @?= ScriptV2
  actualVersion2 @?= actualVersion3
  where
    conf = Tracing LogInfo DetTracing
    unEnvelope (Right (TypedScriptEnvelope ver rl param _ scr)) = (ver, rl, param, scr)
    unEnvelope (Left t) = error $ show t

baselineTest :: Assertion
baselineTest = testHelper @'[] []

tests :: TestTree
tests =
  testGroup
    "mkEnvelope works as expected"
    [ testCase "@PValidator/@PMintingPolicy" baselineTest
    , testCase "@(PBool :--> _)" $
        testHelper @'[PBool] [plyTypeName @Bool]
    , testCase "@(PInteger :--> PUnit :--> PByteString :--> _)" $
        testHelper @'[PBool, PUnit, PByteString]
          [ plyTypeName @Bool
          , plyTypeName @()
          , plyTypeName @ByteString
          ]
    , testCase
        ( "@(PBuiltinPair PValue PCredential "
            ++ ":--> PCurrencySymbol :--> PPosixTime :--> PInterval PInteger :--> _)"
        )
        $ testHelper
          @'[ PBuiltinPair (PValue Sorted NonZero) PCredential
            , PCurrencySymbol
            , PPosixTime
            , PInterval PInteger
            ]
          [ plyTypeName @(Value, Credential)
          , plyTypeName @CurrencySymbol
          , plyTypeName @POSIXTime
          , plyTypeName @(Interval Integer)
          ]
    , testCase
        ( "@(PBuiltinList PTxInInfo "
            ++ ":--> PTxOutRef :--> PExtended PInteger :--> PPubKeyHash "
            ++ ":--> PMaybeData PByteString :--> PMap PDatumHash PDatum :--> _)"
        )
        $ testHelper
          @'[ PBuiltinList PTxInInfo
            , PTxOutRef
            , PExtended PInteger
            , PPubKeyHash
            , PMaybeData PByteString
            , PMap Sorted PDatumHash PDatum
            ]
          [ plyTypeName @[TxInInfo]
          , plyTypeName @TxOutRef
          , plyTypeName @(Extended Integer)
          , plyTypeName @PubKeyHash
          , plyTypeName @(Maybe ByteString)
          , plyTypeName @(PlutusMap.Map DatumHash Datum)
          ]
    ]

main :: IO ()
main = defaultMain tests

type PTypeWith :: PType -> [PType] -> PType
type family PTypeWith e xs = r where
  PTypeWith e '[] = e
  PTypeWith e (x : xs) = x :--> PTypeWith e xs
