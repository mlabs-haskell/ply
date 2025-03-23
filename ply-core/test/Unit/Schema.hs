{-# LANGUAGE AllowAmbiguousTypes #-}

module Unit.Schema (test) where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import PlutusLedgerApi.Data.V3 (Address, BuiltinByteString, BuiltinData, Credential, CurrencySymbol, DatumHash, Interval, POSIXTime, PubKeyHash, RedeemerHash, ScriptHash, StakingCredential, TokenName, TxId, TxOutRef, Value)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx.Blueprint (DefinitionsFor, HasBlueprintDefinition, UnrollAll, definitionRef, definitionsToMap, deriveDefinitions)
import qualified PlutusTx.Ratio as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

import Ply.Core.Schema (
  SchemaDescription,
  deriveSchemaDescriptions,
  descriptionFromPlutus,
  normalizeSchemaDescription,
 )

data TestFile = TestFile
  { schema :: SchemaDescription
  , definitions :: Map Text SchemaDescription
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- This is primarily a encoding/decoding and translation test.
encodeDecodeTest :: forall (a :: Type). (DefinitionsFor (UnrollAll '[a]), HasBlueprintDefinition a) => Assertion
encodeDecodeTest = do
  -- Obtain schema in terms of Plutus Blueprint types.
  let defMap = deriveDefinitions @'[a]
      defRef = definitionRef @a @(UnrollAll '[a])
  -- Encode it to JSON so we can test encoding/decoding.
  let encodedDefinitions = Aeson.encode $ definitionsToMap defMap Aeson.toJSON
      encodedRef = Aeson.encode defRef
  -- Translate the original Plutus Blueprint types into Ply types (for comparison later).
  ourSchemaRef <- maybe (errF defRef) pure $ descriptionFromPlutus defRef
  ourDefMap <- deriveSchemaDescriptions @'[a] errF
  -- The expected type is the directly translated Plutus Blueprint type (no encoding/decoding).
  expected <- either errF pure $ normalizeSchemaDescription ourDefMap ourSchemaRef
  -- Decode to Ply types.
  TestFile {schema, definitions} <- TestFile <$> eitherToFail (Aeson.eitherDecode encodedRef) <*> eitherToFail (Aeson.eitherDecode encodedDefinitions)
  actual <- either errF pure $ normalizeSchemaDescription definitions schema
  -- Ensure equality of decoded type with the translated type.
  assertBool ("\nExpected: " ++ show expected ++ "\nActual: " ++ show actual) $ actual == expected
  where
    errF sch = assertFailure $ "Unsupported Schema: " ++ show sch
    eitherToFail = either assertFailure pure

test :: TestTree
test =
  testGroup
    "schema-unit-tests"
    [ testCase "int" $ encodeDecodeTest @Int
    , testCase "integer" $ encodeDecodeTest @Integer
    , testCase "builtin-bytestring" $ encodeDecodeTest @BuiltinByteString
    , testCase "builtin-data" $ encodeDecodeTest @BuiltinData
    , testCase "rational" $ encodeDecodeTest @PlutusTx.Rational
    , testCase "value" $ encodeDecodeTest @Value
    , testCase "credential" $ encodeDecodeTest @Credential
    , testCase "staking-credential" $ encodeDecodeTest @StakingCredential
    , testCase "address" $ encodeDecodeTest @Address
    , testCase "currency-symbol" $ encodeDecodeTest @CurrencySymbol
    , testCase "token-name" $ encodeDecodeTest @TokenName
    , testCase "asset-class" $ encodeDecodeTest @AssetClass
    , testCase "pubkeyhash" $ encodeDecodeTest @PubKeyHash
    , testCase "posixtime" $ encodeDecodeTest @POSIXTime
    , testCase "interval" $ encodeDecodeTest @(Interval POSIXTime)
    , testCase "txid" $ encodeDecodeTest @TxId
    , testCase "txoutref" $ encodeDecodeTest @TxOutRef
    , testCase "script-hash" $ encodeDecodeTest @ScriptHash
    , testCase "datum-hash" $ encodeDecodeTest @DatumHash
    , testCase "redeemer-hash" $ encodeDecodeTest @RedeemerHash
    ]
