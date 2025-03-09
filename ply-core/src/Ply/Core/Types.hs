{-# LANGUAGE TemplateHaskell #-}

module Ply.Core.Types (
  TypedBlueprint (..),
  TypedBlueprintPreamble (..),
  TypedScriptBlueprint (..),
  TypedScriptBlueprintParameter (..),
  TypedScript (..),
  ScriptRole (..),
  ScriptReaderException (..),
  SchemaDescription,
  UPLCProgram,
  UPLCProgramJSON (..),
  PlutusVersionJSON (..),
  AsData (..),
) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)

import Data.Aeson (Options (fieldLabelModifier))
import qualified Data.Aeson as Aeson
import Data.Aeson.Extra (stripPrefix)
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.Aeson.Types (FromJSON (parseJSON))

import Cardano.Binary as CBOR (DecoderError)
import qualified Cardano.Binary as CBOR
import PlutusLedgerApi.Common (uncheckedDeserialiseUPLC)
import PlutusTx.Blueprint (PlutusVersion (PlutusV1, PlutusV2, PlutusV3), Schema)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

import Ply.Core.Schema (SchemaDescription)

-- | Wrapper for anytypes that is data encoded.
newtype AsData a = AsData a

type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

-- | Compiled scripts that preserve script role and parameter types.
type role TypedScript nominal nominal

type TypedScript :: ScriptRole -> [Type] -> Type
data TypedScript r a = TypedScriptConstr !PlutusVersion !UPLCProgram
  deriving stock (Show)

-- | Script role: either a validator or a minting policy.
data ScriptRole = ValidatorRole | MintingPolicyRole
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON)

-- | Errors/Exceptions that may arise during Typed Script reading/parsing.
data ScriptReaderException where
  AesonDecodeError :: String -> ScriptReaderException
  UnsupportedSchema :: forall referencedTypes. Schema referencedTypes -> ScriptReaderException
  UndefinedReference :: {referenceName :: Text, targetSchema :: SchemaDescription, definitionsMap :: Map Text SchemaDescription} -> ScriptReaderException
  ScriptTypeError :: {expectedType :: SchemaDescription, actualType :: SchemaDescription} -> ScriptReaderException

deriving stock instance Show ScriptReaderException
deriving anyclass instance Exception ScriptReaderException

newtype PlutusVersionJSON = PlutusVersionJSON PlutusVersion

instance FromJSON PlutusVersionJSON where
  parseJSON = Aeson.withText "PlutusVersion" $ \case
    "v1" -> pure $ PlutusVersionJSON PlutusV1
    "v2" -> pure $ PlutusVersionJSON PlutusV2
    "v3" -> pure $ PlutusVersionJSON PlutusV3
    _ -> fail "Expected one of: v1, v2, v3"

newtype UPLCProgramJSON = UPLCProgramJSON UPLCProgram

instance FromJSON UPLCProgramJSON where
  parseJSON v =
    fmap UPLCProgramJSON $
      parseJSON v
        >>= either fail (either (fail . show) pure . cborToScript)
          . Base16.decode
          . Text.encodeUtf8

cborToScript :: ByteString -> Either DecoderError UPLCProgram
cborToScript x = uncheckedDeserialiseUPLC <$> CBOR.decodeFull' x

data TypedScriptBlueprintParameter = TypedScriptBlueprintParameter
  { tsbpSchema :: !SchemaDescription
  }
  deriving stock (Generic)

data TypedScriptBlueprint = TypedScriptBlueprint
  { tsbTitle :: !Text
  , tsbParameters :: ![TypedScriptBlueprintParameter]
  , tsbCompiledCode :: !UPLCProgramJSON
  }
  deriving stock (Generic)

data TypedBlueprintPreamble = TypedBlueprintPreamble
  { tbpPlutusVersion :: !PlutusVersionJSON
  }
  deriving stock (Generic)

data TypedBlueprint = TypedBlueprint
  { tbPreamble :: !TypedBlueprintPreamble
  , tbValidators :: ![TypedScriptBlueprint]
  , tbDefinitions :: !(Map Text SchemaDescription)
  }

$(deriveFromJSON defaultOptions {fieldLabelModifier = stripPrefix "tsbp"} ''TypedScriptBlueprintParameter)
$(deriveFromJSON defaultOptions {fieldLabelModifier = stripPrefix "tsb"} ''TypedScriptBlueprint)
$(deriveFromJSON defaultOptions {fieldLabelModifier = stripPrefix "tbp"} ''TypedBlueprintPreamble)
$(deriveFromJSON defaultOptions {fieldLabelModifier = stripPrefix "tb"} ''TypedBlueprint)
