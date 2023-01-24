{-# LANGUAGE AllowAmbiguousTypes #-}

module Ply.Core.Types (
  TypedScript (..),
  ScriptVersion (..),
  ScriptRole (..),
  ScriptReaderException (..),
  TypedScriptEnvelope (..),
  Typename (Typename),
  UPLCProgram,
) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)

import Data.Aeson (object, (.=))
import Data.Aeson.Types (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Object),
  prependFailure,
  typeMismatch,
  (.:),
 )

import Cardano.Binary as CBOR (DecoderError)
import qualified Cardano.Binary as CBOR

import PlutusLedgerApi.Common (deserialiseUPLC, serialiseUPLC)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

import Ply.Core.Serialize.Script (serializeScriptCbor)
import Ply.Core.Typename (Typename (Typename))

type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

-- | Compiled scripts that preserve script role and parameter types.
type role TypedScript nominal nominal

type TypedScript :: ScriptRole -> [Type] -> Type
data TypedScript r a = TypedScriptConstr !ScriptVersion !UPLCProgram
  deriving stock (Show)

-- | Script role: either a validator or a minting policy.
data ScriptRole = ValidatorRole | MintingPolicyRole
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Version identifier for the Plutus script.
data ScriptVersion = ScriptV1 | ScriptV2
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Errors/Exceptions that may arise during Typed Script reading/parsing.
data ScriptReaderException
  = AesonDecodeError String
  | ScriptRoleError {expectedRole :: ScriptRole, actualRole :: ScriptRole}
  | ScriptTypeError {expectedType :: [Typename], actualType :: [Typename]}
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | This is essentially a post-processed version of 'TypedScriptEnvelope''.
data TypedScriptEnvelope = TypedScriptEnvelope
  { -- | Plutus script version.
    tsVersion :: !ScriptVersion
  , -- | Plutus script role, either a validator or a minting policy.
    tsRole :: !ScriptRole
  , -- | List of extra parameter types to be applied before being treated as a validator/minting policy.
    tsParamTypes :: [Typename]
  , -- | Description of the script, not semantically relevant.
    tsDescription :: !Text
  , -- | The actual script.
    tsScript :: !UPLCProgram
  }
  deriving stock (Eq, Show)

cborToScript :: ByteString -> Either DecoderError UPLCProgram
cborToScript x = deserialiseUPLC <$> CBOR.decodeFull' x

instance FromJSON TypedScriptEnvelope where
  parseJSON (Object v) =
    TypedScriptEnvelope
      <$> v .: "version"
      <*> v .: "role"
      <*> v .: "params"
      <*> v .: "description"
      <*> (parseAndDeserialize =<< v .: "cborHex")
    where
      parseAndDeserialize v =
        parseJSON v
          >>= either fail (either (fail . show) pure . cborToScript)
            . Base16.decode
            . Text.encodeUtf8
  parseJSON invalid =
    prependFailure
      "parsing TypedScriptEnvelope' failed, "
      (typeMismatch "Object" invalid)

instance ToJSON TypedScriptEnvelope where
  toJSON (TypedScriptEnvelope ver rol params desc script) =
    toJSON $
      object
        [ "version" .= ver
        , "role" .= rol
        , "params" .= params
        , "description" .= desc
        , "cborHex" .= Text.decodeUtf8 (Base16.encode cborHex)
        , "rawHex" .= Text.decodeUtf8 (Base16.encode rawHex)
        ]
    where
      serializedScript = serialiseUPLC script
      cborHex = serializeScriptCbor serializedScript
      rawHex = SBS.fromShort serializedScript
