{-# LANGUAGE AllowAmbiguousTypes #-}

module Ply.Core.Types (
  TypedScript (..),
  ScriptVersion (..),
  ScriptRole (..),
  ScriptReaderException (..),
  TypedScriptEnvelope' (..),
  TypedScriptEnvelope (..),
  Typename,
  typeName,
) where

import Control.Exception (Exception)
import Control.Monad (when)
import Data.Aeson (object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Text
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import GHC.Generics (Generic)

import Data.Aeson.Types (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Object, String),
  prependFailure,
  typeMismatch,
  unexpected,
  (.:),
 )

import Cardano.Binary (DecoderError)
#if PLUTUS_NEW
import PlutusLedgerApi.V1.Scripts (Script)
#else
import Plutus.V1.Ledger.Scripts (Script)
#endif
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

-- | Compiled scripts that preserve script role and parameter types.
type role TypedScript nominal nominal

type TypedScript :: ScriptRole -> [Type] -> Type
newtype TypedScript r a = TypedScript (Program DeBruijn DefaultUni DefaultFun ())
  deriving stock (Show)

-- | Script role: either a validator or a minting policy.
data ScriptRole = ValidatorRole | MintingPolicyRole
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Errors/Exceptions that may arise during Typed Script reading/parsing.
data ScriptReaderException
  = AesonDecodeError String
  | CBORDecodeError DecoderError
  | ScriptRoleError {expectedRole :: ScriptRole, actualRole :: ScriptRole}
  | ScriptTypeError {expectedType :: [Typename], actualType :: [Typename]}
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | JSON schema of the envelope we'll be using to represent typed scripts in the filesystem,
data TypedScriptEnvelope' = TypedScriptEnvelope'
  { -- | Plutus script version.
    tsVersion' :: !ScriptVersion
  , -- | Plutus script role, either a validator or a minting policy.
    tsRole' :: !ScriptRole
  , -- | List of extra parameter types to be applied before being treated as a validator/minting policy.
    tsParamTypes' :: [Typename]
  , -- | Description of the script, not semantically relevant.
    tsDescription' :: !Text
  , -- | The actual script in serialized CBOR form.
    tsCbor' :: !ByteString
  , -- | The actual script in raw serialized form.
    tsRaw' :: !ByteString
  }
  deriving stock (Eq, Show)

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
    tsScript :: !Script
  }
  deriving stock (Eq, Show)

instance FromJSON TypedScriptEnvelope' where
  parseJSON (Object v) =
    TypedScriptEnvelope'
      <$> v .: "version"
      <*> v .: "role"
      <*> v .: "params"
      <*> v .: "description"
      <*> (parseJSONBase16 =<< v .: "cborHex")
      <*> (parseJSONBase16 =<< v .: "rawHex")
    where
      parseJSONBase16 v =
        either fail pure . Base16.decode . Text.encodeUtf8 =<< parseJSON v
  parseJSON invalid =
    prependFailure
      "parsing TypedScriptEnvelope' failed, "
      (typeMismatch "Object" invalid)

instance ToJSON TypedScriptEnvelope' where
  toJSON (TypedScriptEnvelope' ver rol params desc cborHex rawHex) =
    toJSON $
      object
        [ "version" .= ver
        , "role" .= rol
        , "params" .= params
        , "description" .= desc
        , "cborHex" .= Text.decodeUtf8 (Base16.encode cborHex)
        , "rawHex" .= Text.decodeUtf8 (Base16.encode rawHex)
        ]

-- | Version identifier for the Plutus script.
data ScriptVersion = ScriptV1 | ScriptV2
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Alphanumeric type names as per Haskell syntax. Does not support type operators.
newtype Typename = Typename Text
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)

-- | Obtain the 'Typename' for a given type.
typeName :: forall a. Typeable a => Typename
typeName = Typename . Txt.pack . show $ typeRep (Proxy @a)

-- FIXME: Must have stricter parsing rules.
instance FromJSON Typename where
  parseJSON (String t) = do
    when (Txt.null t) $ fail "Typename cannot be empty"
    pure $ Typename t
  parseJSON x = unexpected x
