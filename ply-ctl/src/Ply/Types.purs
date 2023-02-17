module Ply.Types where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , stringifyAeson
  , JsonDecodeError (..)
  , caseAesonString
  )

import Ctl.Internal.Types.Scripts (Language (..))
import Contract.Address (ByteArray)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Either
import Data.Newtype
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Contract.Scripts
import Ply.TypeList

-- | Equivalent to `TypedScript` in `ply-core`
data TypedScript :: ScriptRole -> TyList Type -> Type
data TypedScript role params = TypedScriptConstr ScriptVersion ByteArray
type role TypedScript nominal nominal

derive instance Generic (TypedScript role params) _
instance Show (TypedScript role params) where show = genericShow

-- | Equivalent to `TypedScriptEnvelope` in `ply-core`
newtype TypedScriptEnvelope =
  TypedScriptEnvelope
  { script :: ByteArray
  , version :: ScriptVersion
  , role :: ScriptRole
  , params :: Array String
  , description :: String
  }

derive instance Eq TypedScriptEnvelope
derive instance Generic TypedScriptEnvelope _
derive instance Newtype TypedScriptEnvelope _
instance Show TypedScriptEnvelope where show = genericShow

instance DecodeAeson TypedScriptEnvelope where
  decodeAeson x = do
    row <- decodeAeson x
    pure $ wrap $ modifyFields row
    where
      modifyFields
        :: { rawHex :: ByteArray
           , version :: ScriptVersion
           , role :: ScriptRole
           , description :: String
           , params :: Array String
           }
        -> { script :: ByteArray
           , version :: ScriptVersion
           , role :: ScriptRole
           , description :: String
           , params :: Array String
           }
      modifyFields mp =
        { script: mp.rawHex
        , version: mp.version
        , role: mp.role
        , description: mp.description
        , params: mp.params
        }

instance EncodeAeson TypedScriptEnvelope where
  encodeAeson (TypedScriptEnvelope row) =
    encodeAeson row

-- | Equivalent to `ScriptRole` in `ply-core`
data ScriptRole = ValidatorRole | MintingPolicyRole

foreign import data ValidatorRole :: ScriptRole
foreign import data MintingPolicyRole :: ScriptRole

derive instance Eq ScriptRole
derive instance Generic ScriptRole _
instance Show ScriptRole where show = genericShow

instance DecodeAeson ScriptRole where
  decodeAeson x =
    caseAesonString
      (Left $ TypeMismatch ("Expecting string, but got: " <> stringifyAeson x))
      (case _ of
          "ValidatorRole" -> Right ValidatorRole
          "MintingPolicyRole" -> Right MintingPolicyRole
          s -> Left $ TypeMismatch ("Expecting ScriptRole, but got: " <> s)
          )
      x

instance EncodeAeson ScriptRole where
  encodeAeson ValidatorRole = encodeAeson "ValidatorRole"
  encodeAeson MintingPolicyRole = encodeAeson "MintingPolicyRole"

-- | Equivalent to `ScriptVersion` in `ply-core`
data ScriptVersion = ScriptV1 | ScriptV2

derive instance Eq ScriptVersion
derive instance Generic ScriptVersion _
instance Show ScriptVersion where show = genericShow

instance DecodeAeson ScriptVersion where
  decodeAeson x =
    caseAesonString
      (Left $ TypeMismatch ("Expecting string, but got: " <> stringifyAeson x))
      (case _ of
          "ScriptV1" -> Right ScriptV1
          "ScriptV2" -> Right ScriptV2
          s -> Left $ TypeMismatch ("Expecting ScriptVersion, but got: " <> s)
          )
      x

instance EncodeAeson ScriptVersion where
  encodeAeson ScriptV1 = encodeAeson "ScriptV1"
  encodeAeson ScriptV2 = encodeAeson "ScriptV2"

toLanguage :: ScriptVersion -> Language
toLanguage ScriptV1 = PlutusV1
toLanguage ScriptV2 = PlutusV2
