module Ply.Types
  ( TypedScript(..)
  , toPlutusScript
  , TypedScriptEnvelope(..)
  , ScriptRole(..)
  , ValidatorRole
  , MintingPolicyRole
  , ScriptVersion
  , fromLanguage
  , toLanguage
  , PlyError(..)
  , PlyError(..)
  , AsData(..)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , stringifyAeson
  , JsonDecodeError(..)
  , caseAesonString
  )

import Contract.Scripts (ApplyArgsError)
import Ctl.Internal.Types.Scripts (Language(..))
import Contract.Address (ByteArray)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Contract.Scripts (PlutusScript)
import Ply.TypeList (TyList)

data AsData a = AsData a

-- | Equivalent to `TypedScript` in `ply-core` except
-- | this Purescript version handles error as well
data TypedScript :: ScriptRole -> TyList Type -> Type
data TypedScript role params = TypedScriptConstr (Either PlyError PlutusScript)

type role TypedScript nominal nominal

derive instance Generic (TypedScript role params) _
instance Show (TypedScript role params) where
  show = genericShow

-- | Aquire `PlutusScript` from `TypedScript`. Forgets all type information.
toPlutusScript
  :: forall role params
   . TypedScript role params
  -> Either PlyError PlutusScript
toPlutusScript (TypedScriptConstr ts) = ts

-- | Equivalent to `TypedScriptEnvelope` in `ply-core`
newtype TypedScriptEnvelope =
  TypedScriptEnvelope
    { script :: PlutusScript
    , role :: ScriptRole
    , params :: Array String
    , description :: String
    }

derive instance Eq TypedScriptEnvelope
derive instance Generic TypedScriptEnvelope _
derive instance Newtype TypedScriptEnvelope _
instance Show TypedScriptEnvelope where
  show = genericShow

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
      -> { script :: PlutusScript
         , role :: ScriptRole
         , description :: String
         , params :: Array String
         }
    modifyFields mp =
      { script: wrap (mp.rawHex /\ toLanguage mp.version)
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
instance Show ScriptRole where
  show = genericShow

instance DecodeAeson ScriptRole where
  decodeAeson x =
    caseAesonString
      (Left $ TypeMismatch ("Expecting string, but got: " <> stringifyAeson x))
      ( case _ of
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
instance Show ScriptVersion where
  show = genericShow

instance DecodeAeson ScriptVersion where
  decodeAeson x =
    caseAesonString
      (Left $ TypeMismatch ("Expecting string, but got: " <> stringifyAeson x))
      ( case _ of
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

fromLanguage :: Language -> ScriptVersion
fromLanguage PlutusV1 = ScriptV1
fromLanguage PlutusV2 = ScriptV2

data PlyError
  = RoleMismatch { expected :: ScriptRole, actual :: ScriptRole }
  | ParamsMismatch { expected :: Array String, actual :: Array String }
  | ApplicationError ApplyArgsError

derive instance Generic PlyError _
instance Show PlyError where
  show = genericShow
