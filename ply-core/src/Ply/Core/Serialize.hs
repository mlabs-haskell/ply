module Ply.Core.Serialize (writeEnvelope) where

import Codec.Serialise (serialise)
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified Cardano.Binary as CBOR
import Plutus.V1.Ledger.Scripts (Script)

import Ply.Core.Types (ScriptRole, ScriptVersion (ScriptV1), Typename)

-- | Write a typed script into a 'TypedScriptEnvelope'.
writeEnvelope ::
  -- | Description for the script (semantically irrelevant).
  Text ->
  -- | Path to write the file to.
  FilePath ->
  -- | Whether the script is a validator or a minting policy.
  ScriptRole ->
  -- | The extra parameter types for the script.
  [Typename] ->
  -- | The script itself.
  Script ->
  IO ()
writeEnvelope descr filepath scrptRole paramTypes scrpt = do
  let scriptSBS = SBS.toShort . LBS.toStrict . serialise $ scrpt
      scriptRawCBOR = CBOR.serialize' scriptSBS
      plutusJson =
        object
          [ "version" .= ScriptV1
          , "role" .= scrptRole
          , "params" .= paramTypes
          , "description" .= descr
          , "cborHex" .= Text.decodeUtf8 (Base16.encode scriptRawCBOR)
          ]
      content = encodePretty plutusJson
  LBS.writeFile filepath content
