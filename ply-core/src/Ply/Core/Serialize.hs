module Ply.Core.Serialize (writeEnvelope) where

import Codec.Serialise (serialise)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Text (Text)

import qualified Cardano.Binary as CBOR
import Plutus.V1.Ledger.Scripts (Script)

import Ply.Core.Types (
  ScriptRole,
  ScriptVersion (ScriptV1),
  TypedScriptEnvelope' (..),
  Typename,
 )

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
  let scriptRaw = LBS.toStrict . serialise $ scrpt
      scriptRawCBOR = CBOR.serialize' . SBS.toShort $ scriptRaw
      plutusEnvelope =
        TypedScriptEnvelope'
          { tsVersion' = ScriptV1
          , tsRole' = scrptRole
          , tsParamTypes' = paramTypes
          , tsDescription' = descr
          , tsCbor' = scriptRawCBOR
          , tsRaw' = scriptRaw
          }
      content = encodePretty plutusEnvelope
  LBS.writeFile filepath content
