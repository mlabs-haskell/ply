module Ply.Core.Serialize (
  writeEnvelope,
  writeTypedScriptEnvolope,
  serializeScript,
  serializeScriptCbor,
  serializeScriptCborHex,
) where

import Codec.Serialise (serialise)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Txt

import qualified Cardano.Binary as CBOR

import Ply.Core.Types (
  ScriptRole,
  ScriptVersion,
  TypedScriptEnvelope (..),
  TypedScriptEnvelope' (..),
  Typename,
 )
import Ply.LedgerExports.Common (Script)

-- | Serialize a script into its raw form.
serializeScript :: Script -> ByteString
serializeScript = LBS.toStrict . serialise

-- | Serialize a script into CBOR.
serializeScriptCbor :: Script -> ByteString
serializeScriptCbor = CBOR.serialize' . SBS.toShort . serializeScript

-- | A showable hex string representing a serialized script in CBOR.
serializeScriptCborHex :: Script -> Text
serializeScriptCborHex = Txt.decodeUtf8 . Base16.encode . serializeScriptCbor

-- | Write 'TypedScriptEnvelop' into file.
writeTypedScriptEnvolope :: FilePath -> TypedScriptEnvelope -> IO ()
writeTypedScriptEnvolope filepath (TypedScriptEnvelope ver role param descr script) =
  writeEnvelope descr filepath ver role param script

-- | Write a typed script into a "Ply.Core.Types.TypedScriptEnvelope".
writeEnvelope ::
  -- | Description for the script (semantically irrelevant).
  Text ->
  -- | Path to write the file to.
  FilePath ->
  -- | Version of the script.
  ScriptVersion ->
  -- | Whether the script is a validator or a minting policy.
  ScriptRole ->
  -- | The extra parameter types for the script.
  [Typename] ->
  -- | The script itself.
  Script ->
  IO ()
writeEnvelope descr filepath scrptVer scrptRole paramTypes scrpt = do
  let plutusEnvelope =
        TypedScriptEnvelope'
          { tsVersion' = scrptVer
          , tsRole' = scrptRole
          , tsParamTypes' = paramTypes
          , tsDescription' = descr
          , tsCbor' = serializeScriptCbor scrpt
          , tsRaw' = serializeScript scrpt
          }
      content = encodePretty plutusEnvelope
  LBS.writeFile filepath content
