module Ply.Core.Serialize (
  writeEnvelope,
  writeTypedScriptEnvolope,
  serializeScriptCborHex,
) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Txt

import Ply.Core.Types (
  ScriptRole,
  ScriptVersion,
  TypedScriptEnvelope (..),
  Typename,
  serializeScriptCbor,
 )
import Ply.LedgerExports.Common (Script)

-- | A showable hex string representing a serialized script in CBOR.
serializeScriptCborHex :: Script -> Text
serializeScriptCborHex = Txt.decodeUtf8 . Base16.encode . serializeScriptCbor

-- | Write 'TypedScriptEnvelop' into file.
writeTypedScriptEnvolope :: FilePath -> TypedScriptEnvelope -> IO ()
writeTypedScriptEnvolope filepath =
  LBS.writeFile filepath . encodePretty

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
writeEnvelope descr filepath scrptVer scrptRole paramTypes scrpt =
  let plutusEnvelope =
        TypedScriptEnvelope scrptVer scrptRole paramTypes descr scrpt
   in writeTypedScriptEnvolope filepath plutusEnvelope
