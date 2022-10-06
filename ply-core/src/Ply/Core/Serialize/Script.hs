module Ply.Core.Serialize.Script (
  serializeScript,
  serializeScriptCbor,
  serializeScriptCborHex,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Txt

import Codec.Serialise (serialise)

import qualified Cardano.Binary as CBOR

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
