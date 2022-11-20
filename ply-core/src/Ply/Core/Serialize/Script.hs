module Ply.Core.Serialize.Script (
  serializeScriptCbor,
  serializeScriptCborHex,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Text (Text)
import qualified Data.Text.Encoding as Txt

import qualified Cardano.Binary as CBOR

import PlutusLedgerApi.Common (SerialisedScript)

-- | Serialize a script into CBOR.
serializeScriptCbor :: SerialisedScript -> ByteString
serializeScriptCbor = CBOR.serialize'

-- | A showable hex string representing a serialized script in CBOR.
serializeScriptCborHex :: SerialisedScript -> Text
serializeScriptCborHex = Txt.decodeUtf8 . Base16.encode . serializeScriptCbor
