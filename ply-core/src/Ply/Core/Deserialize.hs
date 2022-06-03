module Ply.Core.Deserialize (readEnvelope) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS

import Codec.Serialise (deserialise)
import qualified Data.Aeson as Aeson

import Cardano.Binary (DecoderError, FromCBOR (fromCBOR))
import qualified Cardano.Binary as CBOR
import qualified Plutus.V1.Ledger.Scripts as PlutusScript

import Ply.Core.Types (
  ScriptReaderException (AesonDecodeError, CBORDecodeError),
  TypedScriptEnvelope (TypedScriptEnvelope),
  TypedScriptEnvelope' (TypedScriptEnvelope'),
 )

-- | Read a 'TypedScriptEnvelope'.
readEnvelope :: FilePath -> IO TypedScriptEnvelope
readEnvelope path = do
  content <- BS.readFile path
  TypedScriptEnvelope' ver rol params desc rawCbor <-
    either (throwIO . AesonDecodeError) pure $
      Aeson.eitherDecodeStrict' content
  scrpt <- either (throwIO . CBORDecodeError) pure $ cborToScript rawCbor
  pure $ TypedScriptEnvelope ver rol params desc scrpt

cborToScript :: ByteString -> Either DecoderError PlutusScript.Script
cborToScript x = deserialise . LBS.fromStrict . SBS.fromShort . getSerializedScript <$> CBOR.decodeFull' x

newtype SerializedScript = SerializedScript {getSerializedScript :: ShortByteString}

instance FromCBOR SerializedScript where
  fromCBOR = SerializedScript . SBS.toShort <$> CBOR.fromCBOR
