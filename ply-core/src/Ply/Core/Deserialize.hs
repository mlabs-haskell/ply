module Ply.Core.Deserialize (readEnvelope) where

import Control.Exception (throwIO)
import qualified Data.ByteString as BS

import qualified Data.Aeson as Aeson

import Ply.Core.Types (
  ScriptReaderException (AesonDecodeError),
  TypedScriptEnvelope,
 )

-- | Read a 'TypedScriptEnvelope'.
readEnvelope :: FilePath -> IO TypedScriptEnvelope
readEnvelope path = do
  content <- BS.readFile path
  either (throwIO . AesonDecodeError) pure $
    Aeson.eitherDecodeStrict' content
