module Ply.Core.Deserialize (readBlueprint) where

import Control.Exception (throwIO)
import qualified Data.ByteString as BS

import qualified Data.Aeson as Aeson

import Ply.Core.Types (
  ScriptReaderException (ScriptParseException),
  TypedBlueprint,
 )

-- | Read a 'TypedBlueprint'. Throws 'ScriptReaderException'.
readBlueprint :: FilePath -> IO TypedBlueprint
readBlueprint path = do
  content <- BS.readFile path
  either (throwIO . ScriptParseException) pure $
    Aeson.eitherDecodeStrict' content
