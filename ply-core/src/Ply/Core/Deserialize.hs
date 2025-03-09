module Ply.Core.Deserialize (readBlueprint) where

import Control.Exception (throwIO)
import qualified Data.ByteString as BS

import qualified Data.Aeson as Aeson

import Ply.Core.Types (
  ScriptReaderException (AesonDecodeError),
  TypedBlueprint,
 )

-- | Read a 'TypedBlueprint'.
readBlueprint :: FilePath -> IO TypedBlueprint
readBlueprint path = do
  content <- BS.readFile path
  either (throwIO . AesonDecodeError) pure $
    Aeson.eitherDecodeStrict' content
