module Ply.Core.Serialize (
  writeEnvelope,
) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS

import Ply.Core.Types (TypedScriptEnvelope)

-- | Write a `TypedScriptEnvelope` into the given filepath.
writeEnvelope :: FilePath -> TypedScriptEnvelope -> IO ()
writeEnvelope filepath = LBS.writeFile filepath . encodePretty
