module Unit.Schema (test) where

import Control.Exception (AssertionFailed (AssertionFailed), throwIO)
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson

import Ply.Core.Schema (
  SchemaDescription (ConstrType, SimpleType),
  normalizeSchemaDescription,
 )

data TestFile = TestFile
  { schema :: SchemaDescription
  , definitions :: Map Text SchemaDescription
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

test :: IO ()
test = do
  res <- Aeson.eitherDecodeFileStrict' @TestFile "test/schemas/001.json"
  TestFile {schema, definitions} <- either (\e -> fail $ "Error: " ++ e) pure res
  let normalizeRes = normalizeSchemaDescription definitions schema
  let expected = ConstrType $ NE.singleton [ConstrType $ [] :| [[]], SimpleType "integer"]
  unless (normalizeRes == Just expected) $
    throwIO . AssertionFailed $ "Expected: " ++ show expected ++ "\nActual: " ++ show normalizeRes
