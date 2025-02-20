{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Schema.Description (SchemaDescription (..), HasSchemaDescription (..), HasDataSchemaDescription (..), schemaDescrOf') where

import Control.Applicative ((<|>))
import Data.Kind (Constraint)
import Data.Text (Text)
import qualified Data.Text as Txt

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap ((!?))
import Data.Aeson.Types (Parser)
import Data.SOP (All2, K (K), POP, Proxy (Proxy))
import Data.SOP.NP (collapse_POP, cpure_POP)

import Control.Monad (unless)
import Data.Aeson.Key (toString)
import Data.Foldable (Foldable (toList))
import Data.List (sortOn)
import Data.Traversable (for)
import Ply.Core.Schema.Types (
  PlyDataSchema (PlyDB, PlyDI, PlyDL, PlyDM, PlyDS),
  PlySchema (PlyBool, PlyByteStr, PlyD, PlyInt, PlyListOf, PlyPairOf, PlyStr, PlyUnit),
 )

data SchemaDescription
  = SimpleType Text
  | ListType SchemaDescription
  | PairType SchemaDescription SchemaDescription
  | DataListType SchemaDescription
  | MapType SchemaDescription SchemaDescription
  | ConstrType [[SchemaDescription]]
  | -- This exists only because CIP-57 leaves certain types (e.g builtin list constituents) expressed as unknown data types.
    AnyDataType
  deriving stock (Eq)

instance Show SchemaDescription where
  show = show . Aeson.encode

-- TODO: Implement toEncoding
instance ToJSON SchemaDescription where
  toJSON (SimpleType s) = Aeson.object ["dataType" .= s]
  -- CIP-57 essentially handicaps builtin lists and pairs. Their constituent types cannot be expressed.
  toJSON (ListType _) = Aeson.object ["dataType" .= Txt.pack "#list"]
  toJSON (PairType _ _) = Aeson.object ["dataType" .= Txt.pack "#pair"]
  toJSON (DataListType descr) = Aeson.object ["dataType" .= Txt.pack "list", "items" .= toJSON descr]
  toJSON (MapType descr1 descr2) = Aeson.object ["dataType" .= Txt.pack "map", "keys" .= toJSON descr1, "values" .= toJSON descr2]
  toJSON (ConstrType descrss) = Aeson.object ["anyOf" .= imap constrDescr descrss]
    where
      imap f = zipWith f [0 ..]
      constrDescr :: Int -> [SchemaDescription] -> Value
      constrDescr ix descrs = Aeson.object ["dataType" .= Txt.pack "constructor", "index" .= ix, "fields" .= map toJSON descrs]
  toJSON AnyDataType = error "absurd: Ply should never produce AnyDataType SchemaDescription"

{- | Note: This instance ignores any extra keys that aren't necessary to parse a schema. As such, 'dataType' is the most important key.

If a dataType is found that doesn't require any other keys for identification (i.e simple types like #integer, #bytes, integer, bytes etc),
then even if there are other keys that _shouldn't_ be there (such as 'items' key in object with 'dataType': '#integer'), they are silently ignored.
-}
instance FromJSON SchemaDescription where
  parseJSON = Aeson.withObject "SchemaDescription" $ \obj ->
    constrTypeParser obj <|> do
      res <- maybe (fail "Key 'dataType' not found") pure $ obj !? "dataType"
      Aeson.withText "Schema.dataType" (parseOtherSchema obj) res
    where
      constrTypeParser :: Aeson.Object -> Parser SchemaDescription
      constrTypeParser obj = assertKey obj "anyOf" >>= Aeson.withArray "Schema.anyOf" parseConstrVariants
      parseConstrVariants :: Aeson.Array -> Parser SchemaDescription
      parseConstrVariants (toList -> constrs) = do
        ivariants <-
          for constrs $
            Aeson.withObject "Schema.anyOf[$]" parseConstrVariant
        let sortedVariants = map snd $ sortOn fst ivariants
        pure $ ConstrType sortedVariants
      parseConstrVariant :: Aeson.Object -> Parser (Int, [SchemaDescription])
      parseConstrVariant obj = do
        res <- assertKey obj "dataType"
        let sanityF = Aeson.withText "Schema.dataType" $ \s ->
              unless (s == "constructor") $ fail "Schema.anyOf[$].dataType must be constructor"
        sanityF res
        ix <- assertKey obj "index" >>= parseJSON @Int
        fields <- assertKey obj "fields" >>= Aeson.withArray "Schema.anyOf[$].fields" (traverse parseJSON)
        pure (ix, toList fields)

      parseOtherSchema :: Aeson.Object -> Text -> Parser SchemaDescription
      parseOtherSchema obj ident
        | ident `elem` simpleIdents = pure $ SimpleType ident
        | otherwise = case ident of
          -- Unfortunately, builtin lists or pairs don't have their constituents expressed.
          "#list" -> pure $ ListType AnyDataType
          "#pair" -> pure $ PairType AnyDataType AnyDataType
          "list" -> do
            res <- assertKey obj "items"
            ListType <$> parseJSON res
          "map" -> do
            res1 <- assertKey obj "keys"
            res2 <- assertKey obj "values"
            MapType <$> parseJSON res1 <*> parseJSON res2
          _ -> fail $ "Unrecognized dataType: " ++ Txt.unpack ident

      simpleIdents :: [Text]
      simpleIdents = ["#integer", "#bytes", "#string", "#unit", "#boolean", "integer", "bytes"]
      assertKey :: Aeson.Object -> Aeson.Key -> Parser Aeson.Value
      assertKey obj key = maybe (fail $ "Key '" ++ toString key ++ "' not found") pure $ obj !? key

type HasSchemaDescription :: PlySchema -> Constraint
class HasSchemaDescription a where
  schemaDescrOf :: Proxy a -> SchemaDescription

schemaDescrOf' :: forall a. HasSchemaDescription a => SchemaDescription
schemaDescrOf' = schemaDescrOf $ Proxy @a

instance HasSchemaDescription PlyInt where
  schemaDescrOf _ = SimpleType "#integer"
instance HasSchemaDescription PlyByteStr where
  schemaDescrOf _ = SimpleType "#bytes"
instance HasSchemaDescription PlyStr where
  schemaDescrOf _ = SimpleType "#string"
instance HasSchemaDescription PlyUnit where
  schemaDescrOf _ = SimpleType "#unit"
instance HasSchemaDescription PlyBool where
  schemaDescrOf _ = SimpleType "#boolean"
instance HasSchemaDescription a => HasSchemaDescription (PlyListOf a) where
  schemaDescrOf _ = ListType $ schemaDescrOf' @a
instance (HasSchemaDescription a, HasSchemaDescription b) => HasSchemaDescription (PlyPairOf a b) where
  schemaDescrOf _ = PairType (schemaDescrOf' @a) (schemaDescrOf' @b)
instance HasDataSchemaDescription d => HasSchemaDescription (PlyD d) where
  schemaDescrOf _ = dataSchemaDescrOf' @d

type HasDataSchemaDescription :: PlyDataSchema -> Constraint
class HasDataSchemaDescription d where
  dataSchemaDescrOf :: Proxy d -> SchemaDescription

instance HasDataSchemaDescription PlyDI where
  dataSchemaDescrOf _ = SimpleType "integer"
instance HasDataSchemaDescription PlyDB where
  dataSchemaDescrOf _ = SimpleType "bytes"
instance HasDataSchemaDescription a => HasDataSchemaDescription (PlyDL a) where
  dataSchemaDescrOf _ = DataListType $ dataSchemaDescrOf' @a
instance (HasDataSchemaDescription a, HasDataSchemaDescription b) => HasDataSchemaDescription (PlyDM a b) where
  dataSchemaDescrOf _ = MapType (dataSchemaDescrOf' @a) (dataSchemaDescrOf' @b)
instance (All2 HasDataSchemaDescription xss) => HasDataSchemaDescription (PlyDS xss) where
  dataSchemaDescrOf _ = ConstrType $ collapse_POP pop
    where
      pop :: POP (K SchemaDescription) xss
      pop = cpure_POP (Proxy @HasDataSchemaDescription) x
      x :: forall a. HasDataSchemaDescription a => K SchemaDescription a
      x = K (dataSchemaDescrOf' @a)

dataSchemaDescrOf' :: forall a. HasDataSchemaDescription a => SchemaDescription
dataSchemaDescrOf' = dataSchemaDescrOf $ Proxy @a
