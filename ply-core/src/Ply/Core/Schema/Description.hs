{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Ply.Core.Schema.Description (
  SchemaDescription (..),
  descriptionFromPlutus,
) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (Foldable (toList))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtEnc
import Data.Traversable (for)

import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Aeson.Key (toString)
import Data.Aeson.KeyMap ((!?))
import Data.Aeson.Types (Parser)
import PlutusTx.Blueprint (ConstructorSchema (MkConstructorSchema), DefinitionId (definitionIdToText), ListSchema (MkListSchema), MapSchema (MkMapSchema), PairSchema (MkPairSchema), Schema (SchemaAnyOf, SchemaBuiltInBoolean, SchemaBuiltInBytes, SchemaBuiltInInteger, SchemaBuiltInList, SchemaBuiltInPair, SchemaBuiltInString, SchemaBuiltInUnit, SchemaBytes, SchemaConstructor, SchemaDefinitionRef, SchemaInteger, SchemaList, SchemaMap, SchemaOneOf), fieldSchemas, index, itemSchema, keySchema, left, right, valueSchema)

refPrefix :: Text
refPrefix = "#/definitions/"

-- TODO: Support CIP-57 validation properties.

-- | A condensed description of CIP57 schema catered around PlutusCore types.
data SchemaDescription
  = SimpleType Text
  | ListType SchemaDescription
  | PairType SchemaDescription SchemaDescription
  | DataListType SchemaDescription
  | MapType SchemaDescription SchemaDescription
  | ConstrType (NonEmpty [SchemaDescription])
  | -- Reference to a schema definition from the top-level `definitions` field.
    SchemaRef Text
  deriving stock (Eq)

descriptionFromPlutus :: Schema referencedTypes -> Maybe SchemaDescription
descriptionFromPlutus (SchemaBuiltInUnit _) = Just $ SimpleType "#unit"
descriptionFromPlutus (SchemaBuiltInBoolean _) = Just $ SimpleType "#boolean"
descriptionFromPlutus (SchemaBuiltInInteger _) = Just $ SimpleType "#integer"
descriptionFromPlutus (SchemaBuiltInString _) = Just $ SimpleType "#string"
descriptionFromPlutus (SchemaBuiltInBytes _) = Just $ SimpleType "#bytes"
descriptionFromPlutus (SchemaInteger _ _) = Just $ SimpleType "integer"
descriptionFromPlutus (SchemaBytes _ _) = Just $ SimpleType "bytes"
descriptionFromPlutus (SchemaBuiltInList _ itemSchema) = ListType <$> descriptionFromPlutus itemSchema
descriptionFromPlutus (SchemaBuiltInPair _ MkPairSchema {left, right}) = PairType <$> descriptionFromPlutus left <*> descriptionFromPlutus right
descriptionFromPlutus (SchemaList _ MkListSchema {itemSchema}) = DataListType <$> descriptionFromPlutus itemSchema
descriptionFromPlutus (SchemaMap _ MkMapSchema {keySchema, valueSchema}) = MapType <$> descriptionFromPlutus keySchema <*> descriptionFromPlutus valueSchema
descriptionFromPlutus (SchemaConstructor _ MkConstructorSchema {fieldSchemas, index}) = if index /= 0 then Nothing else ConstrType . (:| []) <$> traverse descriptionFromPlutus fieldSchemas
descriptionFromPlutus (SchemaAnyOf variants) = ConstrType <$> variantFromPlutus variants
descriptionFromPlutus (SchemaOneOf variants) = ConstrType <$> variantFromPlutus variants
descriptionFromPlutus (SchemaDefinitionRef defId) = Just . SchemaRef $ definitionIdToText defId
descriptionFromPlutus _ = Nothing

variantFromPlutus :: NonEmpty (Schema referencedTypes) -> Maybe (NonEmpty [SchemaDescription])
variantFromPlutus variants
  | Just processedVariants <- traverse assertConstructor variants =
    let plutusFields = snd <$> sortOn fst (toList processedVariants)
     in traverse (traverse descriptionFromPlutus) $ NE.fromList plutusFields
  | otherwise = Nothing
  where
    assertConstructor (SchemaConstructor _ MkConstructorSchema {fieldSchemas, index}) = Just (index, fieldSchemas)
    assertConstructor _ = Nothing

instance Show SchemaDescription where
  show = Txt.unpack . TxtEnc.decodeUtf8Lenient . LBS.toStrict . Aeson.encodePretty

-- TODO: Implement toEncoding
instance ToJSON SchemaDescription where
  toJSON (SimpleType s) = Aeson.object ["dataType" .= s]
  -- CIP-57 essentially handicaps builtin lists and pairs. Their constituent types cannot be expressed.
  toJSON (ListType x) = Aeson.object ["dataType" .= Txt.pack "#list", "items" .= x]
  toJSON (PairType l r) = Aeson.object ["dataType" .= Txt.pack "#pair", "left" .= l, "right" .= r]
  toJSON (DataListType descr) = Aeson.object ["dataType" .= Txt.pack "list", "items" .= toJSON descr]
  toJSON (MapType descr1 descr2) = Aeson.object ["dataType" .= Txt.pack "map", "keys" .= toJSON descr1, "values" .= toJSON descr2]
  toJSON (ConstrType descrss) = Aeson.object ["oneOf" .= imap constrDescr (toList descrss)]
    where
      imap f = zipWith f [0 ..]
      constrDescr :: Int -> [SchemaDescription] -> Value
      constrDescr ix descrs = Aeson.object ["dataType" .= Txt.pack "constructor", "index" .= ix, "fields" .= map toJSON descrs]
  toJSON (SchemaRef name) = Aeson.object ["$ref" .= (refPrefix <> name)]

{- | Note: This instance ignores any extra keys that aren't necessary to parse a schema. As such, 'dataType' is the most important key.

If a dataType is found that doesn't require any other keys for identification (i.e simple types like #integer, #bytes, integer, bytes etc),
then even if there are other keys that _shouldn't_ be there (such as 'items' key in object with 'dataType': '#integer'), they are silently ignored.
-}
instance FromJSON SchemaDescription where
  parseJSON = Aeson.withObject "SchemaDescription" $ \obj ->
    refParser obj <|> multiConstrTypeParser obj <|> do
      res <- maybe (fail "Key 'dataType' not found") pure $ obj !? "dataType"
      Aeson.withText "Schema.dataType" (parseOtherSchema obj) res
    where
      refParser :: Aeson.Object -> Parser SchemaDescription
      refParser obj = assertKey obj "$ref" >>= Aeson.withText "Schema.$ref" parseRefName
      parseRefName :: Text -> Parser SchemaDescription
      parseRefName t = maybe (fail "Invalid Schema ref format") (pure . SchemaRef) $ Txt.stripPrefix refPrefix t
      multiConstrTypeParser :: Aeson.Object -> Parser SchemaDescription
      multiConstrTypeParser obj = assertAnyKey obj "oneOf" "anyOf" >>= Aeson.withArray "Schema.oneOf/anyOf" parseConstrVariants
      parseConstrVariants :: Aeson.Array -> Parser SchemaDescription
      parseConstrVariants (toList -> constrs) = do
        ivariants <-
          for constrs $
            Aeson.withObject "Schema.oneOf/anyOf[$]" parseConstrVariant
        let sortedVariants = map snd $ sortOn fst ivariants
        case NE.nonEmpty sortedVariants of
          Nothing -> fail "oneOf/anyOf combinator must contain at least one schema in the list value"
          Just x -> pure $ ConstrType x
      parseConstrVariant :: Aeson.Object -> Parser (Int, [SchemaDescription])
      parseConstrVariant obj = do
        res <- assertKey obj "dataType"
        let sanityF = Aeson.withText "Schema.dataType" $ \s ->
              unless (s == "constructor") $ fail "Schema.oneOf/anyOf[$].dataType must be constructor"
        sanityF res
        ix <- assertKey obj "index" >>= parseJSON @Int
        fields <- assertKey obj "fields" >>= Aeson.withArray "Schema.constructor[$].fields" (traverse parseJSON)
        pure (ix, toList fields)

      parseOtherSchema :: Aeson.Object -> Text -> Parser SchemaDescription
      parseOtherSchema obj ident
        | ident `elem` simpleIdents = pure $ SimpleType ident
        | otherwise = case ident of
          "#list" -> do
            res <- assertKey obj "items"
            ListType <$> parseJSON res
          "#pair" -> do
            res1 <- assertKey obj "left"
            res2 <- assertKey obj "right"
            PairType <$> parseJSON res1 <*> parseJSON res2
          "list" -> do
            res <- assertKey obj "items"
            DataListType <$> parseJSON res
          "map" -> do
            res1 <- assertKey obj "keys"
            res2 <- assertKey obj "values"
            MapType <$> parseJSON res1 <*> parseJSON res2
          "constructor" -> do
            -- This is the single variant case.
            fields <- assertKey obj "fields" >>= Aeson.withArray "Schema.constructor[$].fields" (traverse parseJSON)
            pure $ ConstrType [toList fields]
          _ -> fail $ "Unrecognized dataType: " ++ Txt.unpack ident

      simpleIdents :: [Text]
      simpleIdents = ["#integer", "#bytes", "#string", "#unit", "#boolean", "integer", "bytes"]
      assertKey :: Aeson.Object -> Aeson.Key -> Parser Aeson.Value
      assertKey obj key = maybe (fail $ "Key '" ++ toString key ++ "' not found") pure $ obj !? key
      assertAnyKey :: Aeson.Object -> Aeson.Key -> Aeson.Key -> Parser Aeson.Value
      assertAnyKey obj key1 key2 = do
        maybe (fail $ "Neither of the keys: '" ++ toString key1 ++ " or " ++ toString key2 ++ "' were found") pure $ (obj !? key1) <|> (obj !? key2)
