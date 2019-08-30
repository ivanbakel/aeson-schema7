{-# LANGUAGE QuasiQuotes #-}
module Data.Aeson.Schema.V7.Parser
  ( parseSchema
  , ParserMonad
  ) where

import Data.Aeson.Schema.V7.Schema

import           Prelude hiding (const, id, minimum, maximum, not, unlines)
import qualified Prelude as P (not)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as Aeson.BE
import           Data.Aeson.BetterErrors ((<|>))
import           Data.Aeson.QQ (aesonQQ)
import           Data.Functor ((<&>))
import           Data.Functor.Identity (Identity)
import           Data.HashMap.Strict (fromList)
import           Data.List (nub)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, unlines)

parseSchema :: (ParserMonad m) => Aeson.Value -> m (Either ErrorMessage Schema)
parseSchema value
  = Aeson.BE.parseValueM asSchema value <&> \case
      Right schema -> Right schema
      Left parseError -> Left (unlines (Aeson.BE.displayError (\message -> message) parseError))

type ErrorMessage = Text

type Parser m a = Aeson.BE.ParseT ErrorMessage m a

class (Monad m) => ParserMonad m where
  warn :: Text -> Parser m ()
  err  :: Text -> Parser m a
  err = Aeson.BE.throwCustomError

instance ParserMonad Identity where
  warn _ignoreWarning = pure ()

eachInOneOrMany :: Monad m => Aeson.BE.ParseT err m a -> Aeson.BE.ParseT err m (OneOrMany a)
eachInOneOrMany innerParser
  = (One <$> innerParser)
    <|>
    (Many <$> Aeson.BE.eachInArray innerParser)

asType :: (ParserMonad m) => Parser m SchemaType
asType
  = Aeson.BE.asText >>= toSchemaType

  where
    toSchemaType = \case
      "string" -> pure StringType
      "integer" -> pure IntegerType
      "number" -> pure NumberType
      "object" -> pure ObjectType
      "array" -> pure ArrayType
      "boolean" -> pure BooleanType
      "null"    -> pure NullType
      other     -> err ("unknown type `" <> other <> "`")

-- TODO: Register key use, so reuse and nonuse are errs
useKey :: (ParserMonad m) => Text -> Parser m a -> Parser m (Maybe a)
useKey = Aeson.BE.keyMay

asStringSchema :: (ParserMonad m) => Parser m StringSchema
asStringSchema = do
  minLength <- useKey "minLength" asCount
  maxLength <- useKey "maxLength" asCount
  pattern   <- useKey "pattern" asPattern
  format    <- useKey "format" asFormat
  contentMediaType <- useKey "contentMediaType" asMediaType
  contentEncoding <- useKey "contentEncoding" asEncoding

  if badBounds minLength maxLength
    then warn "`minLength` is greater than `maxLength` - the schema is unsatisfiable!"
    else pure ()

  pure StringSchema{..}

asCount :: (ParserMonad m) => Parser m Count
asCount
  = Aeson.BE.asIntegral >>= \integer ->
      if integer >= 0
        then pure integer
        else err "this value cannot be negative"

asPattern :: (ParserMonad m) => Parser m Pattern
asPattern = Aeson.BE.asText

asFormat :: (ParserMonad m) => Parser m Format
asFormat = Aeson.BE.asText

asMediaType :: (ParserMonad m) => Parser m MediaType
asMediaType = Aeson.BE.asText

asEncoding :: (ParserMonad m) => Parser m Encoding
asEncoding
  = Aeson.BE.withText \case
     "7bit" -> Right SevenBit
     "8bit" -> Right EightBit
     "binary" -> Right Binary
     "quoted-printable" -> Right QuotedPrintable
     "base64" -> Right Base64
     other    -> Left ("unknown encoding `" <> other <> "`")

badBounds :: Maybe Count -> Maybe Count -> Bool
badBounds smaller bigger = fromMaybe False ((>) <$> smaller <*> bigger)

asNumberSchema :: (ParserMonad m) => Parser m NumberSchema
asNumberSchema = do
  multipleOf <- useKey "multipleOf" asNumber
  minimum    <- useKey "minimum" asNumber
  maximum    <- useKey "maximum" asNumber

  exclusiveMinimum <- useKey "exclusiveMinimum" asExclusive
  exclusiveMaximum <- useKey "exclusiveMaximum" asExclusive

  case (minimum, exclusiveMinimum) of
    (Just _, Nothing) -> pure ()
    (Nothing, Just (ExcludeBoundary _)) -> pure ()
    (Nothing, Nothing) -> pure ()

    (Just val, Just (ExcludeBoundary exclusiveVal)) ->
      warn $
        "Using both `minimum` and `exclusiveMinimum` is redundant - you should use"
        <> if val > exclusiveVal
            then pack (show [aesonQQ| { minimum: #{val} } |])
            else pack (show [aesonQQ| { exclusiveMinimum : #{exclusiveVal} } |])

    (Just val, Just (DoExclude exclude)) ->
      warn $
        "The boolean form of `exclusiveMinimum` is deprecated` - you should use"
        <> if exclude
              then pack (show [aesonQQ| { exclusiveMinimum : #{val} } |])
              else pack (show [aesonQQ| { minimum : #{val} } |])
    (Nothing, Just (DoExclude _)) ->
      warn $
        "The boolean form of `exclusiveMinimum` makes no sense without a value for\
        \ `minimum`, and will be ignored"

  case (maximum, exclusiveMaximum) of
    (Just _, Nothing) -> pure ()
    (Nothing, Just (ExcludeBoundary _)) -> pure ()
    (Nothing, Nothing) -> pure ()

    (Just val, Just (ExcludeBoundary exclusiveVal)) ->
      warn $
        "Using both `maximum` and `exclusiveMaximum` is redundant - you should use"
        <> if val < exclusiveVal
            then pack (show [aesonQQ| { maximum : #{val} } |])
            else pack (show [aesonQQ| { exclusiveMaximum : #{exclusiveVal} } |])

    (Just val, Just (DoExclude exclude)) ->
      warn $
        "The boolean form of `exclusiveMaximum` is deprecated` - you should use"
        <> if exclude
              then pack (show [aesonQQ| { exclusiveMaximum : #{val} } |])
              else pack (show [aesonQQ| { maximum : #{val} } |])
    (Nothing, Just (DoExclude _)) ->
      warn $
        "The boolean form of `exclusiveMaximum` makes no sense without a value for\
        \ `maximum`, and will be ignored"

  let numberSchema = NumberSchema{..}

  case buildRanges numberSchema of
    [] -> warn "This combination of bounds results in an empty range!"
    _  -> pure ()

  pure numberSchema

asNumber :: (ParserMonad m) => Parser m Number
asNumber = Aeson.BE.asScientific

asExclusive :: (ParserMonad m) => Parser m ExclusiveSchema
asExclusive
  = ExcludeBoundary <$> asNumber
    <|>
    DoExclude <$> Aeson.BE.asBool

asObjectSchema :: (ParserMonad m) => Parser m ObjectSchema
asObjectSchema = do
  properties <- useKey "properties" asPropertiesSchema
  additionalProperties <- useKey "additionalProperties" asSchema
  propertyNames <- useKey "propertyNames" asStringSchema
  patternProperties <- useKey "patternProperties" asPatternPropertiesSchema

  -- TODO: Check these against property names and pattern properties
  required <- useKey "required" (Aeson.BE.eachInArray asPropertyKey)

  minProperties <- useKey "minProperties" asCount
  maxProperties <- useKey "maxProperties" asCount

  if badBounds minProperties maxProperties
    then warn "`minProperties` is greater than `maxProperties` - the schema is unsatisfiable!"
    else pure ()

  if maxProperties < (length <$> required)
    then warn
          "There are more properties in `requiredProperties` than are allowed by\
          \ `maxProperties` - the schema is unsatisfiable!"
    else pure ()

  pure ObjectSchema{..}

asPropertiesSchema :: ParserMonad m => Parser m PropertiesSchema
asPropertiesSchema = do
  keySchemaPairs <- Aeson.BE.eachInObject asSchema

  pure (PropertiesSchema (fromList keySchemaPairs))

asPatternPropertiesSchema :: ParserMonad m => Parser m PatternPropertiesSchema
asPatternPropertiesSchema = do
  patternSchemaPairs <- Aeson.BE.eachInObject asSchema

  pure (PatternPropertiesSchema (fromList patternSchemaPairs))

asPropertyKey :: (ParserMonad m) => Parser m PropertyKey
asPropertyKey = Aeson.BE.asText

asArraySchema :: (ParserMonad m) => Parser m ArraySchema
asArraySchema = do
  items <- useKey "items" asItemsSchema
  contains <- useKey "contains" asSchema
  additionalItems <- useKey "additionalItems" asSchema

  case items of
    Just (ListSchema _) -> do
      case additionalItems of
        Nothing -> pure ()
        Just _  ->
          (warn "`additionalItems` is ignored when doing list validation")

    Just (TupleSchema _) ->
      -- TODO: warn about interaction with bound flags
      pure ()

    Nothing -> pure ()

  minItems <- useKey "minItems" asCount
  maxItems <- useKey "maxItems" asCount

  if badBounds minItems maxItems
    then warn "`minItems` is greater than `maxItems` - the schema is unsatisfiable!"
    else pure ()

  uniqueItems <- useKey "uniqueItems" asFlag

  -- TODO: Warn about detectable errs, like contains being unsatisfiable

  pure ArraySchema{..}

asFlag :: (ParserMonad m) => Parser m Flag
asFlag = Aeson.BE.asBool

asItemsSchema :: (ParserMonad m) => Parser m ItemsSchema
asItemsSchema
  = ListSchema <$> asSchema
    <|>
    TupleSchema <$> (Aeson.BE.eachInArray asSchema)

asSchema :: (ParserMonad m) => Parser m Schema
asSchema =
  asSchemaObject
  <|>
  (SchemaFlag <$> asFlag)
  where
    asSchemaObject = do
      schema <- useKey "$schema" asTextContent
      id <- useKey "$id" asURI

      title <- useKey "title" asTextContent
      description <- useKey "description" asTextContent
      -- TODO: warn if these don't match the schema
      defaultValue <- useKey "defaultValue" asJSONContent
      examples <- useKey "examples" (Aeson.BE.eachInArray asJSONContent)

      (types, typedSchemas) <- asTypedSchema

      -- TODO: warn if these don't match the type schema
      valueSchema <- asValueSchema

      anyOf <- useKey "anyOf" (Aeson.BE.eachInArray asSchema)
      allOf <- useKey "allOf" (Aeson.BE.eachInArray asSchema)
      oneOf <- useKey "oneOf" (Aeson.BE.eachInArray asSchema)
      not   <- useKey "not" asSchema

      ifThenElse <- asIfThenElseSchema

      ref <- useKey "$ref" asURI

      -- TODO: warn about presence of other keys in the presence of $ref

      additionalContent <- asAdditionalContent

      pure Schema{..}

asTextContent :: (ParserMonad m) => Parser m TextContent
asTextContent = Aeson.BE.asText

asJSONContent :: (ParserMonad m) => Parser m JSONContent
asJSONContent = Aeson.BE.asValue

asURI :: (ParserMonad m) => Parser m URI
asURI = Aeson.BE.asText

asTypedSchema :: (ParserMonad m) => Parser m (Maybe (OneOrMany SchemaType), TypedSchemas)
asTypedSchema = do
  typeVal <- useKey "type" (eachInOneOrMany asType)

  case typeVal of
    Nothing -> pure ()
    Just (One _) -> pure ()

    Just (Many types) -> do
      if null types
        then err "the `type` array cannot be empty"
        else pure ()

      if types /= nub types
        then warn "the `type` array contains duplicate values"
        else pure ()

      if (IntegerType `elem` types && NumberType `elem` types)
        then warn
              "the `type` array contains both `number` and `integer` as types\
              \ but `number` is more general than `integer`"
        else pure ()

      pure ()

  stringSchema <- asStringSchema
  if P.not (typeVal `accepts` StringType) && hasKeySet stringSchema
    then warn
          "one or more keys for the type `string` are set, but the schema\
          \ doesn't accept values of type `string`"
    else pure ()

  numberSchema <- asNumberSchema
  if P.not (typeVal `accepts` NumberType || typeVal `accepts` IntegerType) && hasKeySet numberSchema
    then warn
          "one or more keys for the type `number` are set, but the schema\
          \ doesn't accept values of type `number`"
    else pure ()

  objectSchema <- asObjectSchema
  if P.not (typeVal `accepts` ObjectType) && hasKeySet objectSchema
    then warn
          "one or more keys for the type `object` are set, but the schema\
          \ doesn't accept values of type `object`"
    else pure ()

  arraySchema <- asArraySchema
  if P.not (typeVal `accepts` ArrayType) && hasKeySet arraySchema
    then warn
          "one or more keys for the type `array` are set, but the schema\
          \ doesn't accept values of type `array`"
    else pure ()

  pure (typeVal, TypedSchemas{..})

  where
    accepts :: Maybe (OneOrMany SchemaType) -> SchemaType -> Bool
    accepts Nothing _ = True
    accepts (Just (One single)) sType = moreGeneralThan single sType
    accepts (Just (Many types)) sType = any (`moreGeneralThan` sType) types

asValueSchema :: (ParserMonad m) => Parser m (Maybe ValueSchema)
asValueSchema = do
  const <- useKey "const" asJSONContent
  enum  <- useKey "enum" (Aeson.BE.eachInArray asJSONContent)

  case (const, enum) of
    (Just constVal, Just enumVals) ->
      if (constVal `elem` enumVals)
        then do
          warn
            "the value of `enum` here is redundant - the only possible value\
            \ is the one specified by `const`"

          pure (Just (ConstSchema constVal))
        else
          err
            "`const` and `enum` have conflicting values - this schema cannot\
            \ accept any values!"
    (Just constVal, Nothing) ->
      pure (Just (ConstSchema constVal))
    (Nothing, Just enumVals) ->
      pure (Just (EnumSchema enumVals))
    (Nothing, Nothing) ->
      pure Nothing

asIfThenElseSchema :: ParserMonad m => Parser m (Maybe IfThenElseSchema)
asIfThenElseSchema = do
  maybeIfS <- useKey "if" asSchema
  maybeThenS <- useKey "then" asSchema
  maybeElseS <- useKey "else" asSchema

  case maybeIfS of
    Nothing -> case (maybeThenS, maybeElseS) of
      (Nothing, Nothing) ->
        pure Nothing

      (Just _, _) -> do
        warn "usage of `then` without an `if` has no effect"
        pure Nothing

      (_, Just _) -> do
        warn "usage of `else` without an `if` has no effect"
        pure Nothing

    Just ifS ->
      pure (Just (IfThenElseSchema ifS maybeThenS maybeElseS))

-- TODO: This logic
asAdditionalContent :: ParserMonad m => Parser m (Maybe AdditionalContent)
asAdditionalContent = pure Nothing
