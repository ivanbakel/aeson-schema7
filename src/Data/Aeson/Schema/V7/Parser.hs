{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Aeson.Schema.V7.Parser
  ( parseSchema
  , parseSchemaSuppressingWarnings

  , SchemaParser

  , ParserOutput (..)
  , warn
  , err
  , suppressParseWarnings

  , SchemaPatternParser (..)
  , parsePattern
  , textPatterns

  , ParseResult
  ) where

import Data.Aeson.Schema.V7.Schema

import           Prelude hiding (const, id, minimum, maximum, not, unlines)
import qualified Prelude as P (not)

import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as Aeson.BE
import           Data.Aeson.BetterErrors ((<|>))
import           Data.Aeson.QQ (aesonQQ)
import           Data.HashMap.Strict (fromList)
import           Data.List (nub)
import           Data.Maybe (fromMaybe)
import qualified Data.Ranges as R
import           Data.Text (Text, pack, unpack, unlines)
import qualified Polysemy as Poly
import qualified Polysemy.Internal as Poly
import qualified Polysemy.Fail as Poly.Fail

type ErrorMessage = Text

data SchemaPatternParser pattern m a where
  ParsePattern :: Text -> SchemaPatternParser pattern m pattern

Poly.makeSem ''SchemaPatternParser

textPatterns :: Poly.Sem (SchemaPatternParser Text ': r) a -> Poly.Sem r a
textPatterns = Poly.interpret \(ParsePattern pattern) -> pure pattern

data ParserOutput m a where
  Warn :: ErrorMessage -> ParserOutput m ()
  Err  :: ErrorMessage -> ParserOutput m a

Poly.makeSem ''ParserOutput

suppressParseWarnings :: Poly.Sem (ParserOutput ': r) a -> Poly.Sem (Poly.Fail.Fail ': r) a
suppressParseWarnings
  = Poly.reinterpret \case
      Warn _message -> pure ()
      Err message -> Poly.send (Poly.Fail.Fail (unpack message))

type SchemaParser pattern r = Poly.Members [ParserOutput, SchemaPatternParser pattern] r
type ParseResult pattern r a = Aeson.BE.ParseT ErrorMessage (Poly.Sem r) a

parseSchema :: Poly.Members [ParserOutput, SchemaPatternParser pattern] r => Aeson.Value -> Poly.Sem r (Schema pattern)
parseSchema value = do
    result <- Aeson.BE.parseValueM asSchema value

    case result of
      Right schema -> pure schema
      Left parseError -> err (unlines (Aeson.BE.displayError (\message -> message) parseError))

parseSchemaSuppressingWarnings :: Poly.Members [Poly.Fail.Fail, SchemaPatternParser pattern] r => Aeson.Value -> Poly.Sem r (Schema pattern)
parseSchemaSuppressingWarnings
  = Poly.subsume . suppressParseWarnings . parseSchema

eachInOneOrMany :: Monad m => Aeson.BE.ParseT err m a -> Aeson.BE.ParseT err m (OneOrMany a)
eachInOneOrMany innerParser
  = (One <$> innerParser)
    <|>
    (Many <$> Aeson.BE.eachInArray innerParser)

asType :: SchemaParser pattern r => ParseResult pattern r SchemaType
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
      other     -> lift $ err ("unknown type `" <> other <> "`")

-- TODO: Register key use, so reuse and nonuse are errs
useKey :: Text -> ParseResult pattern r a -> ParseResult pattern r (Maybe a)
useKey = Aeson.BE.keyMay

asStringSchema :: SchemaParser pattern r => ParseResult pattern r (StringSchema pattern)
asStringSchema = do
  minLength <- useKey "minLength" asCount
  maxLength <- useKey "maxLength" asCount
  pattern   <- useKey "pattern" asPattern
  format    <- useKey "format" asFormat
  contentMediaType <- useKey "contentMediaType" asMediaType
  contentEncoding <- useKey "contentEncoding" asEncoding

  if badBounds minLength maxLength
    then lift $ warn "`minLength` is greater than `maxLength` - the schema is unsatisfiable!"
    else pure ()

  pure StringSchema{..}

asCount :: SchemaParser pattern r => ParseResult pattern r Count
asCount
  = Aeson.BE.asIntegral >>= \integer ->
      if integer >= 0
        then pure integer
        else lift $ err "this value cannot be negative"

asPattern :: SchemaParser pattern r => ParseResult pattern r pattern
asPattern = (lift . parsePattern) =<< Aeson.BE.asText

asFormat :: SchemaParser pattern r => ParseResult pattern r Format
asFormat = Aeson.BE.asText

asMediaType :: SchemaParser pattern r => ParseResult pattern r MediaType
asMediaType = Aeson.BE.asText

asEncoding :: SchemaParser pattern r => ParseResult pattern r Encoding
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

asNumberSchema :: SchemaParser pattern r => ParseResult pattern r NumberSchema
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
      lift $ warn $
        "Using both `minimum` and `exclusiveMinimum` is redundant - you should use"
        <> if val > exclusiveVal
            then pack (show [aesonQQ| { minimum: #{val} } |])
            else pack (show [aesonQQ| { exclusiveMinimum : #{exclusiveVal} } |])

    (Just val, Just (DoExclude exclude)) ->
      lift $ warn $
        "The boolean form of `exclusiveMinimum` is deprecated` - you should use"
        <> if exclude
              then pack (show [aesonQQ| { exclusiveMinimum : #{val} } |])
              else pack (show [aesonQQ| { minimum : #{val} } |])
    (Nothing, Just (DoExclude _)) ->
      lift $ warn $
        "The boolean form of `exclusiveMinimum` makes no sense without a value for\
        \ `minimum`, and will be ignored"

  case (maximum, exclusiveMaximum) of
    (Just _, Nothing) -> pure ()
    (Nothing, Just (ExcludeBoundary _)) -> pure ()
    (Nothing, Nothing) -> pure ()

    (Just val, Just (ExcludeBoundary exclusiveVal)) ->
      lift $ warn $
        "Using both `maximum` and `exclusiveMaximum` is redundant - you should use"
        <> if val < exclusiveVal
            then pack (show [aesonQQ| { maximum : #{val} } |])
            else pack (show [aesonQQ| { exclusiveMaximum : #{exclusiveVal} } |])

    (Just val, Just (DoExclude exclude)) ->
      lift $ warn $
        "The boolean form of `exclusiveMaximum` is deprecated` - you should use"
        <> if exclude
              then pack (show [aesonQQ| { exclusiveMaximum : #{val} } |])
              else pack (show [aesonQQ| { maximum : #{val} } |])
    (Nothing, Just (DoExclude _)) ->
      lift $ warn $
        "The boolean form of `exclusiveMaximum` makes no sense without a value for\
        \ `maximum`, and will be ignored"

  let numberSchema = NumberSchema{..}

  if rangeIsEmpty (buildInterval numberSchema)
    then lift $ warn "This combination of bounds results in an empty range!"
    else pure ()

  pure numberSchema

  where
    rangeIsEmpty :: R.Ranges Number -> Bool
    rangeIsEmpty range = R.unRanges range == R.unRanges R.inf

asNumber :: SchemaParser pattern r => ParseResult pattern r Number
asNumber = Aeson.BE.asScientific

asExclusive :: SchemaParser pattern r => ParseResult pattern r ExclusiveSchema
asExclusive
  = ExcludeBoundary <$> asNumber
    <|>
    DoExclude <$> Aeson.BE.asBool

asObjectSchema :: SchemaParser pattern r => ParseResult pattern r (ObjectSchema pattern)
asObjectSchema = do
  properties <- useKey "properties" asPropertiesSchema
  additionalProperties <- useKey "additionalProperties" asSchema
  propertyNames <- useKey "propertyNames" asSchema
  patternProperties <- useKey "patternProperties" asPatternPropertiesSchema

  dependencies <- useKey "dependencies" asDependencies

  -- TODO: Check these against property names and pattern properties
  required <- useKey "required" (Aeson.BE.eachInArray asPropertyKey)

  minProperties <- useKey "minProperties" asCount
  maxProperties <- useKey "maxProperties" asCount

  if badBounds minProperties maxProperties
    then lift $ warn "`minProperties` is greater than `maxProperties` - the schema is unsatisfiable!"
    else pure ()

  if maxProperties < (length <$> required)
    then lift $ warn
          "There are more properties in `requiredProperties` than are allowed by\
          \ `maxProperties` - the schema is unsatisfiable!"
    else pure ()

  pure ObjectSchema{..}

asPropertiesSchema :: SchemaParser pattern r => ParseResult pattern r (PropertiesSchema pattern)
asPropertiesSchema = do
  keySchemaPairs <- Aeson.BE.eachInObject asSchema

  pure (PropertiesSchema (fromList keySchemaPairs))

asPatternPropertiesSchema :: SchemaParser pattern r => ParseResult pattern r (PatternPropertiesSchema pattern)
asPatternPropertiesSchema = do
  PatternPropertiesSchema <$> Aeson.BE.forEachInObject \key -> do
    patternKey <- lift (parsePattern key)
    propertySchema <- asSchema
    pure (patternKey, propertySchema)

asPropertyKey :: SchemaParser pattern r => ParseResult pattern r PropertyKey
asPropertyKey = Aeson.BE.asText

asDependencies :: SchemaParser pattern r => ParseResult pattern r (DependenciesSchema pattern)
asDependencies = do
  keyDependencyPairs <- Aeson.BE.eachInObject asDependency

  pure (DependenciesSchema (fromList keyDependencyPairs))

asDependency :: SchemaParser pattern r => ParseResult pattern r (Dependency pattern)
asDependency
  = (PropertyDependency <$> Aeson.BE.eachInArray asPropertyKey)
    <|>
    (SchemaDependency <$> asSchema)

asArraySchema :: SchemaParser pattern r => ParseResult pattern r (ArraySchema pattern)
asArraySchema = do
  items <- useKey "items" asItemsSchema
  contains <- useKey "contains" asSchema
  additionalItems <- useKey "additionalItems" asSchema

  case items of
    Just (ListSchema _) -> do
      case additionalItems of
        Nothing -> pure ()
        Just _  ->
          lift $ warn "`additionalItems` is ignored when doing list validation"

    Just (TupleSchema _) ->
      -- TODO: warn about interaction with bound flags
      pure ()

    Nothing -> pure ()

  minItems <- useKey "minItems" asCount
  maxItems <- useKey "maxItems" asCount

  if badBounds minItems maxItems
    then lift $ warn "`minItems` is greater than `maxItems` - the schema is unsatisfiable!"
    else pure ()

  uniqueItems <- useKey "uniqueItems" asFlag

  -- TODO: Warn about detectable errs, like contains being unsatisfiable

  pure ArraySchema{..}

asFlag :: SchemaParser pattern r => ParseResult pattern r Flag
asFlag = Aeson.BE.asBool

asItemsSchema :: SchemaParser pattern r => ParseResult pattern r (ItemsSchema pattern)
asItemsSchema
  = ListSchema <$> asSchema
    <|>
    TupleSchema <$> (Aeson.BE.eachInArray asSchema)

asSchema :: SchemaParser pattern r => ParseResult pattern r (Schema pattern)
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
      valueSchemas <- asValueSchemas

      anyOf <- useKey "anyOf" (Aeson.BE.eachInArray asSchema)
      allOf <- useKey "allOf" (Aeson.BE.eachInArray asSchema)
      oneOf <- useKey "oneOf" (Aeson.BE.eachInArray asSchema)
      not   <- useKey "not" asSchema

      ifThenElse <- asIfThenElseSchema

      ref <- useKey "$ref" asURI

      -- TODO: warn about presence of other keys in the presence of $ref

      additionalContent <- asAdditionalContent

      pure Schema{..}

asTextContent :: SchemaParser pattern r => ParseResult pattern r TextContent
asTextContent = Aeson.BE.asText

asJSONContent :: SchemaParser pattern r => ParseResult pattern r JSONContent
asJSONContent = Aeson.BE.asValue

asURI :: SchemaParser pattern r => ParseResult pattern r URI
asURI = Aeson.BE.asText

asTypedSchema :: SchemaParser pattern r => ParseResult pattern r (Maybe (OneOrMany SchemaType), TypedSchemas pattern)
asTypedSchema = do
  typeVal <- useKey "type" (eachInOneOrMany asType)

  case typeVal of
    Nothing -> pure ()
    Just (One _) -> pure ()

    Just (Many types) -> do
      if null types
        then lift $ err "the `type` array cannot be empty"
        else pure ()

      if types /= nub types
        then lift $ warn "the `type` array contains duplicate values"
        else pure ()

      if (IntegerType `elem` types && NumberType `elem` types)
        then lift $ warn
              "the `type` array contains both `number` and `integer` as types\
              \ but `number` is more general than `integer`"
        else pure ()

      pure ()

  stringSchema <- asStringSchema
  if P.not (typeVal `accepts` StringType) && hasKeySet stringSchema
    then lift $ warn
          "one or more keys for the type `string` are set, but the schema\
          \ doesn't accept values of type `string`"
    else pure ()

  numberSchema <- asNumberSchema
  if P.not (typeVal `accepts` NumberType || typeVal `accepts` IntegerType) && hasKeySet numberSchema
    then lift $ warn
          "one or more keys for the type `number` are set, but the schema\
          \ doesn't accept values of type `number`"
    else pure ()

  objectSchema <- asObjectSchema
  if P.not (typeVal `accepts` ObjectType) && hasKeySet objectSchema
    then lift $ warn
          "one or more keys for the type `object` are set, but the schema\
          \ doesn't accept values of type `object`"
    else pure ()

  arraySchema <- asArraySchema
  if P.not (typeVal `accepts` ArrayType) && hasKeySet arraySchema
    then lift $ warn
          "one or more keys for the type `array` are set, but the schema\
          \ doesn't accept values of type `array`"
    else pure ()

  pure (typeVal, TypedSchemas{..})

  where
    accepts :: Maybe (OneOrMany SchemaType) -> SchemaType -> Bool
    accepts Nothing _ = True
    accepts (Just (One single)) sType = moreGeneralThan single sType
    accepts (Just (Many types)) sType = any (`moreGeneralThan` sType) types

asValueSchemas :: SchemaParser pattern r => ParseResult pattern r ValueSchemas
asValueSchemas = do
  constSchema <- useKey "const" asJSONContent
  enumSchema  <- useKey "enum" (Aeson.BE.eachInArray asJSONContent)

  case (constSchema, enumSchema) of
    (Just constVal, Just enumVals) ->
      if (constVal `elem` enumVals)
        then do
          lift $ warn
            "the value of `enum` here is redundant - the only possible value\
            \ is the one specified by `const`"
        else
          lift $ warn
            "`const` and `enum` have conflicting values - this schema cannot\
            \ accept any values!"

    (_, _) -> pure ()

  pure ValueSchemas{..}

asIfThenElseSchema :: SchemaParser pattern r => ParseResult pattern r (Maybe (IfThenElseSchema pattern))
asIfThenElseSchema = do
  maybeIfS <- useKey "if" asSchema
  maybeThenS <- useKey "then" asSchema
  maybeElseS <- useKey "else" asSchema

  case maybeIfS of
    Nothing -> case (maybeThenS, maybeElseS) of
      (Nothing, Nothing) ->
        pure Nothing

      (Just _, _) -> do
        lift $ warn "usage of `then` without an `if` has no effect"
        pure Nothing

      (_, Just _) -> do
        lift $ warn "usage of `else` without an `if` has no effect"
        pure Nothing

    Just ifS ->
      pure (Just (IfThenElseSchema ifS maybeThenS maybeElseS))

-- TODO: This logic
asAdditionalContent :: SchemaParser pattern r => ParseResult pattern r (Maybe (AdditionalContent pattern))
asAdditionalContent = pure Nothing
