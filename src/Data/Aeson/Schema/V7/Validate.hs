{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Aeson.Schema.V7.Validate
  ( validate

  , CheckPattern (..)
  , checkPattern

  , CheckEncoding (..)
  , checkEncoding
  , ignoreEncoding

  , CheckFormat (..)
  , checkFormat
  , ignoreFormat
  ) where

import Data.Aeson.Schema.V7.Schema

import qualified Prelude as P (min)
import Prelude hiding (const, id, length, map, min, minimum, max, maximum, not)

import           Control.Monad (zipWithM)
import           Control.Monad.Extra (allM, anyM, andM, ifM)
import qualified Data.Aeson as Aeson
import qualified Data.Bool as B (not)
import           Data.Foldable (foldrM)
import qualified Data.HashMap.Strict as HM (lookup, keys, member, size, toList)
import qualified Data.List as L (length, nub)
import           Data.Maybe (fromMaybe)
import qualified Data.Ranges as R
import           Data.Scientific (isInteger, coefficient, base10Exponent)
import qualified Data.Text as T (Text, length)
import qualified Data.Vector as V (toList)
import qualified Polysemy as Poly

-- | Regex checker for a fixed pattern type
data CheckPattern pattern m a where
  CheckPattern :: pattern -> T.Text -> CheckPattern pattern m Bool

Poly.makeSem ''CheckPattern

-- | Content encoding checker for strings
data CheckEncoding m a where
  CheckEncoding :: Encoding -> T.Text -> CheckEncoding m Bool

Poly.makeSem ''CheckEncoding

ignoreEncoding :: Poly.InterpreterFor CheckEncoding r
ignoreEncoding = Poly.interpret \(CheckEncoding _ _) -> pure True

-- | Format checker for strings
data CheckFormat m a where
  CheckFormat :: Format -> T.Text -> CheckFormat m Bool

Poly.makeSem ''CheckFormat

ignoreFormat :: Poly.InterpreterFor CheckFormat r
ignoreFormat = Poly.interpret \(CheckFormat _ _) -> pure True

type SchemaValidator pattern r = Poly.Members [CheckPattern pattern, CheckEncoding, CheckFormat] r

validate :: forall pattern r. SchemaValidator pattern r => Schema pattern -> Aeson.Value -> Poly.Sem r Bool
validate (SchemaFlag True) _ = pure True
validate (SchemaFlag False) _ = pure False
validate Schema{..} value = andM
    [ pure $ checkMaybe types \specifiedTypes ->
      case specifiedTypes of
        One single -> single `acceptsValue` value
        Many multiple -> any (`acceptsValue` value) multiple

  , let TypedSchemas{..} = typedSchemas
    in andM
      [ validateString stringSchema value
      , pure (validateNumber numberSchema value)
      , validateObject objectSchema value
      , validateArray arraySchema value
      ]

  , let ValueSchemas{..} = valueSchemas
    in pure $ and
      [ checkMaybe constSchema \constValue -> constValue == value
      , checkMaybe enumSchema \enumValues -> value `elem` enumValues
      ]

  , checkMaybeM anyOf \schemas ->
      anyM (flip validate $ value) schemas

  , checkMaybeM allOf \schemas ->
      allM (flip validate $ value) schemas

  , checkMaybeM oneOf \schemas ->
      exactlyOneM (flip validate $ value) schemas

  , checkMaybeM not \invertedSchema ->
      B.not <$> (validate invertedSchema value)

  , checkMaybeM ifThenElse \IfThenElseSchema{..} -> do
      ifResult <- validate ifS value
      if ifResult
          then checkMaybeM thenS \thenSchema -> validate thenSchema value
          else checkMaybeM elseS \elseSchema -> validate elseSchema value

  ]

  where
    exactlyOneM :: (a -> Poly.Sem r Bool) -> [a] -> Poly.Sem r Bool
    exactlyOneM check list
      = fromMaybe False <$>
          foldrM
            (\element acc -> case acc of
                Nothing -> ifM (check element) (pure $ Just True) (pure Nothing)
                Just True -> ifM (check element) (pure $ Just False) (pure $ Just True)
                Just False -> pure (Just False))
          Nothing
          list

    validateString StringSchema{..} (Aeson.String string) = andM
      [ checkMaybeM minLength \min -> pure (min <= stringLength)
      , checkMaybeM maxLength \max -> pure (max >= stringLength)

      , checkMaybeM pattern \regex -> checkPattern @pattern regex string
      , checkMaybeM format \formatString -> checkFormat formatString string
      , pure True -- TODO : media type checks?
      , checkMaybeM contentEncoding \encoding -> checkEncoding encoding string
      ]

      where
        stringLength = T.length string
    validateString _ _ = pure True

    validateNumber numberSchema@NumberSchema{..} (Aeson.Number number) = and $
      [ checkMaybe multipleOf \divisor ->
          if divisor > number
            then number == 0
            else
              let normalizedExp = P.min (base10Exponent number) (base10Exponent divisor)
                  scaledNumber = coeffWithExponent number normalizedExp
                  scaledDivisor = coeffWithExponent divisor normalizedExp
              in gcd scaledNumber scaledDivisor == scaledDivisor

      , R.inRanges (buildInterval numberSchema) number

      ]

    validateNumber _ _ = True

    validateObject ObjectSchema{..} aesonValue@(Aeson.Object map) = andM
      [ checkMaybeM properties \PropertiesSchema{..} ->
          allM
            (\(key, propertyValue) ->
                  maybe
                    (pure True)
                    (flip validate propertyValue)
                    (HM.lookup key propertiesSchema))
            (HM.toList map)

      , checkMaybeM patternProperties \PatternPropertiesSchema{..} ->
          allM
            (\(key, keyValue) ->
                allM
                  (\(pattern, patternSchema) -> do
                      checkResult <- checkPattern pattern key
                      if checkResult
                        then validate patternSchema keyValue
                        else pure True
                  )
                  patternPropertiesSchema
            )
            (HM.toList map)

      , checkMaybeM additionalProperties \additionalSchema ->
          allM
            (\(key, additionalValue) -> do
                  -- TODO: Consider some kind of key registration for this? It
                  -- would have to account for location in the object
                  let matchesProperty = maybe False (HM.member key . propertiesSchema) properties
                  matchesPatternProperty <- maybe (pure False)
                    (\PatternPropertiesSchema{..} ->
                      anyM (\(pattern, _) -> checkPattern pattern key) patternPropertiesSchema)
                    patternProperties

                  if matchesProperty || matchesPatternProperty
                      then pure True
                      else validate additionalSchema additionalValue)
            (HM.toList map)

      , checkMaybeM required \requiredProps ->
          pure (all (`elem` HM.keys map) requiredProps)

      , checkMaybeM propertyNames \nameSchema ->
          allM (validate nameSchema . Aeson.String) (HM.keys map)

      , checkMaybeM dependencies \DependenciesSchema{..} ->
          allM
            (maybe (pure True) (\case
              PropertyDependency{..} ->
                pure $ all
                  (`HM.member` map)
                  propertyDependencies
              SchemaDependency{..} ->
                validate schemaDependency aesonValue
              )
            . (`HM.lookup` dependenciesSchema))
            (HM.keys map)

      , checkMaybeM minProperties \min ->
          pure (HM.size map >= min)

      , checkMaybeM maxProperties \max ->
          pure (HM.size map <= max)

      ]

    validateObject _ _ = pure True

    validateArray ArraySchema{..} (Aeson.Array arrayVec) = and <$> sequence
      [ checkMaybeM items \case
          ListSchema listSchema ->
            allM (validate listSchema) array
          TupleSchema tupleSchema ->
            and <$> (zipWithM validate tupleSchema array)

      , checkMaybeM contains \containsSchema ->
          anyM (validate containsSchema) array

      , checkMaybeM additionalItems \additional ->
          case items of
            Nothing -> pure True
            Just ListSchema{} -> pure True
            Just (TupleSchema tupleItems) ->
              let beyondTuple = drop (L.length tupleItems) array
              in allM (validate additional) beyondTuple

      , checkMaybeM minItems \min ->
          pure (L.length array >= min)
      , checkMaybeM maxItems \max ->
          pure (L.length array <= max)

      , checkMaybeM uniqueItems \case
          False -> pure True
          True -> pure ((L.nub array) == array)

      ]

      where
        array = V.toList arrayVec

    validateArray _ _ = pure True

    checkMaybe :: Maybe a -> (a -> Bool) -> Bool
    checkMaybe = flip (maybe True)

    checkMaybeM :: Maybe a -> (a -> Poly.Sem r Bool) -> Poly.Sem r Bool
    checkMaybeM = flip (maybe (pure True))

    coeffWithExponent scientificValue newExp
      = let scaleFactor = (base10Exponent scientificValue - newExp)
        in coefficient scientificValue * (10 ^ scaleFactor)

acceptsValue :: SchemaType -> Aeson.Value -> Bool
acceptsValue StringType (Aeson.String _) = True
acceptsValue IntegerType (Aeson.Number val) = isInteger val
acceptsValue NumberType (Aeson.Number _) = True
acceptsValue ObjectType (Aeson.Object _) = True
acceptsValue ArrayType (Aeson.Array _) = True
acceptsValue BooleanType (Aeson.Bool _) = True
acceptsValue NullType Aeson.Null = True
acceptsValue _ _ = False
