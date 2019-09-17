module Data.Aeson.Schema.V7.Validate
  ( validate
  ) where

import Data.Aeson.Schema.V7.Schema

import qualified Prelude as P (min)
import Prelude hiding (const, id, length, map, min, minimum, max, maximum, not)

import qualified Data.Aeson as Aeson
import qualified Data.Bool as B (not)
import qualified Data.HashMap.Strict as HM (lookup, keys, member, size, toList)
import qualified Data.List as L (length, nub)
import           Data.Maybe (fromMaybe)
import qualified Data.Ranges as R
import           Data.Scientific (isInteger, coefficient, base10Exponent)
import qualified Data.Text as T (length)
import qualified Data.Vector as V (toList)
import qualified Text.Regex.PCRE.Heavy as Regex

validate :: Schema -> Aeson.Value -> Bool
validate (SchemaFlag True) _ = True
validate (SchemaFlag False) _ = False
validate Schema{..} value = and $
  [ checkMaybe types \specifiedTypes ->
      case specifiedTypes of
        One single -> single `acceptsValue` value
        Many multiple -> any (`acceptsValue` value) multiple

  , let TypedSchemas{..} = typedSchemas
    in and $
      [ validateString stringSchema value
      , validateNumber numberSchema value
      , validateObject objectSchema value
      , validateArray arraySchema value
      ]

  , let ValueSchemas{..} = valueSchemas
    in and $
      [ checkMaybe constSchema \constValue -> constValue == value
      , checkMaybe enumSchema \enumValues -> value `elem` enumValues
      ]

  , checkMaybe anyOf \schemas ->
      any (flip validate $ value) schemas

  , checkMaybe allOf \schemas ->
      all (flip validate $ value) schemas

  , checkMaybe oneOf \schemas ->
      exactlyOne (flip validate $ value) schemas

  , checkMaybe not \invertedSchema ->
      B.not (validate invertedSchema value)

  , checkMaybe ifThenElse \IfThenElseSchema{..} ->
      if validate ifS value
          then maybe True (flip validate value) thenS
          else maybe True (flip validate value) elseS

  ]

  where
    exactlyOne :: (a -> Bool) -> [a] -> Bool
    exactlyOne check list
      = fromMaybe False $
          foldr
            (\element acc -> case acc of
                Nothing -> if check element then Just True else Nothing
                Just True -> if check element then Just False else Just True
                Just False -> Just False)
          Nothing
          list

    validateString StringSchema{..} (Aeson.String string) = and $
      [ checkMaybe minLength \min -> min <= stringLength
      , checkMaybe maxLength \max -> max >= stringLength

      , checkMaybe pattern \regex -> string Regex.=~ regex
      , True -- TODO : format checks
      , True -- TODO : media type checks?
      , True -- TODO : encoding checks?
      ]

      where
        stringLength = T.length string
    validateString _ _ = True

    validateNumber numberSchema@NumberSchema{..} (Aeson.Number number) = and $
      [ checkMaybe multipleOf \divisor ->
          if divisor > number
            then number == 0
            else
              let normalizedExp = P.min (base10Exponent number) (base10Exponent divisor)
                  scaledNumber = coeffWithExponent number normalizedExp
                  scaledDivisor = coeffWithExponent divisor normalizedExp
              in gcd scaledNumber scaledDivisor == scaledDivisor

      , R.contains number (buildInterval numberSchema)

      ]

    validateNumber _ _ = True

    validateObject ObjectSchema{..} aesonValue@(Aeson.Object map) = and $
      [ checkMaybe properties \PropertiesSchema{..} ->
        all (\(key, propertyValue) ->
                maybe
                  True
                  (flip validate propertyValue)
                  (HM.lookup key propertiesSchema))
            (HM.toList map)

      , checkMaybe additionalProperties \additionalSchema ->
          all (\(key, additionalValue) ->
                  if maybe False (HM.member key . propertiesSchema) properties
                      then True
                      else validate additionalSchema additionalValue)
              (HM.toList map)

      , checkMaybe required \requiredProps ->
          all (`elem` HM.keys map) requiredProps

      , checkMaybe propertyNames \nameSchema ->
          all (validate nameSchema . Aeson.String) (HM.keys map)

      , checkMaybe patternProperties \PatternPropertiesSchema{..} ->
          all
            -- TODO: if matched here, keys should not be subject to
            -- `additionalProperties` checks
            (\(key, keyValue) ->
                all
                  (\(pattern, patternSchema) ->
                      if key Regex.=~ pattern
                        then validate patternSchema keyValue
                        else True
                  )
                  patternPropertiesSchema
            )
            (HM.toList map)

      , checkMaybe dependencies \DependenciesSchema{..} ->
          all
            (maybe True (\case
              PropertyDependency{..} ->
                all
                  (`HM.member` map)
                  propertyDependencies
              SchemaDependency{..} ->
                validate schemaDependency aesonValue
              )
            . (`HM.lookup` dependenciesSchema))
            (HM.keys map)

      , checkMaybe minProperties \min ->
          HM.size map >= min

      , checkMaybe maxProperties \max ->
          HM.size map <= max

      ]

    validateObject _ _ = True

    validateArray ArraySchema{..} (Aeson.Array arrayVec) = and $
      [ checkMaybe items \case
          ListSchema listSchema ->
            all (validate listSchema) array
          TupleSchema tupleSchema ->
            and (zipWith validate tupleSchema array)

      , checkMaybe contains \containsSchema ->
          any (validate containsSchema) array

      , checkMaybe additionalItems \additional ->
          case items of
            Nothing -> True
            Just ListSchema{} -> True
            Just (TupleSchema tupleItems) ->
              let beyondTuple = drop (L.length tupleItems) array
              in all (validate additional) beyondTuple

      , checkMaybe minItems \min ->
          L.length array >= min
      , checkMaybe maxItems \max ->
          L.length array <= max

      , checkMaybe uniqueItems \case
          False -> True
          True -> (L.nub array) == array

      ]

      where
        array = V.toList arrayVec

    validateArray _ _ = True

    checkMaybe = flip (maybe True)

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
