module Data.Aeson.Schema.V7.Validate
  ( validate
  ) where

import Data.Aeson.Schema.V7.Schema

import Prelude hiding (const, id, length, map, min, minimum, max, maximum, not)

import qualified Data.Aeson as Aeson
import qualified Data.Bool as B (not)
import qualified Data.HashMap.Strict as HM (lookup, keys, member, size, toList)
import qualified Data.List as L (length, nub)
import           Data.Maybe (fromMaybe)
import qualified Data.Range.Range as R
import           Data.Scientific (isInteger)
import qualified Data.Text as T (length)
import qualified Data.Vector as V (toList)

validate :: Schema -> Aeson.Value -> Bool
validate (SchemaFlag True) _ = True
validate (SchemaFlag False) _ = False
validate Schema{..} value = and $
  [ checkMaybe types \specifiedTypes ->
      case specifiedTypes of
        One single -> single `acceptsValue` value
        Many types -> any (`acceptsValue` value) types
    
  , let TypedSchemas{..} = typedSchemas
    in and $
      [ validateString stringSchema value
      , validateNumber numberSchema value
      , validateObject objectSchema value
      , validateArray arraySchema value
      ]

  , checkMaybe valueSchema \case
    ConstSchema constValue -> constValue == value
    EnumSchema enumValues -> value `elem` enumValues

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

      , True -- TODO : pattern checks
      , True -- TODO : format checks
      , True -- TODO : media type checks?
      , True -- TODO : encoding checks?
      ]

      where
        stringLength = T.length string
    validateString _ _ = False

    validateNumber numberSchema@NumberSchema{..} (Aeson.Number number) = and $
      [ checkMaybe multipleOf \_divisor -> False -- TODO

      , R.inRanges (buildRanges numberSchema) number

      ]

    validateNumber _ _ = False

    validateObject ObjectSchema{..} (Aeson.Object map) = and $
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

      , checkMaybe requiredProperties \required ->
          all (`elem` HM.keys map) required

      , checkMaybe propertyNames \nameSchema ->
          all (validateString nameSchema . Aeson.String) (HM.keys map)

      , checkMaybe patternProperties \PatternPropertiesSchema{} ->
          True -- TODO: pattern checks

      , checkMaybe minProperties \min ->
          HM.size map >= min

      , checkMaybe maxProperties \max ->
          HM.size map <= max

      ]

    validateObject _ _ = False

    validateArray ArraySchema{..} (Aeson.Array arrayVec) = and $
      [ checkMaybe items \case
          ListSchema{..} ->
            all (validate listSchema) array
          TupleSchema{} -> False -- TODO

      , checkMaybe contains \containsSchema ->
          any (validate containsSchema) array

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

    validateArray _ _ = False

    checkMaybe = flip (maybe True)

acceptsValue :: SchemaType -> Aeson.Value -> Bool
acceptsValue StringType (Aeson.String _) = True
acceptsValue IntegerType (Aeson.Number val) = isInteger val
acceptsValue NumberType (Aeson.Number _) = True
acceptsValue ObjectType (Aeson.Object _) = True
acceptsValue ArrayType (Aeson.Array _) = True
acceptsValue BooleanType (Aeson.Bool _) = True
acceptsValue NullType Aeson.Null = True
acceptsValue _ _ = False
