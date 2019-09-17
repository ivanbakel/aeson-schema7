{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- Unfornate @Enum@ instance below

module Data.Aeson.Schema.V7.Schema where

import Prelude hiding (minimum, maximum)

import qualified Data.Aeson as Aeson
import qualified Data.Ranges as R
import           Data.Text (Text)
import           Data.HashMap.Strict (HashMap)
import           Data.Scientific (Scientific)

import           GHC.Generics (Generic)
import qualified Data.Generics.Traversable as Gen
import           Data.Generics.Traversable.Generic ()

import qualified Text.Regex.PCRE.Heavy as Regex

data SchemaType
  = StringType
  | IntegerType
  | NumberType
  | ObjectType
  | ArrayType
  | BooleanType
  | NullType
  deriving (Eq)

moreGeneralThan :: SchemaType -> SchemaType -> Bool
moreGeneralThan NumberType IntegerType = True
moreGeneralThan type1 type2 = type1 == type2

data StringSchema
  = StringSchema
      { minLength :: Maybe Count
      , maxLength :: Maybe Count
      , pattern   :: Maybe Pattern
      , format    :: Maybe Format

      , contentMediaType :: Maybe MediaType
      , contentEncoding :: Maybe Encoding
      }
  deriving (Generic)

-- TODO Replace
type MediaType = Text
data Encoding
  = SevenBit
  | EightBit
  | Binary
  | QuotedPrintable
  | Base64

type Count = Int

type Pattern = Regex.Regex
type Format = Text

data NumberSchema
  = NumberSchema
      { multipleOf :: Maybe Number
      , minimum    :: Maybe Number
      , maximum    :: Maybe Number
      , exclusiveMinimum :: Maybe ExclusiveSchema
      , exclusiveMaximum :: Maybe ExclusiveSchema
      }
  deriving (Generic)

type Number = Scientific

data ExclusiveSchema
  = DoExclude Bool
  | ExcludeBoundary Number

buildInterval :: NumberSchema -> R.Interval Number
buildInterval NumberSchema{..}
  = foldr1 R.intersection
      [ inclusiveMinimumRange
      , exclusiveMinimumRange
      , inclusiveMaximumRange
      , exclusiveMaximumRange
      ]
  where
    inclusiveMinimumRange
      = maybe R.Everything R.FromClosed minimum
    exclusiveMinimumRange
      = case exclusiveMinimum of
          Just (ExcludeBoundary val) ->
            R.FromOpen val
          Just (DoExclude True) ->
            maybe R.Everything R.FromOpen minimum
          Just (DoExclude False) ->
            R.Everything
          Nothing ->
            R.Everything

    inclusiveMaximumRange
      = maybe R.Everything R.ToClosed maximum
    exclusiveMaximumRange
      = case exclusiveMaximum of
          Just (ExcludeBoundary val) ->
            R.ToOpen val
          Just (DoExclude True) ->
            maybe R.Everything R.ToOpen maximum
          Just (DoExclude False) ->
            R.Everything
          Nothing ->
            R.Everything

data ObjectSchema
  = ObjectSchema
      { properties :: Maybe PropertiesSchema
      , additionalProperties :: Maybe Schema
      , required :: Maybe [PropertyKey]

      , propertyNames :: Maybe Schema
      , patternProperties :: Maybe PatternPropertiesSchema

      , dependencies :: Maybe DependenciesSchema

      , minProperties :: Maybe Count
      , maxProperties :: Maybe Count
      }
  deriving (Generic)

type PropertyKey = Text

type Map = HashMap

newtype PropertiesSchema
  = PropertiesSchema
      { propertiesSchema :: Map PropertyKey Schema
      }

newtype PatternPropertiesSchema
  = PatternPropertiesSchema
      { patternPropertiesSchema :: [(Pattern, Schema)]
      }

newtype DependenciesSchema
  = DependenciesSchema
      { dependenciesSchema :: Map PropertyKey Dependency
      }

data Dependency
  = PropertyDependency
      { propertyDependencies :: [PropertyKey]
      }
  | SchemaDependency
      { schemaDependency :: Schema
      }

data ArraySchema
  = ArraySchema
      { items :: Maybe ItemsSchema
      , contains :: Maybe Schema
      , additionalItems :: Maybe Schema
      , minItems :: Maybe Count
      , maxItems :: Maybe Count
      , uniqueItems :: Maybe Flag
      }
  deriving (Generic)

data ItemsSchema
  = ListSchema
      { listSchema :: Schema
      }
  | TupleSchema
      { tupleSchema :: [Schema]
      }

type Flag = Bool

-- Boolean and null have no specific schema

data Schema
  = Schema
      { schema :: Maybe TextContent
      , id :: Maybe URI
      , title :: Maybe TextContent
      , description :: Maybe TextContent
      , defaultValue :: Maybe JSONContent
      , examples :: Maybe [JSONContent]

      , types :: Maybe (OneOrMany SchemaType)
      , typedSchemas :: TypedSchemas
      , valueSchemas :: ValueSchemas

      , anyOf :: Maybe [Schema]
      , allOf :: Maybe [Schema]
      , oneOf :: Maybe [Schema]
      , not   :: Maybe Schema

      , ifThenElse :: Maybe IfThenElseSchema

      , ref :: Maybe URI

      -- Keys with no semantic meaning
      , additionalContent :: Maybe AdditionalContent
      }
  -- | A schema of `true` (accept everything) or `false` (accept nothing)
  | SchemaFlag Flag

data OneOrMany a
  = One a
  | Many [a]

data TypedSchemas
  = TypedSchemas
      { stringSchema :: StringSchema
      , numberSchema :: NumberSchema
      , objectSchema :: ObjectSchema
      , arraySchema :: ArraySchema
      } 

data ValueSchemas
  = ValueSchemas
      { constSchema :: Maybe JSONContent
      , enumSchema :: Maybe [JSONContent]
      }

type TextContent = Text
type JSONContent = Aeson.Value

data IfThenElseSchema
  = IfThenElseSchema
      { ifS :: Schema
      , thenS :: Maybe Schema
      , elseS :: Maybe Schema
      }

type URI = Text

newtype AdditionalContent
  = AdditionalContent
      { additionalKeys :: Map PropertyKey Schema
      }

class SettableKey a where
  isKeySet :: a -> Bool

instance SettableKey (Maybe a) where
  isKeySet Nothing = False
  isKeySet (Just _) = True

hasKeySet 
  :: Gen.GTraversable SettableKey a
  => a
  -> Bool

hasKeySet
  = Gen.gfoldr @SettableKey (\val acc -> acc || (isKeySet val)) False
