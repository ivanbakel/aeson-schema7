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

data StringSchema pattern
  = StringSchema
      { minLength :: Maybe Count
      , maxLength :: Maybe Count
      , pattern   :: Maybe pattern
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

newtype Format = Format Text

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

buildInterval :: NumberSchema -> R.Ranges Number
buildInterval NumberSchema{..}
  = foldr1 R.intersection
      [ inclusiveMinimumRange
      , exclusiveMinimumRange
      , inclusiveMaximumRange
      , exclusiveMaximumRange
      ]
  where
    inclusiveMinimumRange
      = maybe R.inf R.lbi minimum
    exclusiveMinimumRange
      = case exclusiveMinimum of
          Just (ExcludeBoundary val) ->
            R.lbe val
          Just (DoExclude True) ->
            maybe R.inf R.lbe minimum
          Just (DoExclude False) ->
            R.inf
          Nothing ->
            R.inf

    inclusiveMaximumRange
      = maybe R.inf R.ubi maximum
    exclusiveMaximumRange
      = case exclusiveMaximum of
          Just (ExcludeBoundary val) ->
            R.ube val
          Just (DoExclude True) ->
            maybe R.inf R.ube maximum
          Just (DoExclude False) ->
            R.inf
          Nothing ->
            R.inf

data ObjectSchema pattern
  = ObjectSchema
      { properties :: Maybe (PropertiesSchema pattern)
      , additionalProperties :: Maybe (Schema pattern)
      , required :: Maybe [PropertyKey]

      , propertyNames :: Maybe (Schema pattern)
      , patternProperties :: Maybe (PatternPropertiesSchema pattern)

      , dependencies :: Maybe (DependenciesSchema pattern)

      , minProperties :: Maybe Count
      , maxProperties :: Maybe Count
      }
  deriving (Generic)

type PropertyKey = Text

type Map = HashMap

newtype PropertiesSchema pattern
  = PropertiesSchema
      { propertiesSchema :: Map PropertyKey (Schema pattern)
      }

newtype PatternPropertiesSchema pattern
  = PatternPropertiesSchema
      { patternPropertiesSchema :: [(pattern, Schema pattern)]
      }

newtype DependenciesSchema pattern
  = DependenciesSchema
      { dependenciesSchema :: Map PropertyKey (Dependency pattern)
      }

data Dependency pattern
  = PropertyDependency
      { propertyDependencies :: [PropertyKey]
      }
  | SchemaDependency
      { schemaDependency :: Schema pattern
      }

data ArraySchema pattern
  = ArraySchema
      { items :: Maybe (ItemsSchema pattern)
      , contains :: Maybe (Schema pattern)
      , additionalItems :: Maybe (Schema pattern)
      , minItems :: Maybe Count
      , maxItems :: Maybe Count
      , uniqueItems :: Maybe Flag
      }
  deriving (Generic)

data ItemsSchema pattern
  = ListSchema
      { listSchema :: Schema pattern
      }
  | TupleSchema
      { tupleSchema :: [Schema pattern]
      }

type Flag = Bool

-- Boolean and null have no specific schema

data Schema pattern
  = Schema
      { schema :: Maybe TextContent
      , id :: Maybe URI
      , title :: Maybe TextContent
      , description :: Maybe TextContent
      , defaultValue :: Maybe JSONContent
      , examples :: Maybe [JSONContent]

      , types :: Maybe (OneOrMany SchemaType)
      , typedSchemas :: TypedSchemas pattern
      , valueSchemas :: ValueSchemas

      , anyOf :: Maybe [Schema pattern]
      , allOf :: Maybe [Schema pattern]
      , oneOf :: Maybe [Schema pattern]
      , not   :: Maybe (Schema pattern)

      , ifThenElse :: Maybe (IfThenElseSchema pattern)

      , ref :: Maybe URI

      -- Keys with no semantic meaning
      , additionalContent :: Maybe (AdditionalContent pattern)
      }
  -- | A schema of `true` (accept everything) or `false` (accept nothing)
  | SchemaFlag Flag

data OneOrMany a
  = One a
  | Many [a]

data TypedSchemas pattern
  = TypedSchemas
      { stringSchema :: StringSchema pattern
      , numberSchema :: NumberSchema
      , objectSchema :: ObjectSchema pattern
      , arraySchema :: ArraySchema pattern
      } 

data ValueSchemas
  = ValueSchemas
      { constSchema :: Maybe JSONContent
      , enumSchema :: Maybe [JSONContent]
      }

type TextContent = Text
type JSONContent = Aeson.Value

data IfThenElseSchema pattern
  = IfThenElseSchema
      { ifS :: Schema pattern
      , thenS :: Maybe (Schema pattern)
      , elseS :: Maybe (Schema pattern)
      }

type URI = Text

newtype AdditionalContent pattern
  = AdditionalContent
      { additionalKeys :: Map PropertyKey (Schema pattern)
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
