{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- Unfornate @Enum@ instance below

module Data.Aeson.Schema.V7.Schema where

import Prelude hiding (minimum, maximum)

import qualified Data.Aeson as Aeson
import qualified Data.Generics.Traversable as Gen
import           Data.Maybe (fromMaybe)
import qualified Data.Range.Algebra as R.A
import qualified Data.Range.Range as R
import           Data.Text (Text)
import           Data.Scientific (Scientific, scientific, toBoundedInteger)
import           Data.HashMap.Strict (HashMap)

import           GHC.Generics (Generic)

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

type Pattern = Text
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

instance Enum Scientific where
  succ x = x + (scientific 1 1)
  pred x = x - (scientific 1 1)

  toEnum = (`scientific` 1) . toInteger
  fromEnum = fromMaybe (maxBound) . toBoundedInteger

  enumFrom n = iterate succ n
  enumFromThen n n'
    = let diff = n' - n
      in iterate (\x -> x + diff) n
  enumFromTo n m
    = takeWhile (<= m) (enumFrom n)
  enumFromThenTo n n' m
    = let diff = n' - n
      in takeWhile (if diff < 0 then (>= m) else (<= m)) (enumFromThen n n')

type Range = R.A.RangeExpr [R.Range Number]

buildRanges :: NumberSchema -> [R.Range Number]
buildRanges NumberSchema{..}
  = R.A.eval $ foldr1 R.A.intersection
      [ valueToRange minimum makeMinimum
      , valueToRange maximum makeMaximum
      , exclusiveSchemaToRange exclusiveMinimum minimum makeMinimum
      , exclusiveSchemaToRange exclusiveMaximum maximum makeMaximum
      ]
  where
    makeMinimum = R.A.const . (:[]) . R.LowerBoundRange
    makeMaximum = R.A.const . (:[]) . R.UpperBoundRange

    valueToRange :: Maybe Number -> (Number -> Range) -> Range
    valueToRange (Just val) makeRange
      = makeRange val
    valueToRange Nothing _
      = R.A.const [R.InfiniteRange]

    exclusiveSchemaToRange :: Maybe ExclusiveSchema -> Maybe Number -> (Number -> Range) -> Range
    exclusiveSchemaToRange (Just (DoExclude True)) (Just bound) makeRange
      = valueToRange (Just bound) makeRange `R.A.difference` R.A.const [R.SingletonRange bound]
    exclusiveSchemaToRange (Just (DoExclude _)) maybeBound makeRange
      = valueToRange maybeBound makeRange
    exclusiveSchemaToRange (Just (ExcludeBoundary val)) _ makeRange
      = makeRange val `R.A.difference` R.A.const [R.SingletonRange val]
    exclusiveSchemaToRange Nothing _ _
      = R.A.const [R.InfiniteRange]

data ObjectSchema
  = ObjectSchema
      { properties :: Maybe PropertiesSchema
      , additionalProperties :: Maybe Schema
      , requiredProperties :: Maybe [PropertyKey]
      , propertyNames :: Maybe StringSchema
      , patternProperties :: Maybe PatternPropertiesSchema

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
      { patternPropertiesSchema :: Map Pattern Schema
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
      , typedSchemas :: Maybe TypedSchemas
      , valueSchema :: Maybe ValueSchema

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

data ValueSchema
  = ConstSchema
      { constSchema :: JSONContent
      }
  | EnumSchema
      { enumSchema :: [JSONContent]
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
