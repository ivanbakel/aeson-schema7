module Data.Aeson.Schema.V7.Schema where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           Data.Scientific (Scientific)
import           Data.HashMap.Strict (HashMap)

data SchemaType
  = StringType
  | IntegerType
  | NumberType
  | ObjectType
  | ArrayType
  | BooleanType
  | NullType
  deriving (Eq)

data StringSchema
  = StringSchema
      { minLength :: Maybe Count
      , maxLength :: Maybe Count
      , pattern   :: Maybe Pattern
      , format    :: Maybe Format

      , contentMediaType :: Maybe MediaType
      , contentEncoding :: Maybe Encoding
      }

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

type Number = Scientific

data ExclusiveSchema
  = DoExclude Bool
  | ExcludeBoundary Number

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

      , typedSchema :: Maybe TypedSchema
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

data TypedSchema
  = ManyTypes [SchemaType]
  | StringTypedSchema StringSchema
  | IntegerTypedSchema NumberSchema
  | NumberTypedSchema NumberSchema
  | ObjectTypedSchema ObjectSchema
  | ArrayTypedSchema ArraySchema
  | BooleanTypedSchema
  | NullTypedSchema

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
      , thenS :: Schema
      , elseS :: Maybe Schema
      }

type URI = Text

newtype AdditionalContent
  = AdditionalContent
      { additionalKeys :: Map PropertyKey Schema
      }
