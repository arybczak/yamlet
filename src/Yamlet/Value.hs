-- | The values of YAML documents: the content with resolved tags, without the
-- styles, comments and positions of the syntax tree.
--
-- A 'Value' has 'Yamlet.FromYaml' and 'Yamlet.ToYaml' instances, e.g. to
-- read a document whose structure a program does not know. An alias becomes
-- a copy of the value that it refers to. So a small input with many aliases
-- can give a large value, and the decoder limits the aliases: they can add
-- 100000 nodes to a document, or as many nodes as the document has if that
-- is more. A document beyond the limit is an error.
module Yamlet.Value
  ( -- * Values
    Value (..)
  , FloatValue (..)
  , floatValueToDouble
  , doubleToFloatValue
  , floatValueToFloat
  , floatToFloatValue
  , describe

    -- * Tags
  , valueTag
  , nullTag
  , boolTag
  , intTag
  , floatTag
  , strTag
  , seqTag
  , mapTag
  ) where

import Control.DeepSeq
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import GHC.Generics

import Yamlet.Internal.Utils

-- | The value of a node.
data Value
  = Null
  | Bool !Bool
  | Int !Integer
  | Float !FloatValue
  | String !T.Text
  | Sequence [Value]
  | -- | The entries of a mapping in the order of the input. The keys are
    -- unique. The encoder does not check this for a mapping that a program
    -- builds, and a mapping with two equal keys does not read back.
    Mapping [(Value, Value)]
  | -- | A value with a tag that is not the tag of the core schema for it,
    -- e.g. @!point {x: 1}@. A scalar with a tag that the schema does not
    -- know is a 'String' inside, e.g. @!secret abc@.
    --
    -- The encoder writes the tag. A value in 'Tagged' with its own tag of
    -- the core schema reads back without 'Tagged'. A value that does not fit
    -- a tag of the core schema, e.g. a 'String' with 'intTag', does not read
    -- back.
    Tagged !T.Text !Value
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The value of a floating-point number. A finite value is exact, e.g. @0.1@
-- is exactly one tenth.
--
-- Arithmetic on a 'Sci.Scientific' with a huge exponent, e.g. @1e1000000000@,
-- can use all memory. Convert a value from an untrusted input with
-- 'floatValueToDouble' or with the bounded conversions of "Data.Scientific".
data FloatValue
  = -- | A finite value other than negative zero.
    --
    -- The encoder writes a value whose exponent in scientific notation is
    -- beyond the range from -1000 to 1000, e.g. @1.0e1001@, but the decoder
    -- rejects it. The decoder never gives such a value, and a 'Double' is
    -- always in the range.
    Finite !Sci.Scientific
  | -- | Negative zero, e.g. @-0.0@, which a 'Sci.Scientific' cannot hold.
    NegativeZero
  | Infinity
  | NegativeInfinity
  | NaN
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The nearest double, infinite if the value is out of its range.
floatValueToDouble :: FloatValue -> Double
floatValueToDouble = toRealFloat

-- | The value of a double. A finite double becomes the shortest decimal that
-- reads back as the same double, e.g. @0.1@.
doubleToFloatValue :: Double -> FloatValue
doubleToFloatValue = fromRealFloat

-- | The nearest float, infinite if the value is out of its range.
floatValueToFloat :: FloatValue -> Float
floatValueToFloat = toRealFloat

-- | The value of a float. A finite float becomes the shortest decimal that
-- reads back as the same float, e.g. @0.1@.
floatToFloatValue :: Float -> FloatValue
floatToFloatValue = fromRealFloat

toRealFloat :: RealFloat a => FloatValue -> a
toRealFloat = \case
  Finite s -> Sci.toRealFloat s
  NegativeZero -> -0
  Infinity -> 1 / 0
  NegativeInfinity -> -(1 / 0)
  NaN -> 0 / 0

fromRealFloat :: RealFloat a => a -> FloatValue
fromRealFloat d
  | isNaN d = NaN
  | isInfinite d = if d > 0 then Infinity else NegativeInfinity
  | isNegativeZero d = NegativeZero
  | otherwise = Finite (Sci.fromFloatDigits d)

-- | The kind of a value in plain words, for error messages, e.g. "a list".
-- The tag of 'Tagged' does not change it.
describe :: Value -> String
describe = \case
  Null -> "null"
  Bool _ -> "a boolean"
  Int _ -> "an integer"
  Float _ -> "a floating-point number"
  String _ -> "a string"
  Sequence _ -> "a list"
  Mapping _ -> "a mapping"
  Tagged _ v -> describe v

-- | The tag of a value: the tag of 'Tagged', or else the tag of the core
-- schema, e.g. 'intTag' for an 'Int'.
valueTag :: Value -> T.Text
valueTag = \case
  Null -> nullTag
  Bool _ -> boolTag
  Int _ -> intTag
  Float _ -> floatTag
  String _ -> strTag
  Sequence _ -> seqTag
  Mapping _ -> mapTag
  Tagged tag _ -> tag

-- | The tags of the core schema, e.g. @tag:yaml.org,2002:null@ for 'nullTag'.
nullTag, boolTag, intTag, floatTag, strTag, seqTag, mapTag :: T.Text
nullTag = coreTagPrefix <> "null"
boolTag = coreTagPrefix <> "bool"
intTag = coreTagPrefix <> "int"
floatTag = coreTagPrefix <> "float"
strTag = coreTagPrefix <> "str"
seqTag = coreTagPrefix <> "seq"
mapTag = coreTagPrefix <> "map"
