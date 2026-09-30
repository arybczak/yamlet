-- | The values of YAML documents: the content with resolved tags, without the
-- styles, comments and positions of the syntax tree.
--
-- A 'Value' has t'Yamlet.Decode.FromYaml' and t'Yamlet.Encode.ToYaml'
-- instances, e.g. to read a document whose structure a program does not
-- know:
--
-- >>> decodeText @Value "!point {x: 1, y: 2.5}\n"
-- Right (Tagged "!point" (Mapping [(String "x",Int 1),(String "y",Float (Finite 2.5))]))
--
-- An alias becomes a copy of the value that it refers to:
--
-- >>> decodeText @Value "base: &b [1, 2]\ncopy: *b\n"
-- Right (Mapping [(String "base",Sequence [Int 1,Int 2]),(String "copy",Sequence [Int 1,Int 2])])
--
-- A small input with many aliases can give a large value. To prevent this,
-- the decoder limits the aliases. Each node and each character of a scalar
-- counts as one unit. The aliases can add 100000 units to a document. For a
-- document with more units, they can add as many units as the document has.
-- A document beyond the limit is an error.
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
    -- know is a v'String' inside, e.g. @!secret abc@.
    --
    -- The encoder writes the tag. A value with its own tag of the core
    -- schema reads back without 'Tagged', e.g. @Tagged intTag (Int 1)@ reads
    -- back as @Int 1@. A value that does not fit a tag of the core schema,
    -- e.g. a v'String' with 'intTag', does not read back.
    Tagged !T.Text !Value
  deriving stock (Eq, Ord, Show, Generic)

-- The instances of the sum types are written by hand, because GHC does not
-- always remove the generic representation of a sum type. A strict field of
-- a type without lazy parts, e.g. a text, is already in normal form.
instance NFData Value where
  rnf = \case
    Null -> ()
    Bool _ -> ()
    Int _ -> ()
    Float _ -> ()
    String _ -> ()
    Sequence xs -> rnf xs
    Mapping kvs -> rnf kvs
    Tagged _ v -> rnf v

-- | The value of a floating-point number. A finite value is exact, e.g. @0.1@
-- is exactly one tenth.
--
-- Arithmetic on a t'Data.Scientific.Scientific' with a huge exponent, e.g.
-- @1e1000000000@, can use all memory. Convert a value from an untrusted input
-- with 'floatValueToDouble' or with the bounded conversions of
-- "Data.Scientific".
data FloatValue
  = -- | A finite value other than negative zero.
    --
    -- The encoder writes a value whose exponent in scientific notation is
    -- beyond the range from -1000 to 1000, e.g. @1.0e+1001@, but the decoder
    -- rejects it. The decoder never gives such a value, and a 'Double' is
    -- always in the range.
    Finite !Sci.Scientific
  | -- | Negative zero, e.g. @-0.0@, which a t'Data.Scientific.Scientific'
    -- cannot hold.
    NegativeZero
  | Infinity
  | NegativeInfinity
  | NaN
  deriving stock (Eq, Ord, Show, Generic)

instance NFData FloatValue where
  rnf = rwhnf

-- | The nearest double, infinite if the value is out of its range.
--
-- >>> map floatValueToDouble [Finite 0.1, Finite 1e400, NegativeZero]
-- [0.1,Infinity,-0.0]
floatValueToDouble :: FloatValue -> Double
floatValueToDouble = toRealFloat

-- | The value of a double. A finite double becomes the shortest decimal that
-- reads back as the same double, e.g. @0.1@.
--
-- >>> map doubleToFloatValue [0.1, -0, 1 / 0]
-- [Finite 0.1,NegativeZero,Infinity]
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
--
-- >>> describe (Tagged "!point" (Mapping []))
-- "a mapping"
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
-- schema, e.g. 'intTag' for an v'Int'.
--
-- >>> map valueTag [Int 1, Tagged "!point" (Mapping [])]
-- ["tag:yaml.org,2002:int","!point"]
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

-- $setup
-- >>> import Yamlet
