{-# LANGUAGE DeriveAnyClass #-}

-- | The representation graph of a YAML document: nodes with resolved tags and
-- values, and aliases replaced by the nodes that they refer to.
--
-- Most texts in the nodes share the memory of the input, so a node keeps the
-- whole input alive. To keep a text longer than the nodes, copy it with
-- 'Data.Text.copy'. The functions of "Yamlet.Decode" copy the texts that
-- they return.
--
-- An alias shares the memory of the node that it refers to, so a small input
-- with many aliases gives a small graph. But a function that visits every
-- node, e.g. 'Control.DeepSeq.force' or a 'Yamlet.FromYAML' instance for a
-- list, visits a node once for each alias path to it. For an untrusted input,
-- the time and the memory of such a function can be exponential in the size
-- of the input.
module Yamlet.Node
  ( -- * Nodes
    Node (..)
  , Value (..)
  , FloatValue (..)
  , floatToDouble
  , doubleToFloat
  , describe

    -- * Construction
  , node
  , S.noOffset
  , S.Offset (..)

    -- * Tags
  , nullTag
  , boolTag
  , intTag
  , floatTag
  , strTag
  , seqTag
  , mapTag
  , defaultTag
  ) where

import Control.DeepSeq
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import GHC.Generics

import Yamlet.Internal.Syntax qualified as S

-- | A node of a document.
data Node = Node
  { offset :: !S.Offset
  -- ^ The position of the node in the input, or 'noOffset' for a node that
  -- a program created.
  , tag :: !T.Text
  -- ^ The resolved tag, e.g. @tag:yaml.org,2002:str@.
  , value :: !Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The value of a node.
--
-- A scalar with a tag that the schema does not know is a 'String' with its
-- text, and the 'tag' of its node tells what it is.
data Value
  = Null
  | Bool !Bool
  | Int !Integer
  | Float !FloatValue
  | String !T.Text
  | Sequence [Node]
  | -- | The entries of a mapping in the order of the input. The keys are unique.
    Mapping [(Node, Node)]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The value of a floating-point number. A finite value is exact, e.g. @0.1@
-- is exactly one tenth.
--
-- Arithmetic on a 'Sci.Scientific' with a huge exponent, e.g. @1e1000000000@,
-- can use all memory. Convert a value from an untrusted input with
-- 'floatToDouble' or with the bounded conversions of "Data.Scientific".
data FloatValue
  = Finite !Sci.Scientific
  | Infinity
  | NegativeInfinity
  | NaN
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The nearest double, infinite if the value is out of its range.
floatToDouble :: FloatValue -> Double
floatToDouble = \case
  Finite s -> Sci.toRealFloat s
  Infinity -> 1 / 0
  NegativeInfinity -> -1 / 0
  NaN -> 0 / 0

-- | The value of a double. A finite double becomes the shortest decimal that
-- reads back as the same double, e.g. @0.1@.
doubleToFloat :: Double -> FloatValue
doubleToFloat d
  | isNaN d = NaN
  | isInfinite d = if d > 0 then Infinity else NegativeInfinity
  | otherwise = Finite (Sci.fromFloatDigits d)

-- | The kind of a value in plain words, for error messages, e.g. "a list".
describe :: Value -> String
describe = \case
  Null -> "null"
  Bool _ -> "a boolean"
  Int _ -> "an integer"
  Float _ -> "a floating-point number"
  String _ -> "a string"
  Sequence _ -> "a list"
  Mapping _ -> "a mapping"

-- | A node with the default tag for its value.
node :: Value -> Node
node v =
  Node
    { offset = S.noOffset
    , tag = defaultTag v
    , value = v
    }

nullTag, boolTag, intTag, floatTag, strTag, seqTag, mapTag :: T.Text
nullTag = "tag:yaml.org,2002:null"
boolTag = "tag:yaml.org,2002:bool"
intTag = "tag:yaml.org,2002:int"
floatTag = "tag:yaml.org,2002:float"
strTag = "tag:yaml.org,2002:str"
seqTag = "tag:yaml.org,2002:seq"
mapTag = "tag:yaml.org,2002:map"

-- | The tag of a value in the core schema.
defaultTag :: Value -> T.Text
defaultTag = \case
  Null -> nullTag
  Bool _ -> boolTag
  Int _ -> intTag
  Float _ -> floatTag
  String _ -> strTag
  Sequence _ -> seqTag
  Mapping _ -> mapTag
