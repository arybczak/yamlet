{-# LANGUAGE DeriveAnyClass #-}
-- | The representation graph of a YAML document: nodes with resolved tags and
-- values, and aliases replaced by the nodes that they refer to.
module Yamlet.Node
  ( -- * Nodes
    Node(..)
  , Value(..)
  , describe

    -- * Construction
  , node
  , noOffset
  , S.Offset(..)

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
import Data.Text qualified as T
import GHC.Generics

import Yamlet.Syntax qualified as S

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
  deriving anyclass NFData

-- | The value of a node.
--
-- A scalar with a tag that the schema does not know is a 'String' with its
-- text, and the 'tag' of its node tells what it is.
data Value
  = Null
  | Bool !Bool
  | Int !Integer
  | Float !Double
  | String !T.Text
  | Sequence [Node]
  | Mapping [(Node, Node)]
  -- ^ The entries of a mapping in the order of the input. The keys are unique.
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

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
node v = Node
  { offset = noOffset
  , tag = defaultTag v
  , value = v
  }

-- | The offset of a node that does not come from an input.
noOffset :: S.Offset
noOffset = S.Offset (-1)

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
