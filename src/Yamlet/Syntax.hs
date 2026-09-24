{-# LANGUAGE DeriveAnyClass #-}
-- | The representation of a YAML stream that keeps every detail of the
-- presentation: the styles of scalars and collections, anchors, aliases and
-- unresolved tags.
--
-- Most texts in the tree share the memory of the input, so a node keeps the
-- whole input alive. To keep a text longer than the tree, copy it with
-- 'Data.Text.copy'.
module Yamlet.Syntax
  ( -- * Documents
    Document(..)
  , Version(..)

    -- * Nodes
  , Node(..)
  , nodeOffset
  , Props(..)
  , noProps
  , Tag(..)
  , ScalarStyle(..)
  , CollectionStyle(..)

    -- * Positions
  , Offset(..)
  ) where

import Control.DeepSeq
import Data.Text qualified as T
import GHC.Generics

-- | A document of a YAML stream.
data Document = Document
  { version :: !(Maybe Version)
  -- ^ The version from the @%YAML@ directive.
  , explicitStart :: !Bool
  -- ^ The document starts with a @---@ marker.
  , explicitEnd :: !Bool
  -- ^ The document ends with a @...@ marker.
  , root :: !Node
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

-- | The version of YAML that a document declares.
data Version = Version
  { major :: !Int
  , minor :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

-- | A node of a document.
data Node
  = Scalar !Offset !Props !ScalarStyle !T.Text
  | Sequence !Offset !Props !CollectionStyle [Node]
  | Mapping !Offset !Props !CollectionStyle [(Node, Node)]
  | Alias !Offset !T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

-- | The position of the first character of the node content.
nodeOffset :: Node -> Offset
nodeOffset = \case
  Scalar o _ _ _ -> o
  Sequence o _ _ _ -> o
  Mapping o _ _ _ -> o
  Alias o _ -> o

-- | The properties of a node.
data Props = Props
  { anchor :: !(Maybe T.Text)
  , tag :: !Tag
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

-- | No anchor and no tag.
noProps :: Props
noProps = Props Nothing NoTag

-- | The tag of a node after the tag handles are expanded.
data Tag
  = NoTag
  -- ^ The node has no tag.
  | NonSpecificTag
  -- ^ The @!@ tag.
  | Tag !T.Text
  -- ^ A specific tag, e.g. @tag:yaml.org,2002:str@ for @!!str@.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

data ScalarStyle
  = Plain
  | SingleQuoted
  | DoubleQuoted
  | Literal
  | Folded
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass NFData

data CollectionStyle
  = Block
  | Flow
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass NFData

-- | The offset of a byte in the UTF-8 encoded input.
newtype Offset = Offset Int
  deriving newtype (Eq, Ord, Show, NFData)
