{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | The types of the syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Syntax
  ( -- * Documents
    Document (..)
  , Version (..)

    -- * Nodes
  , Node (..)
  , Content (..)
  , Props (..)
  , noProps
  , Tag (..)
  , ScalarStyle (..)
  , CollectionStyle (..)

    -- * Comments
  , Comments (..)
  , noComments
  , Line (..)

    -- * Positions
  , Offset (..)
  , noOffset
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
  , docComments :: !Comments
  -- ^ The lines before the directives or the @---@ marker, the comment on the
  -- line of the marker and the lines at the end of the document.
  , root :: !Node
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The version of YAML that a document declares.
data Version = Version
  { major :: !Int
  , minor :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A node of a document.
data Node = Node
  { offset :: !Offset
  -- ^ The position of the first character of the content.
  , endOffset :: !Offset
  -- ^ The position after the last character of the content.
  , props :: !Props
  , comments :: !Comments
  , content :: !Content
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The content of a node.
data Content
  = Scalar !ScalarStyle !T.Text
  | Sequence !CollectionStyle [Node]
  | Mapping !CollectionStyle [(Node, Node)]
  | -- | An alias has no properties.
    Alias !T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The properties of a node.
data Props = Props
  { anchor :: !(Maybe T.Text)
  , tag :: !Tag
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | No anchor and no tag.
noProps :: Props
noProps = Props Nothing NoTag

-- | The tag of a node after the tag handles are expanded.
data Tag
  = -- | The node has no tag.
    NoTag
  | -- | The @!@ tag.
    NonSpecificTag
  | -- | A specific tag, e.g. @tag:yaml.org,2002:str@ for @!!str@.
    Tag !T.Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The style of a scalar: without quotes, in single or double quotes, or a
-- literal (@|@) or folded (@>@) block scalar.
data ScalarStyle
  = Plain
  | SingleQuoted
  | DoubleQuoted
  | Literal
  | Folded
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | The style of a collection: with indentation, or with brackets and commas.
data CollectionStyle
  = Block
  | Flow
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | The comments and the empty lines that belong to a node.
data Comments = Comments
  { before :: [Line]
  -- ^ The lines above the node.
  , inline :: !(Maybe T.Text)
  -- ^ The comment at the end of the first line of the node. The renderer
  -- writes a line break in it as a space.
  , after :: [Line]
  -- ^ The lines after the last entry of a collection, or between the brackets
  -- of an empty collection.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | No comments and no empty lines.
noComments :: Comments
noComments = Comments [] Nothing []

-- | A line of comments. Several empty lines in a row count as one.
data Line
  = EmptyLine
  | -- | The text after the @#@ and one space, without the white space at its
    -- end. The renderer writes a text with line breaks as several comment
    -- lines, and the parser reads them back as several comments.
    Comment !T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The offset of a byte in the input text, in its UTF-8 encoding. For an
-- input in UTF-16 or UTF-32, the offset counts the bytes of the text after
-- 'Yamlet.Syntax.decodeInput', not the bytes of the input.
newtype Offset = Offset Int
  deriving newtype (Eq, Ord, Show, NFData)

-- | The offset of a node that does not come from an input.
noOffset :: Offset
noOffset = Offset (-1)
