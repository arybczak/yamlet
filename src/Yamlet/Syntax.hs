-- | The representation of a YAML stream that keeps every detail of the
-- presentation: the styles of scalars and collections, anchors, aliases,
-- unresolved tags, comments and empty lines.
--
-- Most texts in the tree share the memory of the input, so a node keeps the
-- whole input alive. To keep a text longer than the tree, copy it with
-- 'Data.Text.copy', or copy the whole tree with 'copyDocument'.
--
-- = Comments
--
-- The parser gives each comment to one node, and the renderer writes it back
-- at the place of that node:
--
-- * A comment on a line of its own belongs to the node below it. If several
--   nodes start on the line below, it belongs to the largest one, e.g. a
--   comment above the first entry of a mapping belongs to the mapping.
--
-- * A comment at the end of a line belongs to the node that ends last before
--   it on that line, e.g. to the value in @key: value # comment@ and to the
--   key in @key: # comment@. A comment on the line of a block scalar header
--   belongs to the block scalar.
--
-- * A comment after the last entry of a block collection belongs to the end
--   of the collection if it is indented at least as deep as the entries, and
--   deeper than the key of the collection. Otherwise it belongs to the node
--   below it.
--
-- * A comment before the directives or the @---@ marker of a document, or on
--   the line of the marker, belongs to the document. A comment with no node
--   below it belongs to the end of the document.
--
-- Empty lines go with the comments that follow them, or with the node below
-- them. Several empty lines in a row count as one.
module Yamlet.Syntax
  ( -- * Parsing
    parseDocuments
  , parseDocumentsText
  , decodeInput
  , copyDocument
  , copyNode

    -- * Rendering
  , renderSyntax
  , RenderOptions(..)
  , defaultRenderOptions

    -- * Documents
  , Document(..)
  , Version(..)

    -- * Nodes
  , Node(..)
  , Content(..)
  , Props(..)
  , noProps
  , Tag(..)
  , ScalarStyle(..)
  , CollectionStyle(..)

    -- ** Construction
  , contentNode
  , scalarNode
  , plainNode
  , sequenceNode
  , mappingNode

    -- * Comments
  , Comments(..)
  , noComments
  , Line(..)

    -- * Positions
  , Offset(..)
  , noOffset
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T

import Yamlet.Error
import Yamlet.Internal.Input
import Yamlet.Internal.Parser
import Yamlet.Internal.Render
import Yamlet.Internal.Syntax

-- | Parse the documents of a stream. The encoding is UTF-8, UTF-16 or UTF-32,
-- detected as the YAML specification describes.
--
-- 'errorAt' needs the text of the input. To report errors of your own, e.g.
-- for a key that the program does not know, decode the input with
-- 'decodeInput' and parse it with 'parseDocumentsText'.
parseDocuments :: BS.ByteString -> Either Error [Document]
parseDocuments bs = decodeInput bs >>= parseStream

-- | Parse the documents of a stream.
parseDocumentsText :: T.Text -> Either Error [Document]
parseDocumentsText = parseStream

-- | A node with the given content, without properties and comments.
contentNode :: Content -> Node
contentNode c = Node
  { offset = noOffset
  , endOffset = noOffset
  , props = noProps
  , comments = noComments
  , content = c
  }

-- | A scalar in the given style. If the style cannot hold the text,
-- 'renderSyntax' uses quotes.
scalarNode :: ScalarStyle -> T.Text -> Node
scalarNode style = contentNode . Scalar style

-- | A plain scalar.
plainNode :: T.Text -> Node
plainNode = scalarNode Plain

-- | A block sequence.
sequenceNode :: [Node] -> Node
sequenceNode = contentNode . Sequence Block

-- | A block mapping.
mappingNode :: [(Node, Node)] -> Node
mappingNode = contentNode . Mapping Block

-- | Copy every text of a document, so that the document does not keep the
-- input alive.
copyDocument :: Document -> Document
copyDocument doc = doc
  { docComments = copyComments doc.docComments
  , root = copyNode doc.root
  }

-- | Copy every text of a node, so that the node does not keep the input
-- alive.
copyNode :: Node -> Node
copyNode n = n
  { props = Props
      { anchor = T.copy <$> n.props.anchor
      , tag = case n.props.tag of
          Tag t -> Tag (T.copy t)
          t -> t
      }
  , comments = copyComments n.comments
  , content = case n.content of
      Scalar style t -> Scalar style (T.copy t)
      Sequence style xs -> Sequence style (map copyNode xs)
      Mapping style kvs -> Mapping style [ (copyNode k, copyNode v) | (k, v) <- kvs ]
      Alias name -> Alias (T.copy name)
  }

copyComments :: Comments -> Comments
copyComments c = Comments
  { before = map copyLine c.before
  , inline = T.copy <$> c.inline
  , after = map copyLine c.after
  }
  where
    copyLine :: Line -> Line
    copyLine = \case
      Comment t -> Comment (T.copy t)
      EmptyLine -> EmptyLine
