-- | The representation of a YAML stream that keeps every detail of the
-- presentation: the styles of scalars and collections, anchors, aliases and
-- unresolved tags.
--
-- Most texts in the tree share the memory of the input, so a node keeps the
-- whole input alive. To keep a text longer than the tree, copy it with
-- 'Data.Text.copy', or copy the whole tree with 'copyDocument'.
module Yamlet.Syntax
  ( -- * Parsing
    parseDocuments
  , parseDocumentsText
  , copyDocument
  , copyNode

    -- * Documents
  , Document(..)
  , Version(..)

    -- * Nodes
  , Node(..)
  , nodeOffset
  , Props(..)
  , noProps
  , Tag(..)
  , ScalarStyle(..)
  , CollectionStyle(..)

    -- ** Construction
  , scalarNode
  , plainNode
  , sequenceNode
  , mappingNode

    -- * Positions
  , Offset(..)
  , noOffset
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T

import Yamlet.Error
import Yamlet.Internal.Input
import Yamlet.Internal.Parser
import Yamlet.Internal.Syntax

-- | Parse the documents of a stream. The encoding is UTF-8, UTF-16 or UTF-32,
-- detected as the YAML specification describes.
parseDocuments :: BS.ByteString -> Either Error [Document]
parseDocuments bs = decodeInput bs >>= parseStream

-- | Parse the documents of a stream.
parseDocumentsText :: T.Text -> Either Error [Document]
parseDocumentsText = parseStream

-- | A scalar in the given style, without properties.
scalarNode :: ScalarStyle -> T.Text -> Node
scalarNode = Scalar noOffset noProps

-- | A plain scalar without properties.
plainNode :: T.Text -> Node
plainNode = scalarNode Plain

-- | A block sequence without properties.
sequenceNode :: [Node] -> Node
sequenceNode = Sequence noOffset noProps Block

-- | A block mapping without properties.
mappingNode :: [(Node, Node)] -> Node
mappingNode = Mapping noOffset noProps Block

-- | Copy every text of a document, so that the document does not keep the
-- input alive.
copyDocument :: Document -> Document
copyDocument doc = doc { root = copyNode doc.root }

-- | Copy every text of a node, so that the node does not keep the input
-- alive.
copyNode :: Node -> Node
copyNode = \case
  Scalar off props style t -> Scalar off (copyProps props) style (T.copy t)
  Sequence off props style xs -> Sequence off (copyProps props) style (map copyNode xs)
  Mapping off props style kvs ->
    Mapping off (copyProps props) style [ (copyNode k, copyNode v) | (k, v) <- kvs ]
  Alias off name -> Alias off (T.copy name)
  where
    copyProps :: Props -> Props
    copyProps props = Props
      { anchor = T.copy <$> props.anchor
      , tag = case props.tag of
          Tag t -> Tag (T.copy t)
          t -> t
      }
