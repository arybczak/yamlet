-- | The representation of a YAML stream that keeps every detail of the
-- presentation: the styles of scalars and collections, anchors, aliases and
-- unresolved tags.
--
-- Most texts in the tree share the memory of the input, so a node keeps the
-- whole input alive. To keep a text longer than the tree, copy it with
-- 'Data.Text.copy'.
module Yamlet.Syntax
  ( -- * Parsing
    parseDocuments
  , parseDocumentsText

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

    -- * Positions
  , Offset(..)
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
