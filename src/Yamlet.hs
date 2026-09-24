-- | A YAML 1.2.2 library.
--
-- Decode a configuration file:
--
-- @
-- data Config = Config
--   { name :: Text
--   , paths :: [FilePath]
--   }
--
-- instance FromYAML Config where
--   parseYAML = withMapping $ \\o -> do
--     rejectUnknownKeys ["name", "paths"] o
--     Config \<$> o .: "name" \<*> o .:? "paths" .!= []
--
-- main :: IO ()
-- main = do
--   input <- BS.readFile "config.yaml"
--   case decode input of
--     Left err -> putStrLn $ prettyError "config.yaml" err
--     Right config -> ...
-- @
module Yamlet
  ( -- * Decoding
    decode
  , decodeAll
  , decodeText
  , decodeAllText
  , decodeNodes
  , decodeInput

    -- * Syntax trees
  , decodeDocument
  , resolveDocument
  , toSyntax

    -- * Encoding
  , encode
  , encodeAll
  , encodeText
  , encodeAllText

    -- * Nodes
  , module Yamlet.Node

    -- * Conversion from nodes
  , module Yamlet.Decode

    -- * Conversion to nodes
  , ToYAML(..)
  , (.=)
  , mapping

    -- * Errors
  , module Yamlet.Error
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Yamlet.Decode
import Yamlet.Encode
import Yamlet.Error
import Yamlet.Internal.Compose
import Yamlet.Internal.Input
import Yamlet.Internal.Parser
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Node

-- | Decode a stream with one document. An empty stream is null.
decode :: FromYAML a => BS.ByteString -> Either Error a
decode bs = decodeInput bs >>= decodeText

-- | Decode every document of a stream.
decodeAll :: FromYAML a => BS.ByteString -> Either Error [a]
decodeAll bs = decodeInput bs >>= decodeAllText

-- | Decode a stream with one document. An empty stream is null.
decodeText :: FromYAML a => T.Text -> Either Error a
decodeText input = decodeNodes input >>= \case
  [] -> convert input (Node (Offset 0) nullTag Null)
  [n] -> convert input n
  _ : n : _ -> Left $ errorAt input n.offset "expected a single document"

-- | Decode every document of a stream.
decodeAllText :: FromYAML a => T.Text -> Either Error [a]
decodeAllText input = decodeNodes input >>= mapM (convert input)

-- | Parse a stream into the root nodes of its documents.
decodeNodes :: T.Text -> Either Error [Node]
decodeNodes input = parseStream input >>= mapM (compose input)

-- | Decode a document of a syntax tree, e.g. to read the values of a file and
-- keep its comments from one parse.
--
-- The text is the input of the document, for the line in an error. For a
-- document that the program built, the text can be empty.
decodeDocument :: FromYAML a => T.Text -> S.Document -> Either Error a
decodeDocument input doc = resolveDocument input doc >>= convert input

-- | Resolve the tags and the aliases of a document of a syntax tree. The
-- resolution fails for a duplicate key, an undefined alias or a value that is
-- not valid for its tag.
--
-- The text is the input of the document, for the line in an error. For a
-- document that the program built, the text can be empty.
resolveDocument :: T.Text -> S.Document -> Either Error Node
resolveDocument = compose

convert :: FromYAML a => T.Text -> Node -> Either Error a
convert input n = case runParser parseYAML n of
  Right a -> Right a
  Left (off, msg) -> Left $ errorAt input off msg

-- | Encode a value as a document.
encode :: ToYAML a => a -> BS.ByteString
encode = T.encodeUtf8 . encodeText

-- | Encode values as a stream of documents.
encodeAll :: ToYAML a => [a] -> BS.ByteString
encodeAll = T.encodeUtf8 . encodeAllText

encodeText :: ToYAML a => a -> T.Text
encodeText a = renderDocuments [toYAML a]

encodeAllText :: ToYAML a => [a] -> T.Text
encodeAllText = renderDocuments . map toYAML
