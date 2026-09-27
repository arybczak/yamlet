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
-- instance FromYaml Config where
--   parseYaml = withMapping $ \\o -> do
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
--
-- A field of type 'S.Node' keeps a part of the document as it was written,
-- and the encoder writes it back with its comments and styles:
--
-- @
-- data Workflow = Workflow
--   { name :: Text
--   , matrix :: Node
--   }
--
-- instance FromYaml Workflow where
--   parseYaml = withMapping $ \\o -> Workflow \<$> o .: "name" \<*> o .: "matrix"
--
-- instance ToYaml Workflow where
--   toYaml w = mapping ["name" .= w.name, "matrix" .= w.matrix]
-- @
module Yamlet
  ( -- * Decoding
    decode
  , decodeAll
  , decodeText
  , decodeAllText
  , decodeInput

    -- * Syntax trees
  , decodeDocument

    -- * Encoding
  , encode
  , encodeAll
  , encodeText
  , encodeAllText

    -- * Nodes
  , S.Node
  , S.Offset (..)

    -- * Comments of keys
  , S.Commented (..)
  , S.Comments (..)
  , S.noComments
  , S.Line (..)

    -- * Values
  , module Yamlet.Value

    -- * Conversion from nodes
  , module Yamlet.Decode

    -- * Conversion to nodes
  , ToYaml (..)
  , (.=)
  , mapping

    -- * Generic instances
  , module Yamlet.Generic

    -- * Errors
  , module Yamlet.Error
  ) where

import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Yamlet.Decode
import Yamlet.Encode
import Yamlet.Error
import Yamlet.Generic
import Yamlet.Internal.Compose
import Yamlet.Internal.Input
import Yamlet.Internal.Parser
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Value

-- | Decode a stream with one document. An empty stream is null.
decode :: FromYaml a => BS.ByteString -> Either Error a
decode bs = decodeInput bs >>= decodeText

-- | Decode every document of a stream.
decodeAll :: FromYaml a => BS.ByteString -> Either Error [a]
decodeAll bs = decodeInput bs >>= decodeAllText

-- | Decode a stream with one document. An empty stream is null.
decodeText :: FromYaml a => T.Text -> Either Error a
decodeText input =
  parseStream input >>= \case
    [] -> convert input (S.Node (S.Offset 0) (S.Offset 0) S.noProps S.noComments (S.Scalar S.Plain ""))
    [doc] -> convert input (documentRoot doc)
    docs@(_ : doc : _) -> do
      mapM_ (\d -> first (uncurry (decoderError input d.root)) (prepare d.root)) docs
      Left $ errorAt input doc.root.offset "expected a single document, but got a second one"

-- | Decode every document of a stream.
decodeAllText :: FromYaml a => T.Text -> Either Error [a]
decodeAllText input = parseStream input >>= mapM (convert input . documentRoot)

-- | Decode a document of a syntax tree, e.g. to read the values of a file and
-- keep its comments from one parse.
--
-- As for a parsed input, the decoder checks the document first. The check
-- fails for a duplicate key, an undefined alias, aliases beyond the limit in
-- "Yamlet.Value", a value that is not valid for its tag or a float whose
-- exponent and value are both beyond the range from -1000 to 1000 in
-- scientific notation.
--
-- The text is the input of the document, for the line in an error. For a
-- document that the program built, the text can be empty.
decodeDocument :: FromYaml a => T.Text -> S.Document -> Either Error a
decodeDocument input doc = convert input (documentRoot doc)

-- | The root of a document with the lines of the document, e.g. the lines
-- before a @---@ marker and at the end of the document, so that a decoder
-- can keep them. The renderer writes them at the same places. The comment on
-- the line of the marker becomes a line above the root.
documentRoot :: S.Document -> S.Node
documentRoot doc
  | null dc.before && isNothing dc.inline && null dc.after = r
  | otherwise = S.Node r.offset r.endOffset r.props comments r.content
  where
    dc :: S.Comments
    dc = doc.docComments

    r :: S.Node
    r = doc.root

    comments :: S.Comments
    comments =
      S.Comments
        { S.before = dc.before ++ [S.Comment c | Just c <- [dc.inline]] ++ r.comments.before
        , S.inline = r.comments.inline
        , S.after = r.comments.after ++ dc.after
        }

convert :: FromYaml a => T.Text -> S.Node -> Either Error a
convert input n = case runParser parseYaml n of
  Right a -> Right a
  Left (off, msg) -> Left $ decoderError input n off msg

-- | An error of the decoder in the document with the root, with the path to
-- the node at the offset.
decoderError :: T.Text -> S.Node -> S.Offset -> String -> Error
decoderError input root off msg = (errorAt input off msg) {path = nodePath off root}

-- | Encode a value as a document.
encode :: ToYaml a => a -> BS.ByteString
encode = T.encodeUtf8 . encodeText

-- | Encode values as a stream of documents.
encodeAll :: ToYaml a => [a] -> BS.ByteString
encodeAll = T.encodeUtf8 . encodeAllText

-- | Encode a value as a document.
encodeText :: ToYaml a => a -> T.Text
encodeText a = renderDocuments [toYaml a]

-- | Encode values as a stream of documents.
encodeAllText :: ToYaml a => [a] -> T.Text
encodeAllText = renderDocuments . map toYaml
