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
--   parseYaml = withMapping $ \\o ->
--     rejectUnknownKeys ["name", "paths"] o
--       *> (Config \<$> o .: "name" \<*> o .:? "paths" .!= [])
--
-- main :: IO ()
-- main = do
--   input <- BS.readFile "config.yaml"
--   case decode input of
--     Left errs -> mapM_ (putStrLn . prettyError "config.yaml") errs
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
  , S.noOffset
  , S.Located (..)

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
  , module Yamlet.Encode

    -- * Generic instances
  , module Yamlet.Generic

    -- * Errors
  , module Yamlet.Error
  ) where

import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Yamlet.Decode
import Yamlet.Encode
import Yamlet.Error
import Yamlet.Generic
import Yamlet.Internal.Compose
import Yamlet.Internal.Encoder
import Yamlet.Internal.Input
import Yamlet.Internal.Parser
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Syntax qualified as S
import Yamlet.Value

-- | Decode a stream with one document. An empty stream is null.
--
-- A syntax error, or an error of the checks that 'decodeDocument' describes,
-- is the only error. Otherwise the result has every error of the decoder that
-- 'Parser' collects, in the order of their positions.
decode :: FromYaml a => BS.ByteString -> Either (NE.NonEmpty Error) a
decode bs = single (decodeInput bs) >>= decodeText

-- | Decode every document of a stream. The errors are those of the first
-- document that fails, as for 'decode'.
decodeAll :: FromYaml a => BS.ByteString -> Either (NE.NonEmpty Error) [a]
decodeAll bs = single (decodeInput bs) >>= decodeAllText

-- | Decode a stream with one document. An empty stream is null. The errors
-- are as for 'decode'.
decodeText :: FromYaml a => T.Text -> Either (NE.NonEmpty Error) a
decodeText input =
  single (parseStream input) >>= \case
    [] -> convert input (S.document (S.Node (S.Offset 0) (S.Offset 0) S.noProps S.noComments (S.Scalar S.Plain "")))
    [doc] -> convert input doc
    docs@(_ : doc : _) -> single $ do
      mapM_ (\d -> first (uncurry (decoderError input d.root)) (prepare d.root)) docs
      Left $ errorAt input doc.root.offset "expected a single document, but got a second one"

-- | Decode every document of a stream. The errors are as for 'decodeAll'.
decodeAllText :: FromYaml a => T.Text -> Either (NE.NonEmpty Error) [a]
decodeAllText input = single (parseStream input) >>= mapM (convert input)

single :: Either Error a -> Either (NE.NonEmpty Error) a
single = first (NE.:| [])

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
-- document that the program built, the text can be empty. The errors are as
-- for 'decode'.
decodeDocument :: FromYaml a => T.Text -> S.Document -> Either (NE.NonEmpty Error) a
decodeDocument = convert

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

convert :: FromYaml a => T.Text -> S.Document -> Either (NE.NonEmpty Error) a
convert input doc =
  first (NE.fromList . documentErrors input doc . NE.toList) (runParser parseYaml (documentRoot doc))

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
