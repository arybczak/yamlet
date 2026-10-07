-- | A YAML 1.2.2 library.
--
-- The library handles two typical use cases well:
--
-- 1. Decoding a document that describes a configuration:
--
--     >>> :{
--     data Config = Config
--       { name :: T.Text
--       , paths :: [FilePath]
--       }
--       deriving stock (Generic, Show)
--       deriving (FromYaml) via GenericYaml Config
--     instance GenericYamlOptions Config where
--       yamlDefault = Just Config {name = requiredField, paths = ["."]}
--     :}
--
--     >>> input = "name: app\n"
--
--     >>> T.putStr input
--     name: app
--
--     >>> either printErrors print (decodeText @Config input)
--     Config {name = "app", paths = ["."]}
--
--     When a document fails to decode, you get multiple errors pointing at what
--     failed and why:
--
--     >>> input = "paths:\n- src\n- 42\nport: 80\n"
--
--     >>> T.putStr input
--     paths:
--     - src
--     - 42
--     port: 80
--
--     >>> either printErrors print (decodeText @Config input)
--     input.yaml:1:1: missing key "name"
--       |
--     1 | paths:
--       | ^
--     input.yaml:3:3: paths[1]: expected a string, but got an integer, quote the value, e.g. '42'
--       |
--     3 | - 42
--       |   ^
--     input.yaml:4:1: unknown key "port", expected one of: name, paths
--       |
--     4 | port: 80
--       | ^
--
-- 2. Decoding a document into a Haskell type and encoding it back. A type
--    can keep the comments of a value with t'Yamlet.Commented', and a part
--    of the document as it was written with t'Yamlet.Node':
--
--     >>> :{
--     data Workflow = Workflow
--       { name :: Commented T.Text
--       , matrix :: Node
--       }
--       deriving stock (Generic)
--       deriving anyclass (GenericYamlOptions)
--       deriving (FromYaml, ToYaml) via GenericYaml Workflow
--     :}
--
--     >>> input = "# The name in the UI.\nname: build # short\nmatrix:\n  # Each system runs the jobs.\n  os: [linux, macos]\n"
--
--     >>> T.putStr input
--     # The name in the UI.
--     name: build # short
--     matrix:
--       # Each system runs the jobs.
--       os: [linux, macos]
--
--     >>> Right workflow = decodeText @Workflow input
--
--     The decoded name keeps the comment above its key and the comment after
--     its value:
--
--     >>> print workflow.name
--     Commented {value = "build", comments = Comments {before = [Comment "The name in the UI."], inline = Just "short", after = []}}
--
--     >>> T.putStr (encodeText workflow)
--     # The name in the UI.
--     name: build # short
--     matrix:
--       # Each system runs the jobs.
--       os: [linux, macos]
module Yamlet
  ( -- * Decoding
    decode
  , decodeAll
  , decodeText
  , decodeAllText
  , decodeInput
  , decodeFile
  , decodeAllFile

    -- * Syntax trees
  , decodeWithDocument
  , decodeDocument
  , decodeDocuments

    -- * Encoding
  , encode
  , encodeAll
  , encodeText
  , encodeAllText
  , encodeFile
  , encodeAllFile

    -- * Nodes
  , S.Node
  , S.Offset (..)
  , S.noOffset
  , S.Located (..)

    -- * Comments
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

import Control.Monad
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
import Yamlet.Internal.FromYaml
import Yamlet.Internal.Input
import Yamlet.Internal.Parser
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Internal.Utils
import Yamlet.Value

-- | Decode a stream with one document. An empty stream is null.
--
-- If the input has a syntax error or fails a check that 'decodeDocument'
-- describes, the result has only that error, with its notes, e.g. the first
-- key of a duplicate key. Otherwise the result has every error that the
-- 'Parser' collects, in the order of their positions.
--
-- >>> decode @[Int] "- 1\n- 2\n"
-- Right [1,2]
--
-- >>> decode @(Maybe Int) ""
-- Right Nothing
--
-- >>> either printErrors print (decode @[Int] "- 1\n- x\n- true\n")
-- input.yaml:2:3: [1]: expected an integer, but got a string
--   |
-- 2 | - x
--   |   ^
-- input.yaml:3:3: [2]: expected an integer, but got a boolean
--   |
-- 3 | - true
--   |   ^
decode :: FromYaml a => BS.ByteString -> Either (NE.NonEmpty Error) a
decode bs = single (decodeInput bs) >>= decodeText

-- | Decode every document of a stream. The errors are those of the first
-- document that fails, as for 'decode'.
--
-- >>> decodeAll @Int "1\n---\n2\n"
-- Right [1,2]
decodeAll :: FromYaml a => BS.ByteString -> Either (NE.NonEmpty Error) [a]
decodeAll bs = single (decodeInput bs) >>= decodeAllText

-- | Decode a stream with one document. An empty stream is null. The errors
-- are as for 'decode'.
--
-- >>> decodeText @Value "ports: [80, 443]\nenabled: yes\n"
-- Right (Mapping [(String "ports",Sequence [Int 80,Int 443]),(String "enabled",String "yes")])
decodeText :: FromYaml a => T.Text -> Either (NE.NonEmpty Error) a
decodeText input = do
  (a, _) <- decodeWithDocument input
  pure a

-- | Decode a stream with one document as 'decodeText' does, and give the
-- document too, e.g. for 'documentErrors' or to write the file back with its
-- comments. An empty stream is a document with null.
decodeWithDocument :: FromYaml a => T.Text -> Either (NE.NonEmpty Error) (a, S.Document)
decodeWithDocument input =
  single (parseStream input) >>= \case
    [] -> withDocument (S.document (S.Node (S.Offset 0) (S.Offset 0) S.noProps S.noComments (S.ScalarContent S.Plain "")))
    [doc] -> withDocument doc
    docs@(_ : doc : _) -> do
      let limit = aliasLimit (map (.root) docs)
          check :: Int -> S.Document -> Either (NE.NonEmpty Error) Int
          check added d = snd <$> first (decoderErrors input d) (prepareWithin limit added d.root)
      foldM_ check 0 docs
      single . Left $ errorAt input doc.root.offset "expected a single document, but got a second one"
  where
    withDocument :: FromYaml a => S.Document -> Either (NE.NonEmpty Error) (a, S.Document)
    withDocument doc = do
      a <- decodeDocument input doc
      pure (a, doc)

-- | Decode every document of a stream. The errors are as for 'decodeAll'.
decodeAllText :: FromYaml a => T.Text -> Either (NE.NonEmpty Error) [a]
decodeAllText input = single (parseStream input) >>= decodeDocuments input

single :: Either Error a -> Either (NE.NonEmpty Error) a
single = first (NE.:| [])

-- | Decode a document of a syntax tree, e.g. to read the values of a file and
-- keep its comments from one parse.
--
-- As for a parsed input, the decoder checks the document first. The check
-- fails for:
--
-- * a duplicate key,
--
-- * an undefined alias,
--
-- * aliases beyond the limit in "Yamlet.Value",
--
-- * a value that is not valid for its tag,
--
-- * a float whose exponent in scientific notation is beyond the range
--   from -1000 to 1000.
--
-- The text is the input of the document. An error takes its line from the
-- text. For a document that the program built, the text can be empty. The
-- errors are as for 'decode'.
--
-- The document has the limit of the aliases to itself. For the documents of
-- a stream, use 'decodeDocuments', so that they share the limit.
decodeDocument :: FromYaml a => T.Text -> S.Document -> Either (NE.NonEmpty Error) a
decodeDocument input doc = firstOfResult $ decodeDocumentWithin (aliasLimit [doc.root]) 0 input doc

-- | Decode the documents of a syntax tree as 'decodeDocument' does, e.g. the
-- documents of a stream from 'Yamlet.Syntax.parseDocumentsText'. The
-- documents share the limit of the aliases, as the documents of a stream do.
-- The errors are as for 'decodeAll'.
decodeDocuments :: FromYaml a => T.Text -> [S.Document] -> Either (NE.NonEmpty Error) [a]
decodeDocuments input docs = go 0 docs
  where
    limit :: Int
    limit = aliasLimit (map (.root) docs)

    go :: FromYaml a => Int -> [S.Document] -> Either (NE.NonEmpty Error) [a]
    go added = \case
      [] -> Right []
      d : ds -> do
        (a, added') <- decodeDocumentWithin limit added input d
        (a :) <$> go added' ds

-- | 'decodeDocument' with the visits of the aliases as for 'prepareWithin'.
decodeDocumentWithin :: FromYaml a => Int -> Int -> T.Text -> S.Document -> Either (NE.NonEmpty Error) (a, Int)
decodeDocumentWithin limit added input doc =
  first (decoderErrors input doc) (runParserWithin limit added parseYaml root)
  where
    -- The root with the lines of the document, e.g. the lines above a @---@
    -- marker and below a @...@ marker, so that a decoder can keep them. The
    -- renderer writes them at the same places. The comment on the line of the
    -- marker becomes a line above the root.
    root :: S.Node
    root
      | null dc.before && isNothing dc.inline && null dc.after = r
      | otherwise = S.withComments comments r

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

-- | The errors of the decoder in the document, with their paths.
decoderErrors :: T.Text -> S.Document -> NE.NonEmpty (S.Offset, String) -> NE.NonEmpty Error
decoderErrors input doc = NE.fromList . documentErrors input doc . NE.toList

-- | Encode a value as a document.
--
-- >>> encode [1, 2 :: Int]
-- "- 1\n- 2\n"
encode :: ToYaml a => a -> BS.ByteString
encode = T.encodeUtf8 . encodeText

-- | Encode values as a stream of documents.
encodeAll :: ToYaml a => [a] -> BS.ByteString
encodeAll = T.encodeUtf8 . encodeAllText

-- | Encode a value as a document.
--
-- >>> T.putStr (encodeText (mapping ["name" .= ("app" :: T.Text), "ports" .= [80, 443 :: Int]]))
-- name: app
-- ports:
-- - 80
-- - 443
encodeText :: ToYaml a => a -> T.Text
encodeText a = renderDocuments [toYaml a]

-- | Encode values as a stream of documents.
--
-- >>> T.putStr (encodeAllText [1, 2 :: Int])
-- 1
-- ---
-- 2
encodeAllText :: ToYaml a => [a] -> T.Text
encodeAllText = renderDocuments . map toYaml

-- | Decode the file as 'decode' does. The file is read as bytes, so the
-- encoding does not depend on the locale. It is UTF-8, UTF-16 or UTF-32,
-- detected as the YAML specification describes.
--
-- For the errors, give the same path to 'prettyError':
--
-- @
-- decodeFile \@Config path >>= \\case
--   Left errs -> mapM_ (putStrLn . prettyError path) errs
--   Right config -> ...
-- @
decodeFile :: FromYaml a => FilePath -> IO (Either (NE.NonEmpty Error) a)
decodeFile path = decode <$> BS.readFile path

-- | Decode every document of the file as 'decodeAll' does, with the encoding
-- of 'decodeFile'.
decodeAllFile :: FromYaml a => FilePath -> IO (Either (NE.NonEmpty Error) [a])
decodeAllFile path = decodeAll <$> BS.readFile path

-- | Encode a value as a document in the file, in UTF-8. The file is written
-- as bytes, so the encoding does not depend on the locale.
encodeFile :: ToYaml a => FilePath -> a -> IO ()
encodeFile path = BS.writeFile path . encode

-- | Encode values as a stream of documents in the file, as 'encodeFile'
-- does.
encodeAllFile :: ToYaml a => FilePath -> [a] -> IO ()
encodeAllFile path = BS.writeFile path . encodeAll

-- $setup
-- >>> import Data.Text qualified as T
-- >>> import Data.Text.IO qualified as T
-- >>> import Yamlet
-- >>> printErrors = mapM_ (putStrLn . prettyError "input.yaml")
