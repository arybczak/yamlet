module Yamlet.Test.Decode.Input
  ( test_files
  , test_emptyStream
  , test_encodings
  , test_byteOrderMarks
  ) where

import Control.Exception
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory
import System.IO
import Test.Tasty.HUnit

import Yamlet
import Yamlet.Syntax qualified as S
import Yamlet.Test.Helpers

-- | The file functions write UTF-8 and read back what they wrote.
test_files :: Assertion
test_files = do
  dir <- getTemporaryDirectory
  (path, h) <- openTempFile dir "yamlet.yaml"
  hClose h
  flip finally (removeFile path) $ do
    let value = M.fromList @T.Text @T.Text [("name", "zażółć")]
    encodeFile path value
    bytes <- BS.readFile path
    assertEqual
      "UTF-8"
      (T.encodeUtf8 "name: zażółć\n")
      bytes
    decoded <- decodeFile path
    assertEqual
      "document"
      (Right value)
      decoded
    encodeAllFile @Int path [1, 2]
    documents <- decodeAllFile @Int path
    assertEqual
      "documents"
      (Right [1, 2])
      documents

test_emptyStream :: Assertion
test_emptyStream = do
  assertEqual
    "null"
    (Right Nothing)
    (decodeText @(Maybe Int) "# nothing\n")
  assertEqual
    "all"
    (Right [])
    (decodeAllText @Int "")

test_encodings :: Assertion
test_encodings = do
  let text = "key: zażółć \x1F600\n" :: T.Text
  assertEqual
    "UTF-8 with BOM"
    (Right text)
    (decodeInput ("\xEF\xBB\xBF" <> T.encodeUtf8 text) >>= stripBom)
  assertEqual
    "UTF-16LE"
    (Right text)
    (decodeInput (T.encodeUtf16LE text))
  assertEqual
    "UTF-16BE"
    (Right text)
    (decodeInput (T.encodeUtf16BE text))
  assertEqual
    "UTF-32LE"
    (Right text)
    (decodeInput (T.encodeUtf32LE text))
  assertEqual
    "UTF-32BE"
    (Right text)
    (decodeInput (T.encodeUtf32BE text))
  case decodeInput "a: b\n\xFF\n" of
    Left err ->
      assertEqual
        "invalid UTF-8"
        (2, 1)
        (err.location.line, err.location.column)
    Right _ -> assertFailure "expected an error"
  let stream = "a: 1\n---\n- b\n" :: T.Text
  case S.parseDocumentsText stream of
    Right docs -> do
      assertEqual
        "documents of the stream"
        2
        (length docs)
      assertEqual
        "documents parsed from UTF-16LE"
        (Right docs)
        (S.parseDocuments (T.encodeUtf16LE stream))
    Left err -> assertFailure (show err)
  case S.parseDocuments "a: b\n\xFF\n" of
    Left err ->
      assertEqual
        "documents parsed from invalid UTF-8"
        (2, 1, "invalid UTF-8")
        (err.location.line, err.location.column, err.message)
    Right _ -> assertFailure "expected an error"
  let invalid :: String -> T.Text -> BS.ByteString -> Assertion
      invalid preface msg bytes =
        assertEqual
          preface
          (Just (2, 3, T.unpack msg))
          (errorOf (first pure (decodeInput bytes)))
  invalid
    "lone surrogate in UTF-16LE"
    "invalid UTF-16"
    (T.encodeUtf16LE "a\nbc" <> "\x00\xD8" <> "d\0")
  invalid
    "odd length of UTF-16BE"
    "invalid UTF-16"
    (T.encodeUtf16BE "a\nbc" <> "\0")
  invalid
    "surrogate in UTF-32BE"
    "invalid UTF-32"
    (T.encodeUtf32BE "a\nbc" <> "\0\0\xDC\0")
  invalid
    "code point beyond Unicode in UTF-32LE"
    "invalid UTF-32"
    (T.encodeUtf32LE "a\nbc" <> "\0\0\x11\0")
  invalid
    "incomplete character in UTF-8"
    "invalid UTF-8"
    ("a\nbc" <> "\xE2\x82")
  invalid
    "surrogate in UTF-8"
    "invalid UTF-8"
    ("a\nbc" <> "\xED\xA0\x80" <> "d")
  let column :: String -> Int -> BS.ByteString -> Assertion
      column preface expected bytes =
        assertEqual
          preface
          (Just expected)
          ((\(_, c, _) -> c) <$> errorOf (decode @Value bytes))
  column
    "error after a UTF-8 BOM"
    1
    "\xEF\xBB\xBF]"
  column
    "error after a UTF-16 BOM"
    1
    "\xFF\xFE]\0"
  column
    "invalid UTF-8 after a BOM"
    2
    "\xEF\xBB\xBF\&b\xFF"
  column
    "error after a BOM between documents"
    1
    "a\n...\n\xEF\xBB\xBF]"
  column
    "error after two BOMs"
    1
    "\xEF\xBB\xBF\xEF\xBB\xBF]"
  column
    "error after two BOMs before a marker"
    5
    "a\n\xEF\xBB\xBF\xEF\xBB\xBF--- ]"
  let errorAfterBom :: String -> (Int, Int, String) -> T.Text -> Assertion
      errorAfterBom preface expected input =
        assertEqual
          preface
          (Just expected)
          (errorOf (decodeAllText @Value input))
  errorAfterBom
    "error after a BOM after an end marker"
    (3, 5, "unexpected ':', quote the value if it contains \": \"")
    "a\n...\n\xFEFF\&b: x: y\n"
  errorAfterBom
    "error after a BOM and a comment after an end marker"
    (4, 4, "unterminated flow sequence")
    "a\n...\n\xFEFF# c\n\xFEFF\&b: [\n"
  errorAfterBom
    "error after a second BOM at the start"
    (1, 4, "unterminated flow sequence")
    "\xFEFF\xFEFF\&a: [\n"
  errorAfterBom
    "tab below a BOM and a comment"
    (2, 2, "unexpected '%', a plain scalar cannot start with it, quote the value")
    "\xFEFF# c\n\t%x\n"
  assertEqual
    "source line after a BOM"
    (Left "]")
    $ either
      (Left . (.sourceLine) . NE.head)
      (const (Right ()))
      (decode @Value "\xEF\xBB\xBF]")
  assertEqual
    "source line at the line feed of a CRLF"
    "a: 1"
    (errorAt "a: 1\r\nb: 2\n" (Offset 5) "message").sourceLine
  where
    stripBom :: T.Text -> Either Error T.Text
    stripBom = Right . T.dropWhile (== '\xFEFF')

test_byteOrderMarks :: Assertion
test_byteOrderMarks = do
  let documents :: String -> [T.Text] -> T.Text -> Assertion
      documents preface expected input =
        assertEqual
          preface
          (Right expected)
          (decodeAllText input)
  documents
    "BOM before a marker after a scalar"
    ["a", "b"]
    "a\n\xFEFF--- b\n"
  assertEqual
    "BOM before a marker after a mapping"
    (Right [Mapping [(String "a", Int 1)], String "b"])
    (decodeAllText @Value "a: 1\n\xFEFF--- b\n")
  documents
    "BOM after an end marker"
    ["a", "b"]
    "a\n...\n\xFEFF# c\n\xFEFF\&b\n"
  documents
    "BOM in a quoted scalar"
    ["a\xFEFF", "b\xFEFF"]
    "--- \"a\xFEFF\"\n--- 'b\xFEFF'\n"
  let bom :: String -> (Int, Int) -> T.Text -> Assertion
      bom preface (l, c) input =
        assertEqual
          preface
          (Just (l, c, "unexpected byte order mark"))
          (errorOf (decodeAllText @Value input))
  bom
    "BOM at the start of a key"
    (2, 1)
    "a: 1\n\xFEFF b: 2\n"
  bom
    "BOM in a plain scalar"
    (1, 5)
    "a: x\xFEFFy\n"
  bom
    "BOM in a block scalar"
    (2, 3)
    "a: |\n  \xFEFFx\n"
  bom
    "BOM before a key"
    (2, 1)
    "a: b\n\xFEFF\&c: d\n"
  bom
    "BOM before a list item"
    (2, 1)
    "- a\n\xFEFF- b\n"
  bom
    "BOM before an indented value"
    (2, 1)
    "a:\n\xFEFF  b\n"
  assertEqual
    "BOM before a comment after a mapping"
    (Right [Mapping [(String "a", String "b")]])
    (decodeAllText @Value "a: b\n\xFEFF#c\n")
  documents
    "BOM before a comment after a scalar"
    ["a"]
    "a\n\xFEFF# c\n"
  documents
    "BOM at the end after a scalar"
    ["a"]
    "a\n\xFEFF"
  assertEqual
    "BOM at the end after a marker"
    (Right [Null])
    (decodeAllText @Value "---\n\xFEFF")
  bom
    "BOM before a scalar after a scalar"
    (2, 1)
    "a\n\xFEFF\&b\n"
  bom
    "BOM in a flow sequence"
    (2, 1)
    "a: [x,\n\xFEFF y]\n"
  bom
    "BOM in a flow mapping"
    (2, 1)
    "a: {x: 1,\n\xFEFF\&y: 2}\n"
  bom
    "BOM before a closing bracket"
    (2, 1)
    "a: [x,\n\xFEFF]\n"
  bom
    "BOM before a comment inside a mapping"
    (2, 1)
    "a: 1\n\xFEFF# c\nb: 2\n"
  bom
    "BOM on an empty line inside a mapping"
    (2, 1)
    "a: 1\n\xFEFF\nb: 2\n"
  bom
    "BOM before a comment inside a list"
    (3, 1)
    "a:\n  - 1\n\xFEFF  # c\n  - 2\n"
  bom
    "second BOM line inside a mapping"
    (3, 1)
    "a: 1\n# c\n\xFEFF# d\n\xFEFF\nb: 2\n"
  let errorAfter :: String -> (Int, Int, String) -> T.Text -> Assertion
      errorAfter preface expected input =
        assertEqual
          preface
          (Just expected)
          (errorOf (decodeAllText @Value input))
  errorAfter
    "error after a BOM in a double-quoted scalar"
    (2, 4, "unexpected '@', a plain scalar cannot start with it, quote the value")
    "\"x\n\xFEFFy\" @\n"
  errorAfter
    "error after a BOM in a single-quoted scalar"
    (2, 4, "unexpected 'z' after the end of a quoted scalar")
    "'x\n\xFEFFy' z\n"
  errorAfter
    "error after a BOM in a flow sequence"
    (2, 5, "unexpected '@', a plain scalar cannot start with it, quote the value")
    "[\"x\n\xFEFFy\", @]\n"
  assertEqual
    "BOM before a marker after an unterminated flow sequence"
    (Just (1, 4, "unterminated flow sequence"))
    (errorOf (decodeAllText @Value "a: [x,\n\xFEFF---\nb\n"))
  documents
    "BOM before a start marker in a double-quoted scalar"
    ["a \xFEFF--- "]
    "\"a\n\xFEFF---\n\"\n"
  documents
    "BOM before an end marker in a single-quoted scalar"
    ["a \xFEFF... b"]
    "'a\n\xFEFF... b'\n"
  documents
    "two BOMs before a marker"
    ["a", "b"]
    "a\n\xFEFF\xFEFF--- b\n"
  documents
    "two BOMs before a marker after an end marker"
    ["a", "b"]
    "--- a\n...\n\xFEFF\xFEFF--- b\n"
  documents
    "two BOMs before a marker after a block scalar"
    ["x\n", "b"]
    "--- |\n x\n\xFEFF\xFEFF--- b\n"
  documents
    "BOM before a marker after a literal at the top level"
    ["x\n", "b"]
    "--- |\nx\n\xFEFF--- b\n"
  documents
    "BOM before a marker after a folded at the top level"
    ["x y\n", "b"]
    "--- >\nx\ny\n\xFEFF--- b\n"
  documents
    "BOM before a comment after a literal at the top level"
    ["x\n"]
    "--- |\nx\n\xFEFF# c\n"
  documents
    "BOM before a marker as the first line of a literal"
    ["", "b"]
    "--- |\n\xFEFF--- b\n"
  -- The time to check a run of BOMs is linear in its length.
  documents
    "many BOMs at the start"
    ["a"]
    (T.replicate 400000 "\xFEFF" <> "a\n")
  documents
    "many BOMs after an end marker"
    ["a", "b"]
    ("a\n...\n" <> T.replicate 400000 "\xFEFF" <> "b\n")
