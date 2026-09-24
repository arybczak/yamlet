module DecodeTests (decodeTests) where

import Data.Int
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Internal qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet
import Yamlet.Schema
import Yamlet.Syntax qualified as S

decodeTests :: TestTree
decodeTests = testGroup "Decode"
  [ testCase "core schema" test_coreSchema
  , testProperty "floats" prop_floats
  , testCase "plain scalars" test_plainSafe
  , testCase "record" test_record
  , testCase "copies" test_copies
  , testCase "aliases" test_aliases
  , testCase "empty stream" test_emptyStream
  , testCase "encodings" test_encodings
  , testGroup "errors"
    [ testCase "syntax" test_syntaxErrors
    , testCase "types" test_typeErrors
    , testCase "keys" test_keyErrors
    , testCase "pretty" test_prettyError
    ]
  ]

test_coreSchema :: Assertion
test_coreSchema = do
  let values :: Either Error [Node]
      values = decodeText "[null, ~, '', true, False, 12, -0, 0o17, 0x1f, 1.5, -.inf, .nan, 1e3, +12, .5, a, '1']"
  case values of
    Left err -> assertFailure (show err)
    Right ns -> assertEqual "values"
      [ Null, Null, String "", Bool True, Bool False, Int 12, Int 0, Int 15, Int 31
      , Float 1.5, Float (-1 / 0), Float 0, Float 1000, Int 12, Float 0.5, String "a", String "1"
      ]
      (map (\n -> case n.value of Float d | isNaN d -> Float 0; v -> v) ns)

test_plainSafe :: Assertion
test_plainSafe = do
  assertBool "word with a dash" $ isPlainSafe "dist-newstyle"
  assertBool "colon without a space" $ isPlainSafe "a:b"
  assertBool "flow indicators" $ isPlainSafe "a, [b]"
  assertBool "number" . not $ isPlainSafe "9.10"
  assertBool "boolean" . not $ isPlainSafe "true"
  assertBool "empty" . not $ isPlainSafe ""
  assertBool "colon and a space" . not $ isPlainSafe "a: b"
  assertBool "comment" . not $ isPlainSafe "a #b"
  assertBool "indicator" . not $ isPlainSafe "*a"
  assertBool "line break" . not $ isPlainSafe "a\nb"
  assertBool "string" $ isPlainString "9.10.3"
  assertBool "string with a colon and a space" $ isPlainString "a: b"
  assertBool "string number" . not $ isPlainString "9.10"
  assertBool "string null" . not $ isPlainString "~"

-- | A decimal number resolves to the same double as 'read' gives.
prop_floats :: Property
prop_floats = forAll genDecimal $ \s ->
  resolvePlain (T.pack s) === Float (read s)
  where
    genDecimal :: Gen String
    genDecimal = do
      int <- digits
      frac <- digits
      ex <- oneof [pure "", ("e" ++) . show <$> choose (-30 :: Int, 30)]
      pure $ int ++ "." ++ frac ++ ex

    digits :: Gen String
    digits = do
      k <- choose (1, 20)
      vectorOf k (elements ['0' .. '9'])

data Config = Config
  { name :: T.Text
  , paths :: [FilePath]
  , jobs :: Int
  }
  deriving stock (Eq, Show)

instance FromYAML Config where
  parseYAML = withMapping $ \o -> do
    rejectUnknownKeys ["name", "paths", "jobs"] o
    Config <$> o .: "name" <*> o .:? "paths" .!= [] <*> o .:? "jobs" .!= 1

test_record :: Assertion
test_record = do
  assertEqual "full" (Right (Config "x" ["a", "b"] 4))
    (decodeText "name: x\npaths: [a, b]\njobs: 4\n")
  assertEqual "defaults" (Right (Config "x" [] 1))
    (decodeText "name: x\npaths:\n")

-- | Decoded texts and error lines do not point into the input.
test_copies :: Assertion
test_copies = do
  case decodeText @(M.Map T.Text T.Text) "key: value\nother: text\n" of
    Left err -> assertFailure (show err)
    Right m -> assertBool "texts are copies" $ all isCopy (M.keys m ++ M.elems m)
  case decodeText @Int "a: 1\nb: [\n" of
    Left err -> assertBool "the source line is a copy" $ isCopy err.sourceLine
    Right _ -> assertFailure "expected an error"
  case S.parseDocumentsText "key: &a value\nother: *a\n" of
    Left err -> assertFailure (show err)
    Right docs -> assertBool "syntax texts are copies" . all isCopy $
      concatMap (texts . (.root) . S.copyDocument) docs
  where
    -- A copy starts at the beginning of its own array.
    isCopy :: T.Text -> Bool
    isCopy (T.Text _ off _) = off == 0

    texts :: S.Node -> [T.Text]
    texts = \case
      S.Scalar _ props _ t -> t : maybe [] pure props.anchor
      S.Sequence _ _ _ xs -> concatMap texts xs
      S.Mapping _ _ _ kvs -> concatMap (\(k, v) -> texts k ++ texts v) kvs
      S.Alias _ name -> [name]

test_aliases :: Assertion
test_aliases = assertEqual "map"
  (Right (M.fromList [("a", [1, 2]), ("b", [1, 2 :: Int])]))
  (decodeText @(M.Map T.Text [Int]) "a: &x [1, 2]\nb: *x\n")

test_emptyStream :: Assertion
test_emptyStream = do
  assertEqual "null" (Right Nothing) (decodeText @(Maybe Int) "# nothing\n")
  assertEqual "all" (Right []) (decodeAllText @Int "")

test_encodings :: Assertion
test_encodings = do
  let text = "key: zażółć\n" :: T.Text
  assertEqual "UTF-8 with BOM" (Right text) (decodeInput ("\xEF\xBB\xBF" <> T.encodeUtf8 text) >>= stripBom)
  assertEqual "UTF-16LE" (Right text) (decodeInput (T.encodeUtf16LE text))
  assertEqual "UTF-16BE" (Right text) (decodeInput (T.encodeUtf16BE text))
  assertEqual "UTF-32LE" (Right text) (decodeInput (T.encodeUtf32LE text))
  assertEqual "UTF-32BE" (Right text) (decodeInput (T.encodeUtf32BE text))
  case decodeInput "a: b\n\xFF\n" of
    Left err -> assertEqual "invalid UTF-8" (2, 1) (err.location.line, err.location.column)
    Right _ -> assertFailure "expected an error"
  where
    stripBom :: T.Text -> Either Error T.Text
    stripBom = Right . T.dropWhile (== '\xFEFF')

-- | The line, the column and the message of an error.
errorOf :: Either Error a -> Maybe (Int, Int, String)
errorOf = \case
  Left err -> Just (err.location.line, err.location.column, err.message)
  Right _ -> Nothing

test_syntaxErrors :: Assertion
test_syntaxErrors = do
  let check :: String -> (Int, Int, String) -> T.Text -> Assertion
      check preface expected input = assertEqual preface (Just expected) (errorOf (decodeNodes input))
  check "bad indentation" (3, 2, "unexpected indentation") "a:\n  b: 1\n c: 2\n"
  check "mapping in a plain scalar" (1, 11, "unexpected ':', quote the value if it contains \": \"")
    "key: value: other\n"
  check "missing closing quote" (1, 7, "unterminated double-quoted scalar") "name: \"abc\nnext: value\n"
  check "badly indented quoted line" (2, 1, "invalid indentation of a line in a single-quoted scalar")
    "name: 'abc\nnext'\n"
  check "end of line" (2, 8, "unexpected end of line") "- key: value\n  other\n"
  check "tab indentation" (2, 1, "tabs cannot be used for indentation") "a:\n\tb: 1\n"
  check "unterminated string" (1, 6, "unterminated double-quoted scalar") "key: \"abc\n"
  check "unclosed flow sequence" (1, 11, "expected ',' or ']'") "key: [a, b\nc: d\n"
  check "invalid escape" (1, 8, "invalid escape sequence") "key: \"a\\qb\"\n"
  check "undefined alias" (2, 4, "undefined alias *x") "a: 1\nb: *x\n"
  check "duplicate key" (3, 1, "duplicate key \"a\"") "a: 1\nb: 2\na: 3\n"
  check "undefined tag handle" (1, 1, "undefined tag handle !e!") "!e!foo bar\n"
  check "invalid character" (1, 4, "invalid character") "a: \x01\n"

test_typeErrors :: Assertion
test_typeErrors = do
  assertEqual "list instead of string" (Just (1, 7, "expected a string, but got a list"))
    (errorOf (decodeText @Config "name: [a]\n"))
  assertEqual "number instead of list" (Just (2, 8, "expected a list, but got an integer"))
    (errorOf (decodeText @Config "name: x\npaths: 42\n"))
  assertEqual "element of a list" (Just (2, 12, "expected a string, but got a boolean"))
    (errorOf (decodeText @Config "name: x\npaths: [a, true]\n"))
  assertEqual "out of range" (Just (1, 1, "the integer is out of the range from -128 to 127"))
    (errorOf (decodeText @Int8 "300"))
  assertEqual "custom failure" (Just (1, 5, "not a vowel"))
    (errorOf (decodeText @[Vowel] "[a, x]"))

newtype Vowel = Vowel Char

instance FromYAML Vowel where
  parseYAML = withText $ \t -> case T.unpack t of
    [c] | c `elem` ("aeiou" :: String) -> pure (Vowel c)
    _ -> fail "not a vowel"

test_keyErrors :: Assertion
test_keyErrors = do
  assertEqual "missing key" (Just (1, 1, "missing key \"name\""))
    (errorOf (decodeText @Config "jobs: 1\n"))
  assertEqual "unknown key" (Just (2, 1, "unknown key \"job\", expected one of: name, paths, jobs"))
    (errorOf (decodeText @Config "name: x\njob: 1\n"))

test_prettyError :: Assertion
test_prettyError = case decodeText @Config "name: x\npaths: 42\n" of
  Left err -> assertEqual "rendered" expected (prettyError "config.yaml" err)
  Right _ -> assertFailure "expected an error"
  where
    expected :: String
    expected = L.intercalate "\n"
      [ "config.yaml:2:8: expected a list, but got an integer"
      , "  |"
      , "2 | paths: 42"
      , "  |        ^"
      ]
