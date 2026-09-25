module DecodeTests (decodeTests) where

import Data.ByteString qualified as BS
import Data.Either
import Data.Int
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Scientific qualified as Sci
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
decodeTests =
  testGroup
    "Decode"
    [ testCase "core schema" test_coreSchema
    , testProperty "floats" prop_floats
    , testCase "exact floats" test_exactFloats
    , testCase "plain scalars" test_plainSafe
    , testCase "record" test_record
    , testCase "copies" test_copies
    , testCase "JSON" test_json
    , testCase "aliases" test_aliases
    , localOption (mkTimeout 10000000) $ testCase "nesting" test_nesting
    , localOption (mkTimeout 10000000) $ testCase "many keys" test_manyKeys
    , localOption (mkTimeout 10000000) $ testCase "alias keys" test_aliasKeys
    , localOption (mkTimeout 10000000) $ testCase "long numbers" test_longNumbers
    , testCase "optional keys" test_optionalKeys
    , testCase "syntax tree" test_syntaxTree
    , testCase "empty stream" test_emptyStream
    , testCase "encodings" test_encodings
    , testGroup
        "errors"
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
    Right ns ->
      assertEqual
        "values"
        [ Null
        , Null
        , String ""
        , Bool True
        , Bool False
        , Int 12
        , Int 0
        , Int 15
        , Int 31
        , Float (Finite 1.5)
        , Float NegativeInfinity
        , Float NaN
        , Float (Finite 1000)
        , Int 12
        , Float (Finite 0.5)
        , String "a"
        , String "1"
        ]
        (map (.value) ns)

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

-- | A decimal number resolves to its exact value, and 'withFloat' gives the
-- same double as 'read'.
prop_floats :: Property
prop_floats = forAll genDecimal $ \s ->
  resolvePlain (T.pack s) === Float (Finite (read s))
    .&&. decodeText @Double (T.pack s) === Right (read s)
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

test_exactFloats :: Assertion
test_exactFloats = do
  assertEqual "one tenth" (Right (Sci.scientific 1 (-1))) (decodeText @Sci.Scientific "0.1")
  assertEqual
    "more digits than a double holds"
    (Right (Sci.scientific 12345678901234567890123 (-3)))
    (decodeText @Sci.Scientific "12345678901234567890.123")
  assertEqual "integer as a scientific" (Right (Sci.scientific 42 0)) (decodeText @Sci.Scientific "42")
  assertEqual "huge exponent" (Right (Sci.scientific 1 1000000000)) (decodeText @Sci.Scientific "1e1000000000")
  assertEqual "huge exponent as a double" (Right (1 / 0)) (decodeText @Double "1e1000000000")
  -- 1 + 2^-24 + 2^-60 is nearest to the float 1 + 2^-23, but the nearest
  -- double is 1 + 2^-24, a tie between two floats that rounds to 1.
  assertEqual
    "float without double rounding"
    (Right (1 + 2 ^^ (-23 :: Int)))
    (decodeText @Float "1.000000059604644776257986737988403547205962240695953369140625")
  assertEqual
    "exponent beyond Int"
    (Just (1, 2, "the exponent of the number is out of range"))
    (errorOf (decodeText @Sci.Scientific "[1e99999999999999999999]"))
  assertEqual
    "negative exponent beyond Int"
    (Just (1, 9, "the exponent of the number is out of range"))
    (errorOf (decodeText @Double "!!float 1e-99999999999999999999"))
  assertEqual
    "exponent beyond Int in the schema"
    [Float Infinity, Float (Finite 0), Float (Finite 0)]
    (map resolvePlain ["1e99999999999999999999", "1e-99999999999999999999", "0e99999999999999999999"])
  assertEqual "zero with an exponent beyond Int" (Right 0) (decodeText @Double "0e99999999999999999999")
  assertEqual
    "negative zero"
    (Right [Float NegativeZero, Float NegativeZero, Float (Finite 0), Int 0])
    (map (.value) <$> decodeText @[Node] "[-0.0, !!float -0, 0.0, -0]")
  assertEqual "negative zero as a double" (Right True) (isNegativeZero <$> decodeText @Double "-0.0")
  assertEqual "negative zero as a scientific" (Right 0) (decodeText @Sci.Scientific "-0.0")
  assertEqual
    "negative and positive zero keys"
    (Right [Float (Finite 0), Float NegativeZero])
    ((\n -> case n.value of Mapping kvs -> [k.value | (k, _) <- kvs]; v -> [v]) <$> decodeText @Node "{0.0: a, -0.0: b}")
  assertEqual
    "infinity as a scientific"
    (Just (1, 1, "expected a finite number"))
    (errorOf (decodeText @Sci.Scientific ".inf"))

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
  assertEqual
    "full"
    (Right (Config "x" ["a", "b"] 4))
    (decodeText "name: x\npaths: [a, b]\njobs: 4\n")
  assertEqual
    "defaults"
    (Right (Config "x" [] 1))
    (decodeText "name: x\npaths:\n")
  assertEqual
    "keys of a map that convert to the same key"
    (Just (2, 1, "duplicate key after conversion"))
    (errorOf (decodeText @(M.Map Double Int) "1: 1\n1.0: 2\n"))
  assertEqual
    "string keys with the same text"
    (Just (2, 6, "duplicate key \"name\""))
    (errorOf (decodeText @Config "name: x\n!foo name: y\n"))

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
    Right docs ->
      assertBool "syntax texts are copies" . all isCopy $
        concatMap (texts . (.root) . S.copyDocument) docs
  where
    -- A copy starts at the beginning of its own array.
    isCopy :: T.Text -> Bool
    isCopy (T.Text _ off _) = off == 0

    texts :: S.Node -> [T.Text]
    texts n = case n.content of
      S.Scalar _ t -> t : maybe [] pure n.props.anchor
      S.Sequence _ xs -> concatMap texts xs
      S.Mapping _ kvs -> concatMap (\(k, v) -> texts k ++ texts v) kvs
      S.Alias name -> [name]

-- | JSON is valid YAML, including the escapes that JSON encoders write.
test_json :: Assertion
test_json = do
  assertEqual
    "document"
    (Right (M.fromList [("a", [1.5, -2e3]), ("b\tc", [])]))
    (decodeText @(M.Map T.Text [Double]) "{\"a\":[1.5,-2E3],\n\t\"b\\tc\": []}")
  assertEqual
    "surrogate pair"
    (Right ["\x1F600", "a\x10000z"])
    (decodeText @[T.Text] "[\"\\ud83d\\ude00\", \"a\\uD800\\uDC00z\"]")
  assertEqual
    "lone high surrogate"
    (Just (1, 3, "invalid escape sequence"))
    (errorOf (decodeText @[T.Text] "[\"\\ud83d\"]"))
  assertEqual
    "high surrogate without a low one"
    (Just (1, 3, "invalid escape sequence"))
    (errorOf (decodeText @[T.Text] "[\"\\ud83d\\u0041\"]"))
  assertEqual
    "lone low surrogate"
    (Just (1, 3, "invalid escape sequence"))
    (errorOf (decodeText @[T.Text] "[\"\\ude00\"]"))

test_aliases :: Assertion
test_aliases = do
  assertEqual
    "map"
    (Right (M.fromList [("a", [1, 2]), ("b", [1, 2 :: Int])]))
    (decodeText @(M.Map T.Text [Int]) "a: &x [1, 2]\nb: *x\n")
  assertEqual
    "anchor before a string with a less-than sign"
    (Right (M.fromList [("a", "<x"), ("b", "<x")]))
    (decodeText @(M.Map T.Text T.Text) "a: &x \"<x\"\nb: *x\n")

-- | The time to parse nested flow sequences is linear in the depth.
test_nesting :: Assertion
test_nesting = do
  let nested :: Int -> T.Text -> T.Text
      nested d t = T.replicate d "[" <> t <> T.replicate d "]"
      depth :: Node -> Int
      depth n = case n.value of
        Sequence [x] -> 1 + depth x
        Mapping [(k, _)] -> depth k
        _ -> 0
  assertEqual "sequences" (Right 100000) (depth <$> decodeText (nested 100000 "x"))
  assertEqual "key" (Right 101) (depth <$> decodeText ("[" <> nested 100 "x" <> ": y]"))
  assertBool "key on two lines" (isLeft (decodeText @Node "[[a,\n b]: c]"))
  -- A flow sequence at the start of a line is first tried as a key.
  assertEqual "on two lines" (Right 40) (depth <$> decodeText (nested 40 "x\n"))
  assertEqual
    "block sequences on a long line"
    (Right 40000)
    (depth <$> decodeText (T.replicate 40000 "- " <> T.replicate 1000000 "x"))

test_optionalKeys :: Assertion
test_optionalKeys = do
  let check :: String -> (Maybe (Maybe Int), Maybe (Maybe Int)) -> T.Text -> Assertion
      check preface expected input =
        assertEqual preface (Right (Right expected)) $
          runParser (withMapping $ \o -> (,) <$> o .:? "a" <*> o .:! "a") <$> decodeText input
  check "missing" (Nothing, Nothing) "b: 1\n"
  check "null" (Nothing, Just Nothing) "a: null\n"
  check "value" (Just (Just 1), Just (Just 1)) "a: 1\n"

test_syntaxTree :: Assertion
test_syntaxTree = do
  let input = "# The build.\nname: x\njobs: 4 # At most.\n"
  case S.parseDocumentsText input of
    Right [doc] -> do
      assertEqual "parsed" (Right (Config "x" [] 4)) (decodeDocument input doc)
      let changed = doc {S.root = S.mappingNode [(S.plainNode "name", S.plainNode "y")]}
      assertEqual "changed" (Right (Config "y" [] 1)) (decodeDocument input changed)
    r -> assertFailure (show r)
  case S.parseDocumentsText "name: x\njobs: many\n" of
    Right [doc] ->
      assertEqual
        "type error"
        (Just (2, 7, "expected an integer, but got a string"))
        (errorOf (decodeDocument @Config "name: x\njobs: many\n" doc))
    r -> assertFailure (show r)
  let key = S.plainNode "a"
      built = S.document (S.mappingNode [(key, key), (key, key)])
  assertEqual "built" (Just (1, 1, "duplicate key \"a\"")) (errorOf (resolveDocument "" built))

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
  let invalid :: String -> T.Text -> BS.ByteString -> Assertion
      invalid preface msg bytes =
        assertEqual preface (Just (2, 3, T.unpack msg)) (errorOf (decodeInput bytes))
  invalid "lone surrogate in UTF-16LE" "invalid UTF-16" (T.encodeUtf16LE "a\nbc" <> "\x00\xD8" <> "d\0")
  invalid "odd length of UTF-16BE" "invalid UTF-16" (T.encodeUtf16BE "a\nbc" <> "\0")
  invalid "surrogate in UTF-32BE" "invalid UTF-32" (T.encodeUtf32BE "a\nbc" <> "\0\0\xDC\0")
  invalid "code point beyond Unicode in UTF-32LE" "invalid UTF-32" (T.encodeUtf32LE "a\nbc" <> "\0\0\x11\0")
  let column :: String -> Int -> BS.ByteString -> Assertion
      column preface expected bytes =
        assertEqual preface (Just expected) ((\(_, c, _) -> c) <$> errorOf (decode @Node bytes))
  column "error after a UTF-8 BOM" 1 "\xEF\xBB\xBF]"
  column "error after a UTF-16 BOM" 1 "\xFF\xFE]\0"
  column "invalid UTF-8 after a BOM" 2 "\xEF\xBB\xBF\&b\xFF"
  column "error after a BOM between documents" 1 "a\n...\n\xEF\xBB\xBF]"
  assertEqual
    "source line after a BOM"
    (Left "]")
    (either (Left . (.sourceLine)) (const (Right ())) (decode @Node "\xEF\xBB\xBF]"))
  let documents :: String -> [T.Text] -> T.Text -> Assertion
      documents preface expected input = assertEqual preface (Right expected) (decodeAllText input)
  documents "BOM before a marker after a scalar" ["a", "b"] "a\n\xFEFF--- b\n"
  assertEqual
    "BOM before a marker after a mapping"
    (Right [Mapping [(node (String "a"), node (Int 1))], String "b"])
    (map (strip . (.value)) <$> decodeAllText @Node "a: 1\n\xFEFF--- b\n")
  documents "BOM after an end marker" ["a", "b"] "a\n...\n\xFEFF# c\n\xFEFF\&b\n"
  documents "BOM in a quoted scalar" ["a\xFEFF", "b\xFEFF"] "--- \"a\xFEFF\"\n--- 'b\xFEFF'\n"
  let bom :: String -> (Int, Int) -> T.Text -> Assertion
      bom preface (l, c) input = assertEqual preface (Just (l, c, "unexpected byte order mark")) (errorOf (decodeNodes input))
  bom "BOM at the start of a key" (2, 1) "a: 1\n\xFEFF b: 2\n"
  bom "BOM in a plain scalar" (1, 5) "a: x\xFEFFy\n"
  bom "BOM in a block scalar" (2, 3) "a: |\n  \xFEFFx\n"
  where
    stripBom :: T.Text -> Either Error T.Text
    stripBom = Right . T.dropWhile (== '\xFEFF')

    strip :: Value -> Value
    strip = \case
      Mapping kvs -> Mapping [(Node noOffset k.tag k.value, Node noOffset v.tag v.value) | (k, v) <- kvs]
      v -> v

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
  check
    "mapping in a plain scalar"
    (1, 11, "unexpected ':', quote the value if it contains \": \"")
    "key: value: other\n"
  check
    "anchor on its own line in a sequence"
    (2, 1, "an anchor or a tag cannot be on a line of its own here, write it after the key or the '-'")
    "- item1\n&node\n- item2\n"
  check
    "tag on its own line after a key"
    (2, 1, "an anchor or a tag cannot be on a line of its own here, write it after the key or the '-'")
    "key: &x\n!!map\n  a: b\n"
  check "flow key on two lines" (2, 2, "unexpected ':', a key must be on a single line") "[23\n]: 42\n"
  check "quoted key on two lines" (2, 3, "a key must be on a single line") "a: 1\n\"c\n d\": 1\n"
  check
    "mapping on the line of the document marker"
    (1, 9, "unexpected ':', a mapping cannot start on the line of '---'")
    "--- key1: value1\n    key2: value2\n"
  check
    "key indented under a value"
    (2, 4, "unexpected ':', this line continues the scalar from the line above, check the indentation and the line above")
    "a: 1\n  b: 2\n"
  check "missing closing quote" (1, 7, "unterminated double-quoted scalar") "name: \"abc\nnext: value\n"
  check
    "badly indented quoted line"
    (2, 1, "invalid indentation of a line in a single-quoted scalar")
    "name: 'abc\nnext'\n"
  check "missing colon" (2, 4, "expected ':' after the key") "a: 1\nb 2\nc: 3\n"
  check "missing colon in a list item" (2, 8, "expected ':' after the key") "- key: value\n  other\n"
  check "missing space after a colon" (2, 3, "expected a space after ':'") "a: 1\nb:2\n"
  check "line that has its colon" (2, 5, "expected an alias name after '*'") "a: 1\nb: *\n"
  check "anchor without a name" (1, 5, "expected an anchor name after '&'") "a: & 1\n"
  check "alias with an anchor" (2, 7, "unexpected '*', an alias cannot have an anchor or a tag") "a: &x 1\nb: &y *x\n"
  check "alias with a tag in a flow sequence" (1, 11, "unexpected '*', an alias cannot have an anchor or a tag") "[&x a, !t *x]\n"
  check
    "colon after an alias"
    (2, 3, "the name of the alias includes the ':', write a space before ':' if the alias is a key")
    "a: &x 1\n*x: 2\n"
  check "alias without a name in a flow sequence" (1, 2, "expected an alias name after '*'") "[*, a]\n"
  check "missing space after a dash" (2, 2, "expected a space after '-'") "- a\n-b\n"
  check "tab indentation" (2, 1, "tabs cannot be used for indentation") "a:\n\tb: 1\n"
  check "unterminated string" (1, 6, "unterminated double-quoted scalar") "key: \"abc\n"
  check "flow sequence before a key" (1, 6, "unterminated flow sequence") "key: [a, b\nc: d\n"
  check "flow sequence at the end" (1, 6, "unterminated flow sequence") "key: [a, b\n"
  check "flow sequence before a comment" (1, 6, "unterminated flow sequence") "key: [a, b # c\nd: e\n"
  check
    "block scalar in a flow sequence"
    (1, 2, "unexpected '|', a block scalar cannot be inside a flow collection")
    "[|\n  x\n]\n"
  check "empty flow entry" (1, 4, "unexpected ',', a flow collection cannot have an empty entry") "[1,,2]\n"
  check "content after a flow sequence" (1, 14, "expected ',' or ']'") "key: [a, \"b\" c]\n"
  check "flow mapping at the end" (1, 1, "unterminated flow mapping") "{\"a\": 1,\n \"b\": 2\n"
  check "flow mapping before a marker" (1, 1, "unterminated flow mapping") "{a: 1\n---\nb\n"
  check "missing colon" (1, 6, "expected ':', ',' or '}'") "{\"a\" 1}"
  check "missing comma after a value" (1, 12, "expected ',' or '}'") "{\"a\": 1 \"b\": 2}"
  check
    "quote in a single-quoted scalar"
    (1, 10, "unexpected 's' after a single-quoted scalar, write '' for a quote inside it")
    "msg: 'it's here'\n"
  check
    "quote in a double-quoted scalar"
    (1, 12, "unexpected 'h' after a double-quoted scalar, write \\\" for a quote inside it")
    "msg: \"say \"hi\"\"\n"
  check
    "quote in a quoted scalar in a flow sequence"
    (1, 5, "unexpected 'b' after a double-quoted scalar, write \\\" for a quote inside it")
    "[\"a\"b]\n"
  check "comment after a quote" (1, 7, "unexpected '#', a comment needs a space before it") "a: \"x\"#c\n"
  check "comment after a flow sequence" (1, 7, "unexpected '#', a comment needs a space before it") "a: [1]#c\n"
  check
    "reserved indicator"
    (1, 7, "unexpected '@', a plain scalar cannot start with it, quote the value")
    "user: @admin\n"
  check
    "reserved indicator in a flow sequence"
    (1, 5, "unexpected '`', a plain scalar cannot start with it, quote the value")
    "[a, `b`]\n"
  check "key among list items" (5, 3, "unexpected key among list items") "a:\n  - x\n\n  # c\n  b: 1\n"
  check "list item among keys" (3, 3, "unexpected list item among mapping entries") "a:\n  b: 1\n  - x\n"
  check "list item among top keys" (2, 1, "unexpected list item among mapping entries") "a: 1\n- b\n"
  check "key among top list items" (2, 1, "unexpected key among list items") "- a\nb: 1\n"
  check "brace after a list item" (2, 1, "unexpected '}'") "- a\n}\n"
  check "text after a block scalar header" (1, 6, "the content of a block scalar starts on the next line") "s: | text\n"
  check
    "zero indentation indicator"
    (1, 5, "the indentation indicator of a block scalar must be from 1 to 9")
    "s: |0\n  x\n"
  check
    "long key"
    (1, 1103, "a key can be at most 1024 characters long, write a longer key after '? '")
    ("\"" <> T.replicate 1100 "k" <> "\": 1\n")
  check "list on the line of its key" (1, 4, "unexpected '-', a list cannot start on the line of its key") "a: - b\n"
  check
    "line of a block scalar"
    (3, 3, "unexpected indentation, the line has less indentation than the block scalar above it")
    "s: >- # folded\n    line1\n  line2\n"
  check
    "invalid escape"
    (1, 8, "invalid escape sequence, write \\\\ for a backslash or use single quotes")
    "key: \"a\\qb\"\n"
  check
    "Windows path"
    (1, 10, "invalid escape sequence, write \\\\ for a backslash or use single quotes")
    "path: \"C:\\Users\\me\"\n"
  check "undefined alias" (2, 4, "undefined alias *x") "a: 1\nb: *x\n"
  check "duplicate key" (3, 1, "duplicate key \"a\"") "a: 1\nb: 2\na: 3\n"
  check
    "two merge keys"
    (4, 3, "duplicate key \"<<\", merge keys are not supported")
    "a: &a {x: 1}\nb:\n  <<: *a\n  <<: *a\n"
  check "undefined tag handle" (1, 1, "undefined tag handle !e!") "!e!foo bar\n"
  check "invalid character" (1, 4, "invalid character U+0001") "a: \x01\n"
  check "backslash at the end of the input" (1, 4, "unterminated double-quoted scalar") "a: \"b\\"
  check "backslash at the end of a key" (1, 2, "unterminated double-quoted scalar") "[\"a\\"
  check "unsupported version" (1, 1, "unsupported YAML version 2.0") "%YAML 2.0\n--- a\n"
  check "version without a minor number" (1, 7, "expected a version such as 1.2 after %YAML") "%YAML 1\n--- a\n"
  check "content after the version" (1, 11, "unexpected content after the %YAML version") "%YAML 1.2 x\n--- a\n"
  check
    "tag directive without a prefix"
    (1, 9, "expected a prefix after the tag handle, e.g. tag:example.com,2000:")
    "%TAG !e!\n--- a\n"
  check "invalid tag handle" (1, 6, "invalid tag handle") "%TAG e tag:x,2000:\n--- a\n"
  check "version beyond Int" (1, 1, "unsupported YAML version") "%YAML 18446744073709551617.2\n--- a\n"
  check "minor version beyond Int" (1, 1, "unsupported YAML version") ("%YAML 1." <> T.replicate 100000 "9" <> "\n--- a\n")
  assertEqual
    "version with leading zeros"
    (Right [Just (S.Version 1 2)])
    (map (.version) <$> S.parseDocumentsText "%YAML 001.0002\n--- a\n")
  check "verbatim tag without a name" (1, 1, "invalid verbatim tag") "!<!> a\n"
  check "verbatim tag without a scheme" (1, 1, "invalid verbatim tag") "!<$:?> a\n"
  check "empty verbatim tag" (1, 1, "invalid verbatim tag") "!<> a\n"
  assertEqual
    "valid verbatim tags"
    (Right ["!bar", "tag:yaml.org,2002:str"])
    (map (.tag) <$> decodeText @[Node] "[!<!bar> a, !<tag:yaml.org,2002:str> b]")
  check "noncharacter U+FFFE" (1, 4, "invalid character U+FFFE") "a: \xFFFE\n"
  check "noncharacter U+FFFF" (1, 5, "invalid character U+FFFF") "a: b\xFFFF\n"

newtype IntOrText = IntOrText (Either Integer T.Text)
  deriving stock (Eq, Show)

instance FromYAML IntOrText where
  parseYAML n = IntOrText <$> ((Left <$> withInt pure n) `orElse` (Right <$> withText pure n))

test_typeErrors :: Assertion
test_typeErrors = do
  assertEqual "first alternative" (Right (IntOrText (Left 1))) (decodeText "1")
  assertEqual "second alternative" (Right (IntOrText (Right "a"))) (decodeText "a")
  assertEqual
    "error of the second alternative"
    (Just (1, 1, "expected a string, but got a boolean"))
    (errorOf (decodeText @IntOrText "true"))
  assertEqual
    "pair"
    (Just (1, 1, "expected a list of 2 elements, but got 1"))
    (errorOf (decodeText @(Int, Int) "[1]"))
  assertEqual
    "triple"
    (Just (1, 1, "expected a list of 3 elements, but got 4"))
    (errorOf (decodeText @(Int, Int, Int) "[1, 2, 3, 4]"))
  assertEqual
    "second document"
    (Just (3, 1, "expected a single document, but got a second one"))
    (errorOf (decodeText @T.Text "a\n---\nb\n"))
  assertEqual
    "YAML 1.1 boolean"
    (Just (1, 1, "expected a boolean, but got the string \"yes\", which is a boolean only in YAML 1.1"))
    (errorOf (decodeText @Bool "yes"))
  assertEqual
    "YAML 1.1 boolean with a tag"
    (Just (1, 11, "invalid value for the tag !!bool, \"off\" is a boolean only in YAML 1.1"))
    (errorOf (decodeNodes "a: !!bool off\n"))
  assertEqual
    "list instead of string"
    (Just (1, 7, "expected a string, but got a list"))
    (errorOf (decodeText @Config "name: [a]\n"))
  assertEqual
    "number instead of list"
    (Just (2, 8, "expected a list, but got an integer"))
    (errorOf (decodeText @Config "name: x\npaths: 42\n"))
  assertEqual
    "element of a list"
    (Just (2, 12, "expected a string, but got a boolean"))
    (errorOf (decodeText @Config "name: x\npaths: [a, true]\n"))
  assertEqual
    "out of range"
    (Just (1, 1, "the integer is out of the range from -128 to 127"))
    (errorOf (decodeText @Int8 "300"))
  assertEqual
    "custom failure"
    (Just (1, 5, "not a vowel"))
    (errorOf (decodeText @[Vowel] "[a, x]"))

newtype Vowel = Vowel Char

instance FromYAML Vowel where
  parseYAML = withText $ \t -> case T.unpack t of
    [c] | c `elem` ("aeiou" :: String) -> pure (Vowel c)
    _ -> fail "not a vowel"

test_keyErrors :: Assertion
test_keyErrors = do
  assertEqual
    "missing key"
    (Just (1, 1, "missing key \"name\""))
    (errorOf (decodeText @Config "jobs: 1\n"))
  assertEqual
    "unknown key"
    (Just (2, 1, "unknown key \"other\", expected one of: name, paths, jobs"))
    (errorOf (decodeText @Config "name: x\nother: 1\n"))
  assertEqual
    "key that is not a string"
    (Just (2, 1, "expected a string as the key, but got an integer"))
    (errorOf (decodeText @Config "name: x\n1: y\n"))
  assertEqual
    "unknown key close to a known one"
    (Just (2, 1, "unknown key \"job\", did you mean \"jobs\"?"))
    (errorOf (decodeText @Config "name: x\njob: 1\n"))
  let lookupError :: T.Text -> T.Text -> Maybe String
      lookupError key input = case runParser (withMapping $ \o -> (.:) @T.Text o key) <$> decodeText input of
        Right (Left (_, msg)) -> Just msg
        _ -> Nothing
  assertEqual
    "integer key"
    (Just "the key 404 is an integer, not a string")
    (lookupError "404" "200: ok\n404: not found\n")
  assertEqual
    "boolean key"
    (Just "the key true is a boolean, not a string")
    (lookupError "true" "true: 1\n")
  assertEqual
    "string key that is missing"
    (Just "missing key \"a\"")
    (lookupError "a" "b: 1\n")
  let withKeys :: [T.Text] -> T.Text
      withKeys ks = T.unlines $ map (<> ": 1") ks
  assertEqual
    "duplicate among many scalar keys"
    (Just (21, 1, "duplicate key \"k1\""))
    (errorOf (decodeNodes (T.unlines [T.pack ("k" ++ show i ++ ": 1") | i <- [1 .. 20 :: Int] ++ [1]])))
  assertEqual
    "duplicate scalar key after a collection key"
    (Just (3, 1, "duplicate key \"a\""))
    (errorOf (decodeNodes (withKeys ["a", "[b]", "a"])))
  assertEqual
    "duplicate collection key"
    (Just (2, 1, "duplicate key"))
    (errorOf (decodeNodes (withKeys ["{c: [d]}", "{c: [d]}"])))
  assertEqual
    "duplicate mapping key in another order"
    (Just (2, 1, "duplicate key"))
    (errorOf (decodeNodes (withKeys ["{a: 1, b: 2}", "{b: 2, a: 1}"])))

-- | The check for duplicate keys does not expand the aliases of a key. The
-- alias *a9 expands to 10^10 nodes.
test_aliasKeys :: Assertion
test_aliasKeys = do
  let anchors :: T.Text
      anchors =
        T.unlines $
          "a0: &a0 [x, x, x, x, x, x, x, x, x, x]"
            : [ T.pack ("a" ++ show i ++ ": &a" ++ show i ++ " [" ++ L.intercalate ", " (replicate 10 ("*a" ++ show (i - 1))) ++ "]")
              | i <- [1 .. 9 :: Int]
              ]
      check :: String -> Maybe (Int, Int, String) -> T.Text -> Assertion
      check preface expected keys = assertEqual preface expected (errorOf (decodeNodes (anchors <> keys)))
  check "different keys" Nothing "? *a9\n: 1\n? [*a8, 1]\n: 2\n? [*a8, 2]\n: 3\n"
  check "duplicate key" (Just (13, 3, "duplicate key")) "? [*a9, 1]\n: 1\n? [*a9, 1]\n: 2\n"
  -- Keys from two separate chains of anchors are equal only after an
  -- expansion to 2^40 items.
  let chains :: T.Text -> T.Text -> T.Text
      chains x y =
        T.unlines $
          ["- &a0 [" <> x <> "]", "- &b0 [" <> y <> "]"]
            ++ [ T.pack ("- &" ++ c : show i ++ " [*" ++ c : show (i - 1) ++ ", *" ++ c : show (i - 1) ++ "]")
               | i <- [1 .. 40 :: Int]
               , c <- "ab"
               ]
            ++ ["- ? *a40", "  : 1", "  ? *b40", "  : 2"]
  assertEqual "equal chains" (Just (85, 5, "duplicate key")) (errorOf (decodeNodes (chains "x" "x")))
  assertEqual "different chains" Nothing (errorOf (decodeNodes (chains "x" "y")))

-- | The time to read a number is not quadratic in the number of its digits.
test_longNumbers :: Assertion
test_longNumbers = do
  let nines :: Int -> T.Text
      nines k = T.replicate k "9"
  assertEqual "integer" (Right (10 ^ (1000000 :: Int) - 1)) (decodeText @Integer (nines 1000000))
  assertEqual "hexadecimal" (Right (16 ^ (100 :: Int) - 1)) (decodeText @Integer ("0x" <> T.replicate 100 "f"))
  assertEqual "octal" (Right (8 ^ (100 :: Int) - 1)) (decodeText @Integer ("0o" <> T.replicate 100 "7"))
  assertEqual
    "float"
    (Right (Float (Finite (Sci.scientific (10 ^ (1000000 :: Int) - 1) (-1)))))
    ((.value) <$> decodeText @Node (nines 999999 <> ".9"))
  assertEqual
    "exponent"
    (Just (1, 1, "the exponent of the number is out of range"))
    (errorOf (decodeText @Node ("1e" <> nines 1000000)))
  let zeros = T.replicate 300000 "0"
  assertEqual
    "trailing zeros"
    (Just (1, 600018, "duplicate key"))
    (errorOf (decodeNodes ("{1" <> zeros <> ".0: a, 1" <> zeros <> ".5: b, 1" <> zeros <> ".00: c}")))

-- | The time of the check for duplicate keys is not quadratic in the number
-- of keys.
test_manyKeys :: Assertion
test_manyKeys = do
  let keys :: [T.Text]
      keys = [T.pack ("k" ++ show i) | i <- [1 .. 100000 :: Int]]
      count :: [T.Text] -> Either Error Int
      count ks = length . entries <$> decodeText @Node (T.unlines (map (<> ": 1") ks))
  assertEqual "one collection key" (Right 100001) (count ("[c]" : keys))
  assertEqual "collection keys" (Right 100000) (count (map (\k -> "[" <> k <> "]") keys))
  assertEqual "mapping keys" (Right 100000) (count (map (\k -> "{a: " <> k <> "}") keys))
  let large = "{" <> T.intercalate ", " (map (<> ": 1") keys) <> "}"
  assertEqual
    "large equal keys"
    (Just (3, 3, "duplicate key"))
    (errorOf (decodeNodes ("? " <> large <> "\n: 1\n? " <> large <> "\n: 2\n")))
  let deep = nestedKey 14 "0"
  assertEqual
    "nested equal keys"
    (Just (3, 3, "duplicate key"))
    (errorOf (decodeNodes ("? " <> deep <> "\n: 1\n? " <> deep <> "\n: 2\n")))
  where
    -- Two mappings as keys that differ only in their last value.
    nestedKey :: Int -> T.Text -> T.Text
    nestedKey d v
      | d == 0 = v
      | otherwise = "{" <> nestedKey (d - 1) "0" <> ": 1, " <> nestedKey (d - 1) "1" <> ": " <> v <> "}"

    entries :: Node -> [(Node, Node)]
    entries n = case n.value of
      Mapping kvs -> kvs
      _ -> []

test_prettyError :: Assertion
test_prettyError = do
  case decodeText @Config "name: x\npaths: 42\n" of
    Left err -> assertEqual "rendered" expected (prettyError "config.yaml" err)
    Right _ -> assertFailure "expected an error"
  case decodeNodes ("a: " <> T.replicate 100 "x" <> ": " <> T.replicate 100 "y" <> "\n") of
    Left err -> assertEqual "long line" expectedLong (prettyError "long.yaml" err)
    Right _ -> assertFailure "expected an error"
  where
    expectedLong :: String
    expectedLong =
      L.intercalate
        "\n"
        [ "long.yaml:1:104: unexpected ':', quote the value if it contains \": \""
        , "  |"
        , "1 | ..." ++ replicate 40 'x' ++ ": " ++ replicate 38 'y' ++ "..."
        , "  | " ++ replicate 43 ' ' ++ "^"
        ]

    expected :: String
    expected =
      L.intercalate
        "\n"
        [ "config.yaml:2:8: expected a list, but got an integer"
        , "  |"
        , "2 | paths: 42"
        , "  |        ^"
        ]
