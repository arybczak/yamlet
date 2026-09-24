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
    (Right [Float Infinity, Float (Finite 0)])
    (map (.value) <$> decodeText @[Node] "[1e99999999999999999999, 1e-99999999999999999999]")
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
  check "missing closing quote" (1, 7, "unterminated double-quoted scalar") "name: \"abc\nnext: value\n"
  check
    "badly indented quoted line"
    (2, 1, "invalid indentation of a line in a single-quoted scalar")
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
  check "unsupported version" (1, 1, "unsupported YAML version 2.0") "%YAML 2.0\n--- a\n"
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
  check "noncharacter U+FFFE" (1, 4, "invalid character") "a: \xFFFE\n"
  check "noncharacter U+FFFF" (1, 5, "invalid character") "a: b\xFFFF\n"

test_typeErrors :: Assertion
test_typeErrors = do
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
    (Just (2, 1, "unknown key \"job\", expected one of: name, paths, jobs"))
    (errorOf (decodeText @Config "name: x\njob: 1\n"))
  let withKeys :: [T.Text] -> T.Text
      withKeys ks = T.unlines $ map (<> ": 1") ks ++ [T.pack ("k" ++ show i ++ ": 1") | i <- [1 .. 10 :: Int]]
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
  assertEqual "exponent" (Right (Float Infinity)) ((.value) <$> decodeText @Node ("1e" <> nines 1000000))
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
test_prettyError = case decodeText @Config "name: x\npaths: 42\n" of
  Left err -> assertEqual "rendered" expected (prettyError "config.yaml" err)
  Right _ -> assertFailure "expected an error"
  where
    expected :: String
    expected =
      L.intercalate
        "\n"
        [ "config.yaml:2:8: expected a list, but got an integer"
        , "  |"
        , "2 | paths: 42"
        , "  |        ^"
        ]
