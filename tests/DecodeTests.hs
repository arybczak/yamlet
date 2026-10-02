module DecodeTests (decodeTests) where

import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.Either
import Data.Fixed
import Data.Foldable
import Data.Int
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Ratio
import Data.Scientific qualified as Sci
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Internal qualified as T
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Data.UUID.Types qualified as UUID
import Data.Void
import System.Directory
import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Fixed)

import Thunks
import Yamlet
import Yamlet.Internal.Parser.Monad qualified as P
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
    , testCase "values" test_values
    , testCase "notFollowedBy" test_notFollowedBy
    , testCase "block scalars" test_blockScalars
    , testCase "containers" test_containers
    , localOption (mkTimeout 10000000) $ testCase "time" test_time
    , testCase "copies" test_copies
    , testCase "JSON" test_json
    , testCase "aliases" test_aliases
    , localOption (mkTimeout 10000000) $ testCase "nesting" test_nesting
    , localOption (mkTimeout 10000000) $ testCase "many keys" test_manyKeys
    , localOption (mkTimeout 10000000) $ testCase "alias keys" test_aliasKeys
    , localOption (mkTimeout 10000000) $ testCase "alias limit" test_aliasLimit
    , localOption (mkTimeout 10000000) $ testCase "long numbers" test_longNumbers
    , -- 0.2 s with the check of the lengths, 8 s without it.
      localOption (mkTimeout 2000000) $ testCase "long unknown names" test_longUnknownNames
    , testCase "optional keys" test_optionalKeys
    , testCase "located values" test_located
    , testCase "syntax tree" test_syntaxTree
    , testCase "empty stream" test_emptyStream
    , testCase "encodings" test_encodings
    , testCase "files" test_files
    , testCase "no thunks" test_noThunks
    , testGroup
        "errors"
        [ testCase "syntax" test_syntaxErrors
        , testCase "types" test_typeErrors
        , testCase "keys" test_keyErrors
        , testCase "collected" test_collectedErrors
        , testCase "pretty" test_prettyError
        , testCase "paths" test_errorPaths
        , testProperty "locations of several errors" prop_errorsAt
        , testCase "paths of several errors" test_nodePaths
        , localOption (mkTimeout 10000000) $ testCase "many errors" test_manyErrors
        ]
    ]

-- | The file functions write UTF-8 and read back what they wrote.
test_files :: Assertion
test_files = do
  dir <- getTemporaryDirectory
  (path, h) <- openTempFile dir "yamlet.yaml"
  hClose h
  flip finally (removeFile path) $ do
    let value = M.fromList [("name" :: T.Text, "zażółć" :: T.Text)]
    encodeFile path value
    bytes <- BS.readFile path
    assertEqual "UTF-8" (T.encodeUtf8 "name: zażółć\n") bytes
    decoded <- decodeFile path
    assertEqual "document" (Right value) decoded
    encodeAllFile path [1, 2 :: Int]
    documents <- decodeAllFile path
    assertEqual "documents" (Right [1, 2 :: Int]) documents

-- | The decoders of the types that the library defines return values without
-- thunks.
test_noThunks :: Assertion
test_noThunks = do
  check "value" (decodeText @Value input)
  check "node" (decodeText @S.Node input)
  check "commented values" (decodeText @(M.Map Value (Commented Value)) input)
  check "located values" (decodeText @(M.Map Value (Located Value)) input)
  check "value with its document" (decodeWithDocument @Value input)
  where
    input :: T.Text
    input =
      T.unlines
        [ "# The anchor."
        , "a: &x [1, 2.5, -.inf, \"s\"] # the list"
        , "b: *x"
        , "? [k, 1]"
        , ": {n: null, t: true, !custom tag: !custom v}"
        , "c: |"
        , "  text"
        ]

    check :: String -> Either (NE.NonEmpty Error) a -> Assertion
    check preface = \case
      Right x -> thunks x >>= assertEqual preface []
      Left errs -> assertFailure (preface ++ ": " ++ show errs)

test_coreSchema :: Assertion
test_coreSchema = do
  let values :: Either (NE.NonEmpty Error) [Value]
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
        ns

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
  assertEqual "largest exponent" (Right (Sci.scientific 99 999)) (decodeText @Sci.Scientific "9.9e1000")
  assertEqual "smallest exponent" (Right (Sci.scientific 15 (-1001))) (decodeText @Sci.Scientific "1.5e-1000")
  assertEqual "large exponent as a double" (Right (1 / 0)) (decodeText @Double "1e1000")
  -- 1 + 2^-24 + 2^-60 is nearest to the float 1 + 2^-23, but the nearest
  -- double is 1 + 2^-24, a tie between two floats that rounds to 1.
  assertEqual
    "float without double rounding"
    (Right (1 + 2 ^^ (-23 :: Int)))
    (decodeText @Float "1.000000059604644776257986737988403547205962240695953369140625")
  forM_ ["1e1001", "10e1000", "0.1e-1000", "1" <> T.replicate 1001 "0" <> ".0", "1e99999999999999999999", "11e9223372036854775807"] $ \number ->
    assertEqual
      ("exponent beyond the limit in " ++ show number)
      (Just (1, 2, "the exponent of the number is out of the range from -1000 to 1000, quote the value if it is a string, e.g. '" ++ T.unpack number ++ "'"))
      (errorOf (decodeText @Sci.Scientific ("[" <> number <> "]")))
  assertEqual
    "exponent beyond the limit for a string"
    (Just (1, 9, "the exponent of the number is out of the range from -1000 to 1000, quote the value if it is a string, e.g. '61e9540'"))
    (errorOf (decodeText @(M.Map T.Text T.Text) "gitsha: 61e9540"))
  assertEqual
    "exponent beyond the limit with a tag"
    (Just (1, 9, "the exponent of the number is out of the range from -1000 to 1000"))
    (errorOf (decodeText @Double "!!float 1e-99999999999999999999"))
  assertEqual
    "exponent beyond the limit in the text, value within it"
    (Right (Sci.scientific 1 997))
    (decodeText @Sci.Scientific "0.0001e1001")
  assertEqual
    "exponent beyond the limit in the schema"
    [Float Infinity, Float (Finite 0), Float (Finite 0)]
    (map resolvePlain ["1e1001", "1e-1001", "0e99999999999999999999"])
  assertEqual "zero with an exponent beyond the limit" (Right 0) (decodeText @Double "0e99999999999999999999")
  assertEqual
    "negative zero"
    (Right [Float NegativeZero, Float NegativeZero, Float (Finite 0), Int 0])
    (decodeText @[Value] "[-0.0, !!float -0, 0.0, -0]")
  assertEqual "integer with a float tag" (Right 12) (decodeText @Double "!!float 12")
  forM_ ["0x10", "0o10"] $ \t ->
    assertEqual
      ("integer in another base with a float tag, " ++ show t)
      (Just (1, 9, "invalid value for the tag !!float"))
      (errorOf (decodeText @Double ("!!float " <> t)))
  assertEqual "negative zero as a double" (Right True) (isNegativeZero <$> decodeText @Double "-0.0")
  assertEqual "negative zero as a scientific" (Right 0) (decodeText @Sci.Scientific "-0.0")
  assertEqual
    "negative and positive zero keys"
    (Right [Float (Finite 0), Float NegativeZero])
    ((\case Mapping kvs -> map fst kvs; v -> [v]) <$> decodeText @Value "{0.0: a, -0.0: b}")
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

instance FromYaml Config where
  parseYaml = withMapping $ \o -> do
    rejectUnknownKeys ["name", "paths", "jobs"] o
    Config
      <$> parseField o "name"
      <*> parseFieldDefault o "paths" []
      <*> parseFieldDefault o "jobs" 1

-- | Edge cases of block scalars that the specification leaves unclear.
test_blockScalars :: Assertion
test_blockScalars = do
  -- libyaml and the JavaScript package yaml give the same result.
  assertEqual "indentation indicator at the top level" (Right " a\n") (decodeText @T.Text "--- |1\n  a\n")
  assertEqual "indentation indicator without a marker" (Right " a\n") (decodeText @T.Text "|2\n   a\n")
  -- The end of the input ends a last line of spaces, as in the test JEF9/02
  -- of the YAML test suite.
  assertEqual "keep with spaces at the end" (Right "a\n\n") (decodeText @T.Text "|+\n  a\n  ")
  assertEqual "keep with an empty line and spaces at the end" (Right "a\n\n\n") (decodeText @T.Text "|+\n  a\n\n  ")
  assertEqual "keep with a line break at the end" (Right "a\n\n") (decodeText @T.Text "|+\n  a\n  \n")

-- | An error inside 'P.notFollowedBy' is not lost.
test_notFollowedBy :: Assertion
test_notFollowedBy = do
  let T.Text arr off len = "a"
      e = P.Env {P.array = arr, P.base = off, P.end = off + len, P.streamEnd = off + len, P.handles = M.empty}
  case P.runParser e off (P.notFollowedBy (P.throwAt off "boom")) of
    Left (P.ParseError _ msg) -> assertEqual "message" "boom" msg
    Right _ -> assertFailure "expected an error"

test_values :: Assertion
test_values = do
  assertEqual
    "mapping"
    (Right (Mapping [(String "a", Sequence [Int 1, Int 2])]))
    (decodeText @Value "a: [1, 2]")
  assertEqual
    "tags"
    (Right (Sequence [Tagged "!point" (Mapping [(String "x", Int 1)]), Tagged "!secret" (String "abc"), Int 1]))
    (decodeText @Value "- !point {x: 1}\n- !secret abc\n- !!int 1\n")

test_containers :: Assertion
test_containers = do
  assertEqual "set" (Right (Set.fromList [1, 2, 3])) (decodeText @(Set.Set Int) "[3, 1, 2]")
  assertEqual
    "set with a duplicate after conversion"
    (Just ((1, 5, "duplicate element after conversion"), (1, 2, "the first element")))
    (errorWithNote (decodeText @(Set.Set Double) "[1, 1.0]"))
  assertEqual "int map" (Right (IM.fromList [(1, "a"), (2, "b")])) (decodeText @(IM.IntMap T.Text) "{2: b, 1: a}")
  assertEqual
    "int map with a duplicate key"
    (Just ((1, 8, "duplicate key 0x1, the same value as the first key"), (1, 2, "the first key 1")))
    (errorWithNote (decodeText @(IM.IntMap T.Text) "{1: a, 0x1: b}"))
  assertEqual "int set" (Right (IS.fromList [1, 2, 3])) (decodeText @IS.IntSet "[3, 1, 2]")
  assertEqual
    "int set with a duplicate"
    (Just ((1, 5, "duplicate element"), (1, 2, "the first element")))
    (errorWithNote (decodeText @IS.IntSet "[1, 0x1]"))
  assertEqual "sequence" (Right (Seq.fromList [1, 2])) (decodeText @(Seq.Seq Int) "[1, 2]")
  assertEqual "left" (Right (Left 1)) (decodeText @(Either Int T.Text) "{Left: 1}")
  assertEqual "right" (Right (Right "a")) (decodeText @(Either Int T.Text) "{Right: a}")
  assertEqual
    "either with another key"
    (Just (1, 2, "expected the key Left or Right"))
    (errorOf (decodeText @(Either Int Int) "{Up: 1}"))
  assertEqual
    "either with two keys"
    (Just (1, 1, "expected a mapping with one key, Left or Right"))
    (errorOf (decodeText @(Either Int Int) "{Left: 1, Right: 2}"))
  assertEqual "tuple of 4" (Right (1, 'a', True, "b")) (decodeText @(Int, Char, Bool, T.Text) "[1, a, true, b]")
  assertEqual
    "tuple of 10"
    (Right (1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    (decodeText @(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
  assertEqual
    "tuple of 10 with the wrong size"
    (Just (1, 1, "expected a list of 10 elements, but got 1"))
    (errorOf (decodeText @(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) "[1]"))

test_time :: Assertion
test_time = do
  assertEqual "day" (Right (fromGregorian 2026 9 25)) (decodeText "2026-09-25")
  assertEqual
    "invalid day"
    (Just (1, 1, "expected a date such as 2026-09-25"))
    (errorOf (decodeText @Day "2026-02-30"))
  assertEqual "invalid month" (Just (1, 1, "expected a month such as 2026-09")) (errorOf (decodeText @Month "2026-13"))
  assertEqual "uppercase quarter" (Right (YearQuarter 2026 Q3)) (decodeText "2026-Q3")
  assertEqual "invalid quarter" (Just (1, 1, "expected a quarter such as 2026-q3")) (errorOf (decodeText @Quarter "2026-q5"))
  assertEqual "day of the week in another case" (Right Friday) (decodeText "FriDay")
  assertEqual
    "invalid day of the week"
    (Just (1, 1, "expected a day of the week such as monday"))
    (errorOf (decodeText @DayOfWeek "mon"))
  assertEqual
    "unknown key of calendar days"
    (Just (1, 22, "unknown key \"weeks\", expected one of: months, days"))
    (errorOf (decodeText @CalendarDiffDays "{months: 1, days: 2, weeks: 3}"))
  assertEqual "short year" (Just (1, 1, "expected a date such as 2026-09-25")) (errorOf (decodeText @Day "26-09-25"))
  assertEqual "time without seconds" (Right (TimeOfDay 12 30 0)) (decodeText "12:30")
  assertEqual "time with a fraction" (Right (TimeOfDay 12 30 5.25)) (decodeText "12:30:05.25")
  assertEqual
    "fraction of 13 digits"
    (Just (1, 1, "expected a time such as 12:30:00"))
    (errorOf (decodeText @TimeOfDay "12:30:05.1234567890123"))
  assertEqual "end of a day" (Right (TimeOfDay 24 0 0)) (decodeText "24:00")
  assertEqual "invalid time" (Just (1, 1, "expected a time such as 12:30:00")) (errorOf (decodeText @TimeOfDay "24:01"))
  let noon = LocalTime (fromGregorian 2026 9 25) (TimeOfDay 12 30 0)
  assertEqual "local time with T" (Right noon) (decodeText "2026-09-25T12:30:00")
  assertEqual "local time with a space" (Right noon) (decodeText "2026-09-25 12:30")
  let utcNoon = UTCTime (fromGregorian 2026 9 25) (12 * 3600 + 30 * 60)
  assertEqual "UTC time" (Right utcNoon) (decodeText "2026-09-25T12:30:00Z")
  assertEqual "UTC time from an offset" (Right utcNoon) (decodeText "2026-09-25T14:30:00+02:00")
  assertEqual "offset without a colon" (Right utcNoon) (decodeText "2026-09-25T14:30:00+0200")
  assertEqual
    "space before an offset"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25T14:30:00 +02:00"))
  assertEqual "offset in hours" (Right utcNoon) (decodeText "2026-09-25T10:30:00-02")
  assertEqual
    "lowercase separator"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25t12:30:00Z"))
  assertEqual
    "lowercase zone"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25T12:30:00z"))
  assertEqual "large offset" (Right utcNoon) (decodeText "2026-09-26T12:29:00+23:59")
  assertEqual
    "offset beyond a day"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25T12:30:00+24:00"))
  assertEqual
    "time without a time zone"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25T12:30:00"))
  assertEqual
    "zoned time"
    (Right (noon, 120))
    ((\z -> (zonedTimeToLocalTime z, timeZoneMinutes (zonedTimeZone z))) <$> decodeText "2026-09-25T12:30:00+02:00")
  assertEqual "duration" (Right (1.5 :: NominalDiffTime)) (decodeText "1.5")
  assertEqual "whole duration" (Right (60 :: DiffTime)) (decodeText "60")
  assertEqual "picosecond" (Right (picosecondsToDiffTime 1)) (decodeText "1e-12")
  assertEqual "tiny duration" (Right (0 :: DiffTime)) (decodeText "1e-1000")
  assertEqual "largest duration" (Right (10 ^ (1000 :: Int) :: NominalDiffTime)) (decodeText "1e1000")
  assertEqual
    "integer duration beyond the limit of floats"
    (Right (10 ^ (1001 :: Int) :: NominalDiffTime))
    (decodeText ("1" <> T.replicate 1001 "0"))
  forM_ [minBound, maxBound - 11, maxBound] $ \ex ->
    assertEqual
      ("duration with the exponent " ++ show ex)
      (Left "the exponent of the number is out of the range from -1000 to 1000")
      (first (snd . NE.head) (runParser (parseYaml @NominalDiffTime) (toYaml (Float (Finite (Sci.scientific 1 ex))))))
  assertEqual
    "zero duration with a large exponent"
    (Right (0 :: DiffTime))
    (runParser parseYaml (toYaml (Float (Finite (Sci.scientific 0 maxBound)))))

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
    (Just ((2, 1, "duplicate key after conversion"), (1, 1, "the first key")))
    (errorWithNote (decodeText @(M.Map Double Int) "1: 1\n1.0: 2\n"))
  assertEqual
    "string keys with the same text"
    (Just ((2, 6, "duplicate key \"name\""), (1, 1, "the first key \"name\"")))
    (errorWithNote (decodeText @Config "name: x\n!foo name: y\n"))
  assertEqual
    "several string keys with the same text and a bad field"
    [ (2, 1, "duplicate key \"name\"")
    , (1, 6, "the first key \"name\"")
    , (3, 7, "expected an integer, but got a string")
    , (4, 6, "duplicate key \"jobs\"")
    , (3, 1, "the first key \"jobs\"")
    ]
    (errorsOf (decodeText @Config "!foo name: x\nname: y\njobs: z\n!foo jobs: 4\n"))

-- | Decoded texts and error lines do not point into the input.
test_copies :: Assertion
test_copies = do
  case decodeText @(M.Map T.Text T.Text) "key: value\nother: text\n" of
    Left err -> assertFailure (show err)
    Right m -> assertBool "texts are copies" $ all isCopy (M.keys m ++ M.elems m)
  case decodeText @Int "a: 1\nb: [\n" of
    Left errs -> assertBool "the source line is a copy" $ all (isCopy . (.sourceLine)) errs
    Right _ -> assertFailure "expected an error"
  case S.parseDocumentsText "key: &a value\nother: *a\n" of
    Left err -> assertFailure (show err)
    Right docs ->
      assertBool "syntax texts are copies" $
        all (all isCopy . texts . (.root) . S.copyDocument) docs
  case decodeText @(M.Map T.Text Node) "key: value\nother: [a, &x b] # c\n" of
    Left err -> assertFailure (show err)
    Right m -> assertBool "texts of kept nodes are copies" $ all (all isCopy . texts) (M.elems m)
  case decodeText @Value "a: !x [b, !y c]\n" of
    Left err -> assertFailure (show err)
    Right v -> assertBool "texts of values are copies" $ all isCopy (valueTexts v)
  -- A lazy copy would keep the input alive until the program forces it.
  case decodeText @[T.Text] "- a\n- b\n" of
    Left err -> assertFailure (show err)
    Right xs -> do
      _ <- evaluate (length xs)
      mapM thunks xs >>= assertEqual "items of a list are copies, not thunks" [] . concat
  case S.parseDocumentsText "a: 1\nb: 2\n" of
    Right [doc]
      | Right keys <- runParser (withMapping (pure . objectKeys)) doc.root -> do
          _ <- evaluate (length keys)
          mapM thunks keys >>= assertEqual "keys of an object are copies, not thunks" [] . concat
    _ -> assertFailure "expected the keys of the mapping"
  where
    -- A copy starts at the beginning of its own array.
    isCopy :: T.Text -> Bool
    isCopy (T.Text _ off _) = off == 0

    texts :: S.Node -> [T.Text]
    texts n = case n.content of
      S.ScalarContent _ t -> t : maybe [] pure n.props.anchor
      S.SequenceContent _ xs -> concatMap texts xs
      S.MappingContent _ kvs -> concatMap (\(k, v) -> texts k ++ texts v) kvs
      S.AliasContent name -> [name]

    valueTexts :: Value -> [T.Text]
    valueTexts = \case
      String t -> [t]
      Sequence xs -> concatMap valueTexts xs
      Mapping kvs -> concatMap (\(k, v) -> valueTexts k ++ valueTexts v) kvs
      Tagged tag v -> tag : valueTexts v
      _ -> []

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
  assertEqual
    "anchor inside a node with the same anchor"
    (Right (Sequence [Sequence [Int 1], Int 1]))
    (decodeText @Value "- &a [&a 1]\n- *a\n")
  assertEqual
    "anchor inside a node with the same anchor, typed"
    (Right ([1], 1))
    (decodeText @([Int], Int) "- &a [&a 1]\n- *a\n")
  assertEqual
    "anchor inside a mapping with the same anchor"
    (Right (M.fromList [("x", 1)], 1))
    (decodeText @(M.Map T.Text Int, Int) "- &a {x: &a 1}\n- *a\n")
  assertEqual
    "error inside an alias"
    [(1, 11, "expected an integer, but got a string"), (2, 4, "expected an integer, but got a string"), (3, 4, "expected an integer, but got a string")]
    (errorsOf (decodeText @(M.Map T.Text [Int]) "a: &x [1, x]\nb: *x\nc: *x\n"))
  assertEqual
    "path of an error inside an alias"
    (Left [(2, 3, [Index 1])])
    ( first
        (map (\err -> (err.location.line, err.location.column, err.path)) . NE.toList)
        (decodeText @([T.Text], [Int]) "- &x [a, b]\n- *x\n")
    )

-- | The time to parse nested flow sequences is linear in the depth.
test_nesting :: Assertion
test_nesting = do
  let nested :: Int -> T.Text -> T.Text
      nested d t = T.replicate d "[" <> t <> T.replicate d "]"
      depth :: Value -> Int
      depth = \case
        Sequence [x] -> 1 + depth x
        Mapping [(k, _)] -> depth k
        _ -> 0
  assertEqual "sequences" (Right 100000) (depth <$> decodeText (nested 100000 "x"))
  assertEqual "key" (Right 101) (depth <$> decodeText ("[" <> nested 100 "x" <> ": y]"))
  assertBool "key on two lines" (isLeft (decodeText @Value "[[a,\n b]: c]"))
  -- A flow sequence at the start of a line is first tried as a key.
  assertEqual "on two lines" (Right 40) (depth <$> decodeText (nested 40 "x\n"))
  assertEqual
    "block sequences on a long line"
    (Right 40000)
    (depth <$> decodeText (T.replicate 40000 "- " <> T.replicate 1000000 "x"))
  assertEqual
    "block sequences on a line with a comment below"
    (Right 200000)
    (depth <$> decodeText (T.replicate 200000 "- " <> "x\n\n# c\n"))
  assertEqual
    "block sequences on an indented line with a comment above"
    (Right 400000)
    (depth <$> decodeText ("# c\n" <> T.replicate 400000 " " <> T.replicate 400000 "- " <> "x\n"))

test_optionalKeys :: Assertion
test_optionalKeys = do
  let check :: String -> (Maybe (Maybe Int), Maybe (Maybe Int)) -> T.Text -> Assertion
      check preface expected input =
        assertEqual preface (Right (Right expected)) $
          runParser
            ( withMapping $ \o ->
                (,)
                  <$> parseFieldMaybe o "a"
                  <*> parseFieldIfPresent o "a"
            )
            <$> decodeText input
  check "missing" (Nothing, Nothing) "b: 1\n"
  check "null" (Nothing, Just Nothing) "a: null\n"
  check "value" (Just (Just 1), Just (Just 1)) "a: 1\n"
  let explicit :: String -> Either (NE.NonEmpty (Offset, String)) (Int, Maybe Int, Maybe (Maybe Int)) -> T.Text -> Assertion
      explicit preface expected input =
        assertEqual preface (Right expected) $
          runParser
            ( withMapping $ \o ->
                (,,)
                  <$> parseFieldWith small o "a"
                  <*> parseFieldMaybeWith small o "b"
                  <*> parseFieldIfPresentWith (parseYaml @(Maybe Int)) o "b"
            )
            <$> decodeText input
      small :: Node -> Parser Int
      small = withInt $ \i -> if i < 10 then pure (fromInteger i) else fail "too large"
  explicit "explicit, missing" (Right (1, Nothing, Nothing)) "a: 1\n"
  explicit "explicit, null" (Right (1, Nothing, Just Nothing)) "a: 1\nb: null\n"
  explicit "explicit, value" (Right (1, Just 2, Just (Just 2))) "a: 1\nb: 2\n"
  explicit "explicit, missing key" (Left (pure (Offset 0, "missing key \"a\""))) "b: 1\n"
  explicit "explicit, bad value" (Left (pure (Offset 3, "too large"))) "a: 20\n"
  let keyError :: (Object -> T.Text -> Parser (Maybe Int)) -> Either (NE.NonEmpty (Offset, String)) (Maybe Int)
      keyError op = either (error . show) (runParser (withMapping (`op` "404"))) (decodeText "200: 1\n404: 2\n")
      integerKey :: Either (NE.NonEmpty (Offset, String)) (Maybe Int)
      integerKey = Left (pure (Offset 7, "the key 404 is an integer, not a string"))
  assertEqual "optional integer key" integerKey (keyError parseFieldMaybe)
  assertEqual "optional integer key, null as a value" integerKey (keyError parseFieldIfPresent)
  assertEqual "explicit optional integer key" integerKey (keyError (parseFieldMaybeWith parseYaml))
  assertEqual "explicit optional integer key, null as a value" integerKey (keyError (parseFieldIfPresentWith parseYaml))

-- | A located value keeps the offset of its node, and the errors at its offset
-- have lines, columns and paths.
test_located :: Assertion
test_located = do
  assertEqual "items" (Right [Located "a" (Offset 1), Located "b" (Offset 4)]) (decodeText @[Located T.Text] "[a, b]")
  let input = "skip:\n  - x\n  - y\n"
  case decodeWithDocument @(M.Map T.Text [Located T.Text]) input of
    Right (m, doc) -> do
      let errs = [(item.offset, "unknown package " ++ show item.value) | item <- M.findWithDefault [] "skip" m, item.value == "y"]
      assertEqual
        "error at a located value"
        [(3, 5, "skip[1]", "unknown package \"y\"")]
        [(e.location.line, e.location.column, renderPath e.path, e.message) | e <- documentErrors input doc errs]
      assertEqual
        "error without an offset"
        ["conf.yml: not from the input"]
        (map (prettyError "conf.yml") (documentErrors input doc [(noOffset, "not from the input")]))
    Left errs -> assertFailure (show errs)
  assertEqual
    "second document"
    (errorOf (decodeText @Int "1\n--- 2\n"))
    (errorOf (decodeWithDocument @Int "1\n--- 2\n"))
  assertEqual
    "empty stream"
    (Right (Nothing, S.document (S.Node (Offset 0) (Offset 0) S.noProps S.noComments (S.ScalarContent S.Plain ""))))
    (decodeWithDocument @(Maybe Int) "")
  assertEqual
    "comments of the key"
    (Right (Just (Offset 3, Just "c")))
    (fmap (\l -> (l.offset, l.value.comments.inline)) . M.lookup "a" <$> decodeText @(M.Map T.Text (Located (Commented T.Text))) "a: x # c\n")
  assertEqual "encoded" "a: 1\n" (encodeText (M.fromList [("a" :: T.Text, Located (1 :: Int) (Offset 7))]))

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
  assertEqual
    "built"
    (Just ((0, 0, "duplicate key \"a\""), (0, 0, "the first key \"a\"")))
    (errorWithNote (decodeDocument @Value "" built))
  assertEqual
    "built, rendered"
    (Left ["built.yaml: duplicate key \"a\"", "built.yaml: the first key \"a\""])
    (either (Left . map (prettyError "built.yaml") . NE.toList) (const (Right ())) (decodeDocument @Value "" built))
  assertEqual
    "decoder error in a built node"
    (Just (0, 0, "expected an integer, but got a string"))
    (errorOf (decodeDocument @Int "" (S.document (S.plainNode "x"))))

test_emptyStream :: Assertion
test_emptyStream = do
  assertEqual "null" (Right Nothing) (decodeText @(Maybe Int) "# nothing\n")
  assertEqual "all" (Right []) (decodeAllText @Int "")

test_encodings :: Assertion
test_encodings = do
  let text = "key: zażółć \x1F600\n" :: T.Text
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
        assertEqual preface (Just (2, 3, T.unpack msg)) (errorOf (first pure (decodeInput bytes)))
  invalid "lone surrogate in UTF-16LE" "invalid UTF-16" (T.encodeUtf16LE "a\nbc" <> "\x00\xD8" <> "d\0")
  invalid "odd length of UTF-16BE" "invalid UTF-16" (T.encodeUtf16BE "a\nbc" <> "\0")
  invalid "surrogate in UTF-32BE" "invalid UTF-32" (T.encodeUtf32BE "a\nbc" <> "\0\0\xDC\0")
  invalid "code point beyond Unicode in UTF-32LE" "invalid UTF-32" (T.encodeUtf32LE "a\nbc" <> "\0\0\x11\0")
  invalid "incomplete character in UTF-8" "invalid UTF-8" ("a\nbc" <> "\xE2\x82")
  invalid "surrogate in UTF-8" "invalid UTF-8" ("a\nbc" <> "\xED\xA0\x80" <> "d")
  let column :: String -> Int -> BS.ByteString -> Assertion
      column preface expected bytes =
        assertEqual preface (Just expected) ((\(_, c, _) -> c) <$> errorOf (decode @Value bytes))
  column "error after a UTF-8 BOM" 1 "\xEF\xBB\xBF]"
  column "error after a UTF-16 BOM" 1 "\xFF\xFE]\0"
  column "invalid UTF-8 after a BOM" 2 "\xEF\xBB\xBF\&b\xFF"
  column "error after a BOM between documents" 1 "a\n...\n\xEF\xBB\xBF]"
  column "error after two BOMs" 1 "\xEF\xBB\xBF\xEF\xBB\xBF]"
  column "error after two BOMs before a marker" 5 "a\n\xEF\xBB\xBF\xEF\xBB\xBF--- ]"
  let errorAfterBom :: String -> (Int, Int, String) -> T.Text -> Assertion
      errorAfterBom preface expected input = assertEqual preface (Just expected) (errorOf (decodeAllText @Value input))
  errorAfterBom "error after a BOM after an end marker" (3, 5, "unexpected ':', quote the value if it contains \": \"") "a\n...\n\xFEFF\&b: x: y\n"
  errorAfterBom "error after a BOM and a comment after an end marker" (4, 4, "unterminated flow sequence") "a\n...\n\xFEFF# c\n\xFEFF\&b: [\n"
  errorAfterBom "error after a second BOM at the start" (1, 4, "unterminated flow sequence") "\xFEFF\xFEFF\&a: [\n"
  assertEqual
    "source line after a BOM"
    (Left "]")
    (either (Left . (.sourceLine) . NE.head) (const (Right ())) (decode @Value "\xEF\xBB\xBF]"))
  assertEqual
    "source line at the line feed of a CRLF"
    "a: 1"
    (errorAt "a: 1\r\nb: 2\n" (Offset 5) "message").sourceLine
  let documents :: String -> [T.Text] -> T.Text -> Assertion
      documents preface expected input = assertEqual preface (Right expected) (decodeAllText input)
  documents "BOM before a marker after a scalar" ["a", "b"] "a\n\xFEFF--- b\n"
  assertEqual
    "BOM before a marker after a mapping"
    (Right [Mapping [(String "a", Int 1)], String "b"])
    (decodeAllText @Value "a: 1\n\xFEFF--- b\n")
  documents "BOM after an end marker" ["a", "b"] "a\n...\n\xFEFF# c\n\xFEFF\&b\n"
  documents "BOM in a quoted scalar" ["a\xFEFF", "b\xFEFF"] "--- \"a\xFEFF\"\n--- 'b\xFEFF'\n"
  let bom :: String -> (Int, Int) -> T.Text -> Assertion
      bom preface (l, c) input = assertEqual preface (Just (l, c, "unexpected byte order mark")) (errorOf (decodeAllText @Value input))
  bom "BOM at the start of a key" (2, 1) "a: 1\n\xFEFF b: 2\n"
  bom "BOM in a plain scalar" (1, 5) "a: x\xFEFFy\n"
  bom "BOM in a block scalar" (2, 3) "a: |\n  \xFEFFx\n"
  bom "BOM before a key" (2, 1) "a: b\n\xFEFF\&c: d\n"
  bom "BOM before a list item" (2, 1) "- a\n\xFEFF- b\n"
  bom "BOM before an indented value" (2, 1) "a:\n\xFEFF  b\n"
  assertEqual
    "BOM before a comment after a mapping"
    (Right [Mapping [(String "a", String "b")]])
    (decodeAllText @Value "a: b\n\xFEFF#c\n")
  documents "BOM before a comment after a scalar" ["a"] "a\n\xFEFF# c\n"
  documents "BOM at the end after a scalar" ["a"] "a\n\xFEFF"
  assertEqual "BOM at the end after a marker" (Right [Null]) (decodeAllText @Value "---\n\xFEFF")
  bom "BOM before a scalar after a scalar" (2, 1) "a\n\xFEFF\&b\n"
  bom "BOM in a flow sequence" (2, 1) "a: [x,\n\xFEFF y]\n"
  bom "BOM in a flow mapping" (2, 1) "a: {x: 1,\n\xFEFF\&y: 2}\n"
  bom "BOM before a closing bracket" (2, 1) "a: [x,\n\xFEFF]\n"
  assertEqual
    "BOM before a marker after an unterminated flow sequence"
    (Just (1, 4, "unterminated flow sequence"))
    (errorOf (decodeAllText @Value "a: [x,\n\xFEFF---\nb\n"))
  documents "BOM before a start marker in a double-quoted scalar" ["a \xFEFF--- "] "\"a\n\xFEFF---\n\"\n"
  documents "BOM before an end marker in a single-quoted scalar" ["a \xFEFF... b"] "'a\n\xFEFF... b'\n"
  documents "two BOMs before a marker" ["a", "b"] "a\n\xFEFF\xFEFF--- b\n"
  documents "two BOMs before a marker after an end marker" ["a", "b"] "--- a\n...\n\xFEFF\xFEFF--- b\n"
  documents "two BOMs before a marker after a block scalar" ["x\n", "b"] "--- |\n x\n\xFEFF\xFEFF--- b\n"
  -- The time to check a run of BOMs is linear in its length.
  documents "many BOMs at the start" ["a"] (T.replicate 400000 "\xFEFF" <> "a\n")
  documents "many BOMs after an end marker" ["a", "b"] ("a\n...\n" <> T.replicate 400000 "\xFEFF" <> "b\n")
  where
    stripBom :: T.Text -> Either Error T.Text
    stripBom = Right . T.dropWhile (== '\xFEFF')

-- | The line, the column and the message of the only error.
errorOf :: Either (NE.NonEmpty Error) a -> Maybe (Int, Int, String)
errorOf = \case
  Left (err NE.:| []) -> Just (err.location.line, err.location.column, err.message)
  Left errs -> error $ "expected one error, but got " ++ show (map (.message) (NE.toList errs))
  Right _ -> Nothing

-- | The line, the column and the message of the only error and of its note.
errorWithNote :: Either (NE.NonEmpty Error) a -> Maybe ((Int, Int, String), (Int, Int, String))
errorWithNote = \case
  Left (err NE.:| [note]) -> Just (place err, place note)
  Left errs -> error $ "expected an error and a note, but got " ++ show (map (.message) (NE.toList errs))
  Right _ -> Nothing
  where
    place :: Error -> (Int, Int, String)
    place e = (e.location.line, e.location.column, e.message)

-- | The line, the column and the message of each error.
errorsOf :: Either (NE.NonEmpty Error) a -> [(Int, Int, String)]
errorsOf = \case
  Left errs -> [(err.location.line, err.location.column, err.message) | err <- NE.toList errs]
  Right _ -> []

test_syntaxErrors :: Assertion
test_syntaxErrors = do
  let check :: String -> (Int, Int, String) -> T.Text -> Assertion
      check preface expected input = assertEqual preface (Just expected) (errorOf (decodeAllText @Value input))
  check "bad indentation" (3, 2, "unexpected indentation") "a:\n  b: 1\n c: 2\n"
  -- The BOM at the start of the input is not content of the first line.
  forM_
    [ ("colon in an alias", (1, 3, "the name of the alias includes the ':', write a space before ':' if the alias is a key"), "*x: 1")
    , ("properties on their own line", (1, 1, "an anchor or a tag cannot be on a line of its own here, write it after the key or the '-'"), "&a &b")
    , ("tab before a key", (1, 1, "tabs cannot be used for indentation"), "\tx: y")
    , ("mapping on the start marker line", (1, 6, "unexpected ':', a mapping cannot start on the line of '---'"), "--- a: b")
    , ("key among list items", (2, 1, "unexpected key among list items"), "- a\nb: 1")
    , ("list item without a space", (2, 2, "expected a space after '-'"), "- a\n-b")
    ]
    $ \(preface, expected, input) -> do
      check preface expected input
      check (preface ++ " after a BOM") expected ("\xFEFF" <> input)
  check
    "line after a comment below a plain scalar"
    (3, 3, "a comment ends a plain scalar, so this line cannot continue it")
    "a: x\n# c\n  y\n"
  forM_
    [ ("literal scalar", 4, "a: |\n  x\n# c\n  y\n")
    , ("single-quoted scalar", 3, "a: 'x'\n# c\n  y\n")
    , ("flow sequence", 3, "a: [x]\n# c\n  y\n")
    , ("alias", 3, "a: *x\n# c\n  y\n")
    ]
    $ \(node, line, input) ->
      check ("line after a comment below a " ++ node) (line, 3, "unexpected indentation") input
  forM_
    [ ("list item", "- # c\nfoo\n")
    , ("list item with an anchor", "- &x # c\nfoo\n")
    , ("list item with a tag", "- !t # c\nfoo\n")
    ]
    $ \(node, input) ->
      check ("line after a comment on an empty " ++ node) (2, 1, "unexpected key among list items") input
  check
    "line after a comment below the header of a block scalar"
    (3, 3, "unexpected indentation, the line has less indentation than the block scalar above it")
    "a: |\n# c\n  y\n"
  check
    "mapping in a plain scalar"
    (1, 11, "unexpected ':', quote the value if it contains \": \"")
    "key: value: other\n"
  check
    "content after a quoted value"
    (1, 14, "unexpected 't' after the end of a quoted scalar")
    "key: \"value\" trailing\n"
  check "content after a flow value" (1, 12, "unexpected 'i' after the end of a flow collection") "x: { y: z }in: valid\n"
  check "letter beyond ASCII" (1, 4, "unexpected 'é' after the end of a flow collection") "[a]é\n"
  check "character that cannot be shown" (1, 4, "unexpected U+200B after the end of a flow collection") "[a]\x200B\n"
  check
    "comment line in a plain scalar"
    (3, 3, "a comment ends a plain scalar, so this line cannot continue it")
    "key: word1\n#  xxx\n  word2\n"
  check
    "comment at the end of a line of a plain scalar"
    (2, 1, "a comment ends a plain scalar, so this line cannot continue it")
    "word1  # comment\nword2\n"
  check
    "directive after a comment"
    (3, 1, "unexpected '%', a plain scalar cannot start with it, quote the value")
    "---\nscalar1 # comment\n%YAML 1.2\n---\nscalar2\n"
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
  check "tab before a key" (1, 1, "tabs cannot be used for indentation") "\tkey: value\n"
  check "tab after spaces before a key" (2, 3, "tabs cannot be used for indentation") "a:\n  \tb: c\n"
  check "unterminated string" (1, 6, "unterminated double-quoted scalar") "key: \"abc\n"
  check "flow sequence before a key" (1, 6, "unterminated flow sequence") "key: [a, b\nc: d\n"
  check "flow sequence at the end" (1, 6, "unterminated flow sequence") "key: [a, b\n"
  check "flow sequence before a comment" (1, 6, "unterminated flow sequence") "key: [a, b # c\nd: e\n"
  check
    "closing bracket indented too little"
    (4, 1, "']' is indented too little to end the flow sequence")
    "key: [\n  a,\n  b\n]\n"
  check
    "closing brace after a comment line"
    (3, 1, "'}' is indented too little to end the flow mapping")
    "key: {\n  # c\n}\n"
  check "tab in a flow sequence" (2, 1, "tabs cannot be used for indentation") "a: [\n\tb\n]\n"
  check
    "block scalar in a flow sequence"
    (1, 2, "unexpected '|', a block scalar cannot be inside a flow collection")
    "[|\n  x\n]\n"
  check
    "block scalar indicator after a quoted scalar"
    (1, 8, "unexpected '|' after the end of a quoted scalar")
    "a: 'x' |\n"
  check "block scalar indicator after a flow collection" (1, 8, "unexpected '|' after the end of a flow collection") "a: [x] |\n"
  check "block scalar indicator at the start of a line" (2, 1, "unexpected '|'") "a: b\n| x\n"
  check
    "dash in a flow sequence"
    (1, 2, "unexpected '-', a list item cannot be inside a flow collection, quote '-' if it is a string")
    "[-]\n"
  check "empty flow entry" (1, 4, "unexpected ',', a flow collection cannot have an empty entry") "[1,,2]\n"
  check "content after a flow sequence" (1, 14, "expected ',' or ']'") "key: [a, \"b\" c]\n"
  check "flow mapping at the end" (1, 1, "unterminated flow mapping") "{\"a\": 1,\n \"b\": 2\n"
  check "flow mapping before a marker" (1, 1, "unterminated flow mapping") "{a: 1\n---\nb\n"
  check "start marker in a double-quoted scalar" (2, 1, "unexpected '---' in a double-quoted scalar, indent the line") "a: \"x\n---\n  y\"\n"
  check "end marker in a single-quoted scalar" (2, 1, "unexpected '...' in a single-quoted scalar, indent the line") "a: 'x\n...\n  y'\n"
  check "start marker in a flow sequence" (2, 1, "unexpected '---' in a flow sequence, indent the line") "a: [x,\n---\n  y]\n"
  check "end marker in a flow mapping" (2, 1, "unexpected '...' in a flow mapping, indent the line") "a: {x: 1,\n...\n  y: 2}\n"
  check "start marker after a missing quote" (1, 4, "unterminated double-quoted scalar") "a: \"x\n---\nb: c\n"
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
  check
    "spaces after a key count toward its length"
    (1, 1026, "a key can be at most 1024 characters long, write a longer key after '? '")
    ("a" <> T.replicate 1024 " " <> ": 1\n")
  check
    "spaces after a key in a flow sequence"
    (1, 1029, "expected ',' or ']'")
    ("['a'" <> T.replicate 1024 " " <> ": 1]\n")
  check
    "list on the line of its anchor"
    (1, 9, "unexpected '-', a list cannot start on the line of its anchor or tag")
    "&anchor - sequence entry\n"
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
  assertEqual
    "duplicate key"
    (Just ((3, 1, "duplicate key \"a\""), (1, 1, "the first key \"a\"")))
    (errorWithNote (decodeAllText @Value "a: 1\nb: 2\na: 3\n"))
  assertEqual
    "duplicate key with another text"
    (Just ((2, 1, "duplicate key ~, the same value as the first key"), (1, 1, "the first key null")))
    (errorWithNote (decodeAllText @Value "null: 1\n~: 2\n"))
  assertEqual
    "two merge keys"
    (Just ((4, 3, "duplicate key \"<<\", merge keys are not supported"), (3, 3, "the first key \"<<\"")))
    (errorWithNote (decodeAllText @Value "a: &a {x: 1}\nb:\n  <<: *a\n  <<: *a\n"))
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
  check "version beyond the limit" (1, 1, "unsupported YAML version") "%YAML 1000001.2\n--- a\n"
  check "minor version beyond the limit" (1, 1, "unsupported YAML version") "%YAML 1.1000001\n--- a\n"
  check "version beyond Int" (1, 1, "unsupported YAML version") "%YAML 18446744073709551617.2\n--- a\n"
  check "minor version beyond Int" (1, 1, "unsupported YAML version") ("%YAML 1." <> T.replicate 100000 "9" <> "\n--- a\n")
  assertEqual
    "minor version at the limit"
    (Right [Just (S.YamlVersion 1 1000000)])
    (map (.version) <$> S.parseDocumentsText "%YAML 1.1000000\n--- a\n")
  assertEqual
    "version with leading zeros"
    (Right [Just (S.YamlVersion 1 2)])
    (map (.version) <$> S.parseDocumentsText "%YAML 001.0002\n--- a\n")
  check "verbatim tag without a name" (1, 1, "invalid verbatim tag") "!<!> a\n"
  check "verbatim tag without a scheme" (1, 1, "invalid verbatim tag") "!<$:?> a\n"
  check "empty verbatim tag" (1, 1, "invalid verbatim tag") "!<> a\n"
  let badEscape = "invalid escape in the tag, write '%' and two hexadecimal digits"
  check "escape without digits in a tag" (1, 6, badEscape) "x: !a%zz b\n"
  check "escape with one digit in a tag" (1, 6, badEscape) "x: !a%4 b\n"
  check "escape without digits in a tag prefix" (1, 8, badEscape) "%TAG ! %\xE9\n--- a\n"
  check "secondary handle without a suffix" (1, 6, "expected the rest of the tag after !!") "a: !! z\n"
  check "invalid UTF-8 in a tag" (1, 1, "the escapes of the tag are not valid UTF-8") "!!str%FF a\n"
  assertEqual
    "character from the escapes of the prefix and the suffix"
    (Right ["tag:\xE9"])
    (map (\d -> case d.root.props.tag of S.Tag t -> t; _ -> "") <$> S.parseDocumentsText "%TAG !e! tag:%C3\n--- !e!%A9 a\n")
  assertEqual
    "valid verbatim tags"
    (Right ["!bar", "tag:yaml.org,2002:str"])
    (map valueTag <$> decodeText @[Value] "[!<!bar> a, !<tag:yaml.org,2002:str> b]")
  check "noncharacter U+FFFE" (1, 4, "invalid character U+FFFE") "a: \xFFFE\n"
  check "noncharacter U+FFFF" (1, 5, "invalid character U+FFFF") "a: b\xFFFF\n"

newtype IntOrText = IntOrText (Either Integer T.Text)
  deriving stock (Eq, Show)

instance FromYaml IntOrText where
  parseYaml n = IntOrText <$> ((Left <$> withInt pure n) `orElse` (Right <$> withText pure n))

newtype Size = Size Int
  deriving stock (Eq, Show)

instance FromYaml Size where
  parseYaml = oneOf [("small", Size 1), ("large", Size 2), ("10", Size 10)]

-- | A resolution of 1/40, which needs three places after the point.
data Fortieths

instance HasResolution Fortieths where
  resolution _ = 40

-- | A resolution of 1/3, which has no exact decimal form.
data Thirds

instance HasResolution Thirds where
  resolution _ = 3

test_typeErrors :: Assertion
test_typeErrors = do
  assertEqual "first alternative" (Right (IntOrText (Left 1))) (decodeText "1")
  assertEqual "second alternative" (Right (IntOrText (Right "a"))) (decodeText "a")
  assertEqual
    "error of the second alternative"
    (Just (1, 1, "expected a string, but got a boolean, quote the value, e.g. 'true'"))
    (errorOf (decodeText @IntOrText "true"))
  assertEqual "known name" (Right (Size 2)) (decodeText "large")
  assertEqual
    "close name"
    (Just (1, 1, "unknown value \"lage\", did you mean \"large\"?"))
    (errorOf (decodeText @Size "lage"))
  assertEqual
    "other name"
    (Just (1, 1, "unknown value \"medium\", expected one of: small, large, 10"))
    (errorOf (decodeText @Size "medium"))
  assertEqual
    "plain name that is not a string"
    (Just (1, 1, "expected a string, but got an integer, quote the value, e.g. '10'"))
    (errorOf (decodeText @Size "10"))
  assertEqual
    "collection"
    (Just (1, 1, "expected one of: small, large, 10, but got a list"))
    (errorOf (decodeText @Size "[small]"))
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
    (Just (1, 1, "expected a boolean, but got the string \"yes\", which is a boolean only in YAML 1.1, use true or false"))
    (errorOf (decodeText @Bool "yes"))
  assertEqual
    "quoted YAML 1.1 boolean"
    (Just (1, 1, "expected a boolean, but got a string"))
    (errorOf (decodeText @Bool "'yes'"))
  assertEqual
    "YAML 1.1 boolean with a string tag"
    (Just (1, 7, "expected a boolean, but got a string"))
    (errorOf (decodeText @Bool "!!str yes"))
  assertEqual
    "YAML 1.1 boolean with a tag"
    (Just (1, 11, "invalid value for the tag !!bool, \"off\" is a boolean only in YAML 1.1"))
    (errorOf (decodeAllText @Value "a: !!bool off\n"))
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
    (Just (2, 12, "expected a string, but got a boolean, quote the value, e.g. 'true'"))
    (errorOf (decodeText @Config "name: x\npaths: [a, true]\n"))
  assertEqual
    "float instead of string"
    (Just (1, 1, "expected a string, but got a floating-point number, quote the value, e.g. '9.10'"))
    (errorOf (decodeText @T.Text "9.10"))
  assertEqual
    "integer instead of string"
    (Just (1, 1, "expected a string, but got an integer, quote the value, e.g. '007'"))
    (errorOf (decodeText @T.Text "007"))
  assertEqual "empty value instead of string" (Just (1, 6, "expected a string, but got null")) (errorOf (decodeText @Config "name:\n"))
  assertEqual
    "null instead of string"
    (Just (1, 1, "expected a string, but got null, quote the value, e.g. 'null'"))
    (errorOf (decodeText @T.Text "null"))
  assertEqual
    "tilde instead of string"
    (Just (1, 1, "expected a string, but got null, quote the value, e.g. '~'"))
    (errorOf (decodeText @T.Text "~"))
  assertEqual
    "tagged integer instead of string"
    (Just (1, 7, "expected a string, but got an integer"))
    (errorOf (decodeText @T.Text "!!int 5"))
  assertEqual
    "out of range"
    (Just (1, 1, "the integer is out of the range from -128 to 127"))
    (errorOf (decodeText @Int8 "300"))
  assertEqual
    "custom failure"
    (Just (1, 5, "not a vowel"))
    (errorOf (decodeText @[Vowel] "[a, x]"))
  assertEqual "ordering" (Just (1, 1, "expected LT, EQ or GT")) (errorOf (decodeText @Ordering "lt"))
  assertEqual
    "uppercase UUID"
    (Right (UUID.fromWords 0x123e4567 0xe89b12d3 0xa4564266 0x14174000))
    (decodeText "123E4567-E89B-12D3-A456-426614174000")
  assertEqual
    "invalid UUID"
    (Just (1, 1, "expected a UUID such as 123e4567-e89b-12d3-a456-426614174000"))
    (errorOf (decodeText @UUID.UUID "123e4567e89b12d3a456426614174000"))
  assertEqual "void" (Just (1, 1, "the type Void has no values")) (errorOf (decodeText @Void "a"))
  assertEqual
    "zero denominator"
    (Just (1, 1, "the denominator is 0"))
    (errorOf (decodeText @Rational "{numerator: 1, denominator: 0}"))
  assertEqual "negative denominator" (Right (negate 1 % 2 :: Rational)) (decodeText "{numerator: 2, denominator: -4}")
  assertEqual
    "negation of minBound"
    (Just (1, 1, "the fraction is out of the range of the type"))
    (errorOf (decodeText @(Ratio Int) "{numerator: -9223372036854775808, denominator: -1}"))
  assertEqual
    "minBound as the denominator"
    (Just (1, 1, "the fraction is out of the range of the type"))
    (errorOf (decodeText @(Ratio Int) "{numerator: 1, denominator: -9223372036854775808}"))
  assertEqual
    "minBound reduced"
    (Right (negate 4611686018427387904 % 1 :: Ratio Int))
    (decodeText "{numerator: -9223372036854775808, denominator: 2}")
  assertEqual "fixed from an integer" (Right (3 :: Centi)) (decodeText "3")
  assertEqual "fixed with fewer digits" (Right (1.5 :: Centi)) (decodeText "1.5")
  assertEqual "fixed with an exponent" (Right (120 :: Centi)) (decodeText "1.2e2")
  assertEqual "fixed with too many digits" (Just (1, 1, "expected a multiple of 0.01")) (errorOf (decodeText @Centi "1.239"))
  assertEqual "largest fixed" (Right (10 ^ (1000 :: Int) :: Centi)) (decodeText "1e1000")
  assertEqual "resolution of 2s and 5s" (Right (MkFixed 7 :: Fixed Fortieths)) (decodeText "0.175")
  assertEqual
    "step of a resolution of 2s and 5s"
    (Just (1, 1, "expected a multiple of 0.025"))
    (errorOf (decodeText @(Fixed Fortieths) "0.01"))
  assertEqual "whole number for a resolution without a decimal form" (Right (MkFixed 6 :: Fixed Thirds)) (decodeText "2")
  assertEqual
    "step of a resolution without a decimal form"
    (Just (1, 1, "expected a multiple of 1/3"))
    (errorOf (decodeText @(Fixed Thirds) "0.7"))
  assertEqual
    "fixed with a huge exponent"
    (Left "the exponent of the number is out of the range from -1000 to 1000")
    (first (snd . NE.head) (runParser (parseYaml @Centi) (toYaml (Float (Finite (Sci.scientific 1 maxBound))))))
  assertEqual
    "zero fixed with a huge exponent"
    (Right (0 :: Centi))
    (runParser parseYaml (toYaml (Float (Finite (Sci.scientific 0 maxBound)))))

newtype Vowel = Vowel Char

instance FromYaml Vowel where
  parseYaml = withText $ \t -> case T.unpack t of
    [c] | c `elem` ("aeiou" :: String) -> pure (Vowel c)
    _ -> fail "not a vowel"

-- | The applicative operators collect the errors of both parts, and '>>=' and
-- '>>' stop at the first error.
test_collectedErrors :: Assertion
test_collectedErrors = do
  assertEqual
    "fields"
    [ (1, 7, "expected a string, but got a list")
    , (2, 8, "expected a list, but got an integer")
    , (3, 7, "expected an integer, but got a string")
    ]
    (errorsOf (decodeText @Config "name: [x]\npaths: 1\njobs: x\n"))
  assertEqual
    "unknown keys"
    [ (2, 1, "unknown key \"job\", did you mean \"jobs\"?")
    , (3, 1, "unknown key \"bogus\", expected one of: name, paths, jobs")
    ]
    (errorsOf (decodeText @Config "name: x\njob: 1\nbogus: 2\n"))
  assertEqual
    "list of the known keys once"
    [ (2, 1, "unknown key \"foo\", expected one of: name, paths, jobs")
    , (3, 1, "unknown key \"bar\"")
    , (4, 1, "unknown key \"job\", did you mean \"jobs\"?")
    ]
    (errorsOf (decodeText @Config "name: x\nfoo: 1\nbar: 2\njob: 3\n"))
  assertEqual
    "statement of a do block"
    [(2, 1, "unknown key \"bogus\", expected one of: name, paths, jobs")]
    (errorsOf (decodeText @Config "name: [x]\nbogus: 1\n"))
  assertEqual
    "items of a list"
    [(1, 5, "expected an integer, but got a string"), (1, 11, "expected an integer, but got a string")]
    (errorsOf (decodeText @[Int] "[1, x, 2, y]"))
  assertEqual
    "keys and values of a map"
    [ (1, 5, "expected an integer, but got a string")
    , (1, 8, "expected a string, but got an integer, quote the value, e.g. '1'")
    , (1, 17, "expected an integer, but got a string")
    ]
    (errorsOf (decodeText @(M.Map T.Text Int) "{a: x, 1: 2, b: y}"))
  assertEqual
    "fields of a fraction"
    [ (1, 12, "expected an integer, but got a string")
    , (2, 14, "expected an integer, but got a string")
    , (3, 1, "unknown key \"extra\", expected one of: numerator, denominator")
    ]
    (errorsOf (decodeText @Rational "numerator: x\ndenominator: y\nextra: 1\n"))
  assertEqual
    "fields of a calendar difference"
    [ (1, 9, "expected an integer, but got a string")
    , (2, 7, "expected an integer, but got a string")
    , (3, 1, "unknown key \"weeks\", expected one of: months, days")
    ]
    (errorsOf (decodeText @CalendarDiffDays "months: x\ndays: y\nweeks: 1\n"))
  assertEqual
    "duplicate keys of a map"
    [ (1, 8, "duplicate key after conversion")
    , (1, 2, "the first key")
    , (1, 22, "duplicate key after conversion")
    , (1, 16, "the first key")
    ]
    (errorsOf (decodeText @(M.Map Double T.Text) "{1: a, 1.0: b, 2: c, 2.0: d}"))
  assertEqual
    "duplicate elements of a set"
    [ (1, 5, "duplicate element after conversion")
    , (1, 2, "the first element")
    , (1, 13, "duplicate element after conversion")
    , (1, 10, "the first element")
    ]
    (errorsOf (decodeText @(Set.Set Double) "[1, 1.0, 2, 2.0]"))
  assertEqual
    "elements of a set"
    [(1, 2, "expected a number, but got a string"), (1, 5, "expected a number, but got a string")]
    (errorsOf (decodeText @(Set.Set Double) "[x, y]"))
  assertEqual
    "duplicate and invalid elements of a set"
    [(1, 5, "duplicate element after conversion"), (1, 2, "the first element"), (1, 10, "expected a number, but got a string")]
    (errorsOf (decodeText @(Set.Set Double) "[1, 1.0, x]"))
  assertEqual
    "duplicate elements of an int set"
    [(1, 5, "duplicate element"), (1, 2, "the first element"), (1, 10, "duplicate element"), (1, 2, "the first element")]
    (errorsOf (decodeText @IS.IntSet "[1, 0x1, 1]"))
  let count :: (S.Node -> Parser ()) -> Int
      count p = either (error . show) (either length (const 0) . runParser p) (decodeText @Node "[x, y]")
      item :: S.Node -> Parser Int
      item = parseNode parseYaml
      pair :: (Parser Int -> Parser Int -> Parser r) -> S.Node -> Parser ()
      pair op = withSequence $ \case
        [a, b] -> void (op (item a) (item b))
        _ -> fail "expected two items"
  assertEqual "traverse" 2 (count (withSequence (void . traverse item)))
  assertEqual "traverse_" 2 (count (withSequence (traverse_ item)))
  assertEqual "mapM_" 1 (count (withSequence (mapM_ item)))
  assertEqual "<*>" 2 (count (pair (\a b -> (,) <$> a <*> b)))
  assertEqual "*>" 2 (count (pair (*>)))
  assertEqual "<*" 2 (count (pair (<*)))
  assertEqual ">>" 1 (count (pair (>>)))
  assertEqual ">>=" 1 (count (pair (\a b -> a >>= const b)))

test_keyErrors :: Assertion
test_keyErrors = do
  let merged = "base: &b\n  x: 1\nc:\n  <<: *b\n"
  assertEqual
    "value of a merge key"
    (Just (4, 7, "expected an integer, but got a mapping, merge keys are not supported"))
    (errorOf (decodeText @(M.Map T.Text (M.Map T.Text Int)) merged))
  assertEqual
    "unknown merge key"
    (Just (4, 3, "unknown key \"<<\", merge keys are not supported"))
    (errorOf (decodeText @[Config] "- &b\n  name: x\n  jobs: 2\n- <<: *b\n"))
  assertEqual
    "key missing next to a merge key"
    (Right (Left (pure (Offset 0, "missing key \"x\", merge keys are not supported"))))
    (runParser (withMapping (`parseField` "x")) <$> decodeText @Node "<<: {x: 1}\n" :: Either (NE.NonEmpty Error) (Either (NE.NonEmpty (Offset, String)) Int))
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
      lookupError key input = case runParser (withMapping $ \o -> parseField @T.Text o key) <$> decodeText input of
        Right (Left ((_, msg) NE.:| [])) -> Just msg
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
    (Just ((21, 1, "duplicate key \"k1\""), (1, 1, "the first key \"k1\"")))
    (errorWithNote (decodeAllText @Value (T.unlines [T.pack ("k" ++ show i ++ ": 1") | i <- [1 .. 20 :: Int] ++ [1]])))
  assertEqual
    "duplicate scalar key after a collection key"
    (Just ((3, 1, "duplicate key \"a\""), (1, 1, "the first key \"a\"")))
    (errorWithNote (decodeAllText @Value (withKeys ["a", "[b]", "a"])))
  assertEqual
    "duplicate collection key"
    (Just ((2, 1, "duplicate key"), (1, 1, "the first key")))
    (errorWithNote (decodeAllText @Value (withKeys ["{c: [d]}", "{c: [d]}"])))
  assertEqual
    "duplicate mapping key in another order"
    (Just ((2, 1, "duplicate key"), (1, 1, "the first key")))
    (errorWithNote (decodeAllText @Value (withKeys ["{a: 1, b: 2}", "{b: 2, a: 1}"])))

-- | The check for duplicate keys compares keys with aliases correctly.
test_aliasKeys :: Assertion
test_aliasKeys = do
  let check :: String -> Maybe ((Int, Int, String), (Int, Int, String)) -> T.Text -> Assertion
      check preface expected keys = assertEqual preface expected (errorWithNote (decodeAllText @Value (laughs 3 <> keys)))
  check "different keys" Nothing "? *a3\n: 1\n? [*a2, 1]\n: 2\n? [*a2, 2]\n: 3\n"
  check "duplicate key" (Just ((7, 3, "duplicate key"), (5, 3, "the first key"))) "? [*a3, 1]\n: 1\n? [*a3, 1]\n: 2\n"
  check "duplicate alias key" (Just ((7, 3, "duplicate key *a3"), (5, 3, "the first key *a3"))) "? *a3\n: 1\n? *a3\n: 2\n"
  -- Keys from two separate chains of anchors are equal only after an
  -- expansion to 2^12 items.
  let chains :: T.Text -> T.Text -> T.Text
      chains x y =
        T.unlines $
          ["- &a0 [" <> x <> "]", "- &b0 [" <> y <> "]"]
            ++ [ T.pack ("- &" ++ c : show i ++ " [*" ++ c : show (i - 1) ++ ", *" ++ c : show (i - 1) ++ "]")
               | i <- [1 .. 12 :: Int]
               , c <- "ab"
               ]
            ++ ["- ? *a12", "  : 1", "  ? *b12", "  : 2"]
  assertEqual
    "equal chains"
    (Just ((29, 5, "duplicate key *b12, the same value as the first key"), (27, 5, "the first key *a12")))
    (errorWithNote (decodeAllText @Value (chains "x" "x")))
  assertEqual "different chains" Nothing (errorWithNote (decodeAllText @Value (chains "x" "y")))

-- | Aliases can add 100000 visits to a traversal of a small document, and as
-- many visits as the document has to a large one. Each node and each
-- character of a scalar is a visit.
test_aliasLimit :: Assertion
test_aliasLimit = do
  assertEqual "small expansion" Nothing (errorOf (decodeAllText @Value (laughs 3)))
  assertEqual
    "exponential expansion"
    (Just (5, 25, "the aliases expand the document to more than 100151 nodes and characters"))
    (errorOf (decodeAllText @Value (laughs 9)))
  let items = T.intercalate ", " (replicate 200000 "x")
      copies :: Int -> T.Text
      copies k = T.unlines ("- &a [" <> items <> "]" : replicate k "- *a")
  assertEqual "large document with one copy" Nothing (errorOf (decodeAllText @Value (copies 1)))
  assertEqual
    "large document with two copies"
    (Just (3, 3, "the aliases expand the document to more than 800008 nodes and characters"))
    (errorOf (decodeAllText @Value (copies 2)))
  let long = T.replicate 100000 "x"
      textCopies :: Int -> T.Text
      textCopies k = T.unlines ("- &a " <> long : replicate k "- *a")
  assertEqual "long scalar with one copy" Nothing (errorOf (decodeAllText @Value (textCopies 1)))
  assertEqual
    "long scalar with many copies"
    (Just (3, 3, "the aliases expand the document to more than 202004 nodes and characters"))
    (errorOf (decodeAllText @Value (textCopies 1000)))

-- | Anchors a0 to ak, where each anchor after a0 has ten aliases to the one
-- before it, and the alias *ak expands to about 10^(k+1) nodes.
laughs :: Int -> T.Text
laughs k =
  T.unlines $
    "a0: &a0 [x, x, x, x, x, x, x, x, x, x]"
      : [ T.pack ("a" ++ show i ++ ": &a" ++ show i ++ " [" ++ L.intercalate ", " (replicate 10 ("*a" ++ show (i - 1))) ++ "]")
        | i <- [1 .. k]
        ]

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
    (Right (Float (Finite (Sci.scientific (10 ^ (1000000 :: Int) - 1) (-999999)))))
    (decodeText @Value ("9." <> nines 999999))
  assertEqual
    "exponent"
    (Just (1, 1, "the exponent of the number is out of the range from -1000 to 1000, quote the value if it is a string, e.g. '1e" ++ T.unpack (nines 1000000) ++ "'"))
    (errorOf (decodeText @Value ("1e" <> nines 1000000)))
  let zeros = T.replicate 300000 "0"
  assertEqual
    "trailing zeros"
    ( Just
        ( (1, 600018, "duplicate key 0.1" ++ T.unpack zeros ++ "0, the same value as the first key")
        , (1, 2, "the first key 0.1" ++ T.unpack zeros)
        )
    )
    (errorWithNote (decodeAllText @Value ("{0.1" <> zeros <> ": a, 0.5" <> zeros <> ": b, 0.1" <> zeros <> "0: c}")))
  -- The gcd of a reduction takes quadratic time for most types.
  let big = 3 ^ (2000000 :: Int) :: Integer
  assertEqual
    "fraction"
    (Right big)
    (numerator <$> decodeText @Rational ("{numerator: " <> T.pack (show big) <> ", denominator: " <> T.pack (show (7 ^ (1200000 :: Int) :: Integer)) <> "}"))
  assertEqual
    "float with a long integer part"
    (Right (Float (Finite (Sci.scientific (10 ^ (1000000 :: Int) - 1) (-999000)))))
    (decodeText @Value (nines 1000 <> "." <> nines 999000))

-- | The search for a close known name does not compute the distance of a
-- long unknown name to each known name.
test_longUnknownNames :: Assertion
test_longUnknownNames = do
  let name = T.replicate 1000000 "a"
  assertEqual
    "value"
    (Just (1, 1, "unknown value " ++ show name ++ ", expected one of: small, large, 10"))
    (errorOf (decodeText @Size name))
  assertEqual
    "key"
    (Just (2, 3, "unknown key " ++ show name ++ ", expected one of: name, paths, jobs"))
    (errorOf (decodeText @Config ("name: x\n? " <> name <> "\n: 1\n")))

-- | The time of the check for duplicate keys is not quadratic in the number
-- of keys.
test_manyKeys :: Assertion
test_manyKeys = do
  let keys :: [T.Text]
      keys = [T.pack ("k" ++ show i) | i <- [1 .. 100000 :: Int]]
      count :: [T.Text] -> Either (NE.NonEmpty Error) Int
      count ks = length . entries <$> decodeText @Value (T.unlines (map (<> ": 1") ks))
  assertEqual "one collection key" (Right 100001) (count ("[c]" : keys))
  assertEqual "collection keys" (Right 100000) (count (map (\k -> "[" <> k <> "]") keys))
  assertEqual "mapping keys" (Right 100000) (count (map (\k -> "{a: " <> k <> "}") keys))
  let large = "{" <> T.intercalate ", " (map (<> ": 1") keys) <> "}"
  assertEqual
    "large equal keys"
    (Just ((3, 3, "duplicate key"), (1, 3, "the first key")))
    (errorWithNote (decodeAllText @Value ("? " <> large <> "\n: 1\n? " <> large <> "\n: 2\n")))
  let deep = nestedKey 14 "0"
  assertEqual
    "nested equal keys"
    (Just ((3, 3, "duplicate key"), (1, 3, "the first key")))
    (errorWithNote (decodeAllText @Value ("? " <> deep <> "\n: 1\n? " <> deep <> "\n: 2\n")))
  where
    -- Two mappings as keys that differ only in their last value.
    nestedKey :: Int -> T.Text -> T.Text
    nestedKey d v
      | d == 0 = v
      | otherwise = "{" <> nestedKey (d - 1) "0" <> ": 1, " <> nestedKey (d - 1) "1" <> ": " <> v <> "}"

    entries :: Value -> [(Value, Value)]
    entries = \case
      Mapping kvs -> kvs
      _ -> []

-- | A decoder error has the path to its node.
test_errorPaths :: Assertion
test_errorPaths = do
  check "nested key" (Right "hlint.version") $
    decodeText @(M.Map T.Text (M.Map T.Text T.Text)) "hlint:\n  version: 1\n"
  check "indices" (Right "[1][1]") $ decodeText @[[Int]] "- [1]\n- [2, x]\n"
  -- The mapping and its first key start at the same place.
  check "missing key" (Right "[1]") $ decodeText @[Config] "- name: x\n- jobs: 2\n"
  check "unknown key" (Right "[0]") $ decodeText @[Config] "- name: x\n  bogus: 1\n"
  check "key in quotes" (Right "\"a.b\".c") $
    decodeText @(M.Map T.Text (M.Map T.Text Int)) "\"a.b\":\n  c: x\n"
  check "key with escapes" (Right "\"a\\nb\\t\\\"\\x07\\u2028\\U000e0001\"") $
    decodeText @(M.Map T.Text Int) "\"a\\nb\\t\\\"\\a\\L\\U000E0001\": x\n"
  check "inside a key" (Right "a") $ decodeText @(M.Map T.Text (M.Map [Int] Int)) "a:\n  ? [1, x]\n  : 1\n"
  check "inside a key at the root" (Right "") $ decodeText @(M.Map (M.Map T.Text Int) Int) "? {port: x}\n: 1\n"
  check "in the value of a collection key" (Right "?[1]") $ decodeText @(M.Map [Int] [Int]) "? [1, 2]\n: [3, y]\n"
  check "string key ?" (Right "\"?\"[1]") $ decodeText @(M.Map T.Text [Int]) "'?': [3, y]\n"
  check "alias key" (Right "*a[1]") $ decodeText @(M.Map Value [Int]) "m: [&a 1]\n*a : [3, y]\n"
  check "string key like an alias" (Right "\"*a\"[1]") $ decodeText @(M.Map T.Text [Int]) "'*a': [3, y]\n"
  assertEqual
    "path elements"
    (Left [CollectionKey, Index 1])
    (first ((.path) . NE.head) (decodeText @(M.Map [Int] [Int]) "? [1, 2]\n: [3, y]\n"))
  check "empty value at the end of its key" (Right "a") $ decodeText @(M.Map T.Text Int) "{a}"
  check "empty value at the end of an explicit key" (Right "a") $ decodeText @(M.Map T.Text Int) "? a"
  check "empty key" (Right "") $ decodeText @(M.Map Int Int) ": 1\n"
  check "duplicate key" (Right "a") $ decodeText @Value "a:\n  b: 1\n  b: 2\n"
  check "root" (Right "") $ decodeText @Int "x"
  let key = S.plainNode "a"
  check "built node" (Right "") $ decodeDocument @(M.Map T.Text Int) "" (S.document (S.mappingNode [(key, key)]))
  where
    check :: String -> Either String String -> Either (NE.NonEmpty Error) a -> Assertion
    check preface expected r = assertEqual preface expected (either (Right . renderPath . (.path) . NE.head) (const (Left "no error")) r)

-- | 'errorsAt' gives the errors of 'errorAt', also for several errors on one
-- line and for offsets in any order.
prop_errorsAt :: Property
prop_errorsAt =
  forAll (T.pack <$> listOf (elements "ab \n\r\xFEFF\x17C\x1F600")) $ \input ->
    forAll (listOf (choose (-1, BS.length (T.encodeUtf8 input) + 1))) $ \offs ->
      let errs = [(Offset o, show o) | o <- offs]
      in errorsAt input errs === map (uncurry (errorAt input)) errs

-- | 'nodePaths' gives the paths of 'nodePath' for every offset of a document.
test_nodePaths :: Assertion
test_nodePaths = do
  let input = "a:\n  - [b, {c: d}]\n  - ? [e]\n    : f\nb: &x {g: h}\nc: *x\n"
  case S.parseDocumentsText input of
    Right [doc] -> do
      let offs = map Offset [-1 .. T.length input + 1]
      assertEqual "in order" (map (`nodePath` doc.root) offs) (nodePaths offs doc.root)
      assertEqual "in reverse" (map (`nodePath` doc.root) (reverse offs)) (nodePaths (reverse offs) doc.root)
    r -> assertFailure (show r)

-- | The time to locate errors and to find their paths is linear in the number
-- of errors, also for errors on one line.
test_manyErrors :: Assertion
test_manyErrors = do
  let n = 100000 :: Int
  check "flow" ("[" <> T.intercalate ", " (replicate n "x") <> "]") (\i -> 1 + 3 * i) (\i -> (1, 2 + 3 * i))
  check "block" (T.concat (replicate n "- x\n")) (\i -> 2 + 4 * i) (\i -> (i + 1, 3))
  where
    check :: String -> T.Text -> (Int -> Int) -> (Int -> (Int, Int)) -> Assertion
    check preface input offset location = case S.parseDocumentsText input of
      Right [doc] -> do
        let n = length (items doc.root)
            offs = [Offset (offset i) | i <- [0 .. n - 1]]
        assertEqual
          (preface ++ ", locations")
          [location i | i <- [0 .. n - 1]]
          [(err.location.line, err.location.column) | err <- errorsAt input [(o, "e") | o <- offs]]
        assertEqual (preface ++ ", paths") [[Index i] | i <- [0 .. n - 1]] (nodePaths offs doc.root)
      _ -> assertFailure "expected one document"

    items :: S.Node -> [S.Node]
    items node = case node.content of
      S.SequenceContent _ xs -> xs
      _ -> []

test_prettyError :: Assertion
test_prettyError = do
  case decodeText @Config "name: x\npaths: 42\n" of
    Left errs -> assertEqual "rendered" [expected] (map (prettyError "config.yaml") (NE.toList errs))
    Right _ -> assertFailure "expected an error"
  case decodeAllText @Value ("a: " <> T.replicate 100 "x" <> ": " <> T.replicate 100 "y" <> "\n") of
    Left errs -> assertEqual "long line" [expectedLong] (map (prettyError "long.yaml") (NE.toList errs))
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
        [ "config.yaml:2:8: paths: expected a list, but got an integer"
        , "  |"
        , "2 | paths: 42"
        , "  |        ^"
        ]
