module EncodeTests (encodeTests) where

import Data.Fixed
import Data.Functor.Const
import Data.Functor.Identity
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Monoid qualified as Mon
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Scientific qualified as Sci
import Data.Semigroup qualified as Sem
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Data.Tree qualified as Tree
import Data.UUID.Types qualified as UUID
import Test.QuickCheck hiding (Fixed)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Fixed)

import Yamlet
import Yamlet.Syntax qualified as S

encodeTests :: TestTree
encodeTests =
  testGroup
    "Encode"
    [ testCase "block style" test_blockStyle
    , testCase "quoting" test_quoting
    , testCase "floats" test_floats
    , testProperty "float format" prop_floatFormat
    , localOption (mkTimeout 10000000) $ testCase "long floats" test_longFloats
    , testCase "literal block scalars" test_literal
    , testCase "tags" test_tags
    , testCase "syntax tree" test_syntax
    , testCase "kept nodes" test_keptNodes
    , testCase "comments of keys" test_commentedKeys
    , -- The renderers differ only in rare cases, e.g. for a key that needs an
      -- explicit entry. 10000 cases take about 0.2 s.
      localOption (QuickCheckTests 10000) $ testProperty "fast renderer" prop_fastRenderer
    , testProperty "fast renderer of several documents" prop_fastRendererAll
    , testCase "containers" test_containers
    , testCase "base" test_base
    , testCase "time" test_time
    , testProperty "round trip" prop_roundTrip
    , testProperty "syntax round trip" prop_syntaxRoundTrip
    ]

test_containers :: Assertion
test_containers = do
  assertEqual "set" "- 1\n- 2\n- 3\n" (encodeText (Set.fromList [3, 1, 2 :: Int]))
  assertEqual "int set" "- 1\n- 2\n- 3\n" (encodeText (IS.fromList [3, 1, 2]))
  assertEqual "left" "Left: 1\n" (encodeText (Left @Int @T.Text 1))
  assertEqual "right" "Right: a\n" (encodeText (Right @Int @T.Text "a"))
  roundTrip "int map" (IM.fromList [(1, "a"), (-2, "b" :: T.Text)])
  roundTrip "sequence" (Seq.fromList [1, 2, 3 :: Int])
  roundTrip "either" [Left 1, Right "a" :: Either Int T.Text]
  let tree = Tree.Node 'a' [Tree.Node 'b' [], Tree.Node 'c' [Tree.Node 'd' []]]
  assertEqual "tree" "- a\n- - - b\n    - []\n  - - c\n    - - - d\n        - []\n" (encodeText tree)
  roundTrip "tree" tree
  let uuid = UUID.fromWords 0x123e4567 0xe89b12d3 0xa4564266 0x14174000
  assertEqual "UUID" "123e4567-e89b-12d3-a456-426614174000\n" (encodeText uuid)
  roundTrip "UUIDs" [uuid, UUID.nil]
  roundTrip "tuple of 10" (1 :: Int, 'a', True, "b" :: T.Text, 2.5 :: Double, [1 :: Int], Just 'c', (), 'd', -1 :: Int)

test_base :: Assertion
test_base = do
  assertEqual "ordering" "- LT\n- EQ\n- GT\n" (encodeText [LT, EQ, GT])
  assertEqual "proxy" "null\n" (encodeText (Proxy @Int))
  assertEqual "unit" "[]\n" (encodeText ())
  roundTrip "unit" ()
  assertEqual
    "unit from null"
    (Left "expected an empty list, but got null")
    (either (Left . (.message) . NE.head) Right (decodeText @() "null"))
  assertEqual
    "unit from a list with items"
    (Left "expected an empty list, but got a list")
    (either (Left . (.message) . NE.head) Right (decodeText @() "[1]"))
  assertEqual "ratio" "numerator: 1\ndenominator: 3\n" (encodeText (1 % 3 :: Rational))
  assertEqual "fixed" "1.25\n" (encodeText (1.25 :: Centi))
  assertEqual "fixed with a trailing zero" "1.5\n" (encodeText (1.5 :: Milli))
  assertEqual "whole fixed" "3.0\n" (encodeText (3 :: Uni))
  assertEqual "newtype" "- 1\n- 2\n" (encodeText (Identity [1, 2 :: Int]))
  assertEqual "string in a newtype" "ab\n" (encodeText (Sem.Min ("ab" :: String)))
  roundTrip "ordering" [LT, EQ, GT]
  roundTrip "negative ratio" (negate 7 % 4 :: Rational)
  roundTrip "fixed" (-123.456 :: Milli)
  roundTrip "nano" (0.000000001 :: Nano)
  roundTrip "resolution of a power of 2" (MkFixed 3 :: Fixed Quarters)
  assertEqual "resolution of 2s and 5s" "0.025\n" (encodeText (MkFixed 1 :: Fixed Fortieths))
  roundTrip "resolution of 2s and 5s" (MkFixed 7 :: Fixed Fortieths)
  assertEqual "resolution without a decimal form" "0.3\n" (encodeText (MkFixed 1 :: Fixed Thirds))
  roundTrip "newtypes" (Down 'a', Sem.Max (1 :: Int), Mon.First (Just True), Sem.Sum (2.5 :: Double), Sem.All False, Const @Int @Bool 3)

-- | A resolution of 1/4, which has an exact decimal form.
data Quarters

instance HasResolution Quarters where
  resolution _ = 4

-- | A resolution of 1/40, which needs three places after the point.
data Fortieths

instance HasResolution Fortieths where
  resolution _ = 40

-- | A resolution of 1/3, which has no exact decimal form.
data Thirds

instance HasResolution Thirds where
  resolution _ = 3

test_time :: Assertion
test_time = do
  let noon = LocalTime (fromGregorian 2026 9 25) (TimeOfDay 12 30 5.25)
  assertEqual "day" "2026-09-25\n" (encodeText (fromGregorian 2026 9 25))
  assertEqual "time, a base-60 number in YAML 1.1" "'12:30:00'\n" (encodeText (TimeOfDay 12 30 0))
  assertEqual "time without trailing zeros" "'12:30:15.000001'\n" (encodeText (TimeOfDay 12 30 15.000001))
  assertEqual "time with picoseconds" "'12:30:15.000000000001'\n" (encodeText (TimeOfDay 12 30 15.000000000001))
  assertEqual "local time" "2026-09-25T12:30:05.25\n" (encodeText noon)
  assertEqual "UTC time" "2026-09-25T12:30:00Z\n" (encodeText (UTCTime (fromGregorian 2026 9 25) (12 * 3600 + 30 * 60)))
  assertEqual "zoned time" "2026-09-25T12:30:05.25-02:30\n" (encodeText (ZonedTime noon (minutesToTimeZone (-150))))
  assertEqual "day of the year 0, which PyYAML cannot build" "'0000-01-01'\n" (encodeText (fromGregorian 0 1 1))
  assertEqual "day of the year 10000" "10000-01-01\n" (encodeText (fromGregorian 10000 1 1))
  assertEqual
    "UTC leap second"
    "'2016-12-31T23:59:60.5Z'\n"
    (encodeText (UTCTime (fromGregorian 2016 12 31) 86400.5))
  assertEqual
    "local leap second"
    "'2016-12-31T23:59:60'\n"
    (encodeText (LocalTime (fromGregorian 2016 12 31) (TimeOfDay 23 59 60)))
  assertEqual
    "zoned time of the year 0"
    "'0000-06-01T12:00:00+01:00'\n"
    (encodeText (ZonedTime (LocalTime (fromGregorian 0 6 1) (TimeOfDay 12 0 0)) (hoursToTimeZone 1)))
  assertEqual
    "hour 24"
    "'2024-01-01T24:00:00'\n"
    (encodeText (LocalTime (fromGregorian 2024 1 1) (TimeOfDay 24 0 0)))
  assertEqual
    "time zone of 25 hours"
    "'2024-01-01T12:00:00+25:00'\n"
    (encodeText (ZonedTime (LocalTime (fromGregorian 2024 1 1) (TimeOfDay 12 0 0)) (hoursToTimeZone 25)))
  roundTrip "leap second" (UTCTime (fromGregorian 2016 12 31) 86400.5)
  assertEqual "duration" "1.5\n" (encodeText (1.5 :: NominalDiffTime))
  roundTrip "local time" noon
  roundTrip "UTC time" (UTCTime (fromGregorian (-44) 3 15) 0.000000000001)
  roundTrip "diff time" (picosecondsToDiffTime 123456789)
  assertEqual "month" "2026-09\n" (encodeText (YearMonth 2026 9))
  assertEqual "month of a negative year" "-0044-03\n" (encodeText (YearMonth (-44) 3))
  assertEqual "quarter" "2026-q3\n" (encodeText (YearQuarter 2026 Q3))
  assertEqual "quarter of a year" "q3\n" (encodeText Q3)
  assertEqual "day of the week" "monday\n" (encodeText Monday)
  assertEqual "calendar days" "months: 1\ndays: 2\n" (encodeText (CalendarDiffDays 1 2))
  assertEqual "calendar time" "months: 1\ntime: 1.5\n" (encodeText (CalendarDiffTime 1 1.5))
  roundTrip "months" [YearMonth 2026 1, YearMonth 12345 12, YearMonth (-1) 6]
  roundTrip "quarters" [YearQuarter 2026 Q1, YearQuarter (-5) Q4]
  roundTrip "quarters of a year" [Q1, Q2, Q3, Q4]
  roundTrip "days of the week" [Monday .. Sunday]
  roundTrip "calendar days" (CalendarDiffDays (-3) 40)
  roundTrip "calendar time" (CalendarDiffTime 2 (-0.000000000001))
  assertEqual
    "zoned time with a large offset"
    (Right (noon, 900))
    ( (\z -> (zonedTimeToLocalTime z, timeZoneMinutes (zonedTimeZone z)))
        <$> decodeText (encodeText (ZonedTime noon (minutesToTimeZone 900)))
    )

-- | Encoding a value and decoding the result gives the same value.
roundTrip :: (Eq a, Show a, ToYaml a, FromYaml a) => String -> a -> Assertion
roundTrip preface x = assertEqual preface (Right x) (decodeText (encodeText x))

test_blockStyle :: Assertion
test_blockStyle = assertEqual "output" expected (encodeText value)
  where
    value :: S.Node
    value =
      mapping
        [ "source_paths" .= ["." :: T.Text]
        , "exclude_paths" .= ["dist" :: T.Text, "dist-newstyle"]
        , "language" .= ("Haskell2010" :: T.Text)
        , "nested" .= mapping ["a" .= (1 :: Int), "b" .= [[True, False]]]
        , "records" .= [mapping ["x" .= (1.5 :: Double), "y" .= Null]]
        , "empty_list" .= ([] :: [Int])
        , "empty_map" .= mapping []
        ]

    expected :: T.Text
    expected =
      T.unlines
        [ "source_paths:"
        , "- ."
        , "exclude_paths:"
        , "- dist"
        , "- dist-newstyle"
        , "language: Haskell2010"
        , "nested:"
        , "  a: 1"
        , "  b:"
        , "  - - true"
        , "    - false"
        , "records:"
        , "- x: 1.5"
        , "  'y': null"
        , "empty_list: []"
        , "empty_map: {}"
        ]

test_quoting :: Assertion
test_quoting = do
  let check :: T.Text -> T.Text -> Assertion
      check expected s = assertEqual (show s) (expected <> "\n") (encodeText s)
  check "dist-newstyle" "dist-newstyle"
  check "-foo" "-foo"
  check "'-'" "-"
  check "'- a'" "- a"
  check "''" ""
  check "'true'" "true"
  check "'null'" "null"
  check "'12'" "12"
  check "'0x1F'" "0x1F"
  check "'1.5'" "1.5"
  check "'a: b'" "a: b"
  check "a:b" "a:b"
  check "'a #b'" "a #b"
  check "a#b" "a#b"
  check "'#a'" "#a"
  check "' a'" " a"
  check "'a '" "a "
  check "'---'" "---"
  check "\"a\\tb\"" "a\tb"
  check "\"\\x01\"" "\x01"
  check "\"\\uFEFF\"" "\xFEFF"
  check "zażółć" "zażółć"
  check "'yes'" "yes"
  check "'Off'" "Off"
  check "'y'" "y"
  check "yesterday" "yesterday"
  -- The texts that YAML 1.1 reads as other types.
  check "'22:22'" "22:22"
  check "'1:30.5'" "1:30.5"
  check "'1:5'" "1:5"
  check "'1:59'" "1:59"
  check "1:60" "1:60"
  check "'1_000'" "1_000"
  check "'0b101'" "0b101"
  check "'0b-1'" "0b-1"
  check "'0_b+1_0'" "0_b+1_0"
  check "-0b-1" "-0b-1"
  check "'1.5_0'" "1.5_0"
  check "'2024-01-01'" "2024-01-01"
  check "'2024-1-1 10:00:00 +02:00'" "2024-1-1 10:00:00 +02:00"
  check "'<<'" "<<"
  check "'='" "="
  check "'09:30'" "09:30"
  check "'1,000'" "1,000"
  check "'0,5'" "0,5"
  check "'trUe'" "trUe"
  check "'.e+9'" ".e+9"
  check "'0X1F'" "0X1F"
  check "'+_85'" "+_85"
  check "'8_11E3'" "8_11E3"
  check "'2024-1-1'" "2024-1-1"
  check "':foo'" ":foo"
  check "Truely" "Truely"
  check "1.2.3" "1.2.3"
  check "2024-01" "2024-01"
  check "\"a\\u2028b\"" "a\x2028\&b"
  check "\"a\\u2029b\"" "a\x2029\&b"
  assertEqual "YAML 1.1 boolean as a key" "'NO': Norway\n" (encodeText (mapping ["NO" .= ("Norway" :: T.Text)]))

-- | A float reads back as a float, not as an integer.
test_floats :: Assertion
test_floats = do
  assertEqual "integral double" "12.0\n" (encodeText (12 :: Double))
  assertEqual "double" "0.1\n" (encodeText (0.1 :: Double))
  assertEqual "small double" "0.01\n" (encodeText (0.01 :: Double))
  assertEqual "smallest decimal notation" "0.000001\n" (encodeText (1e-6 :: Double))
  assertEqual "below decimal notation" "1.0e-7\n" (encodeText (1e-7 :: Double))
  assertEqual "largest decimal notation" "100000000000000000000.0\n" (encodeText (1e20 :: Double))
  assertEqual "above decimal notation" "1.0e+21\n" (encodeText (1e21 :: Double))
  assertEqual "large scientific" "1.0e+30\n" (encodeText (Sci.scientific 1 30))
  assertEqual
    "exact scientific"
    "12345678901234567890.123\n"
    (encodeText (Sci.scientific 12345678901234567890123 (-3)))
  assertEqual "exponent beyond the limit" "1.0e+10001\n" (encodeText (Sci.scientific 1 10001))
  assertEqual "exponent beyond Int" "1.0e+9223372036854775808\n" (encodeText (Sci.scientific 10 maxBound))
  assertEqual "negative exponent beyond Int" "-1.23e+9223372036854775810\n" (encodeText (Sci.scientific (-1230) maxBound))
  assertEqual "zero with a large exponent" "0.0\n" (encodeText (Sci.scientific 0 maxBound))
  assertEqual "infinity" "-.inf\n" (encodeText (-(1 / 0) :: Double))
  assertEqual "not a number" ".nan\n" (encodeText (0 / 0 :: Double))
  assertEqual "float" "0.1\n" (encodeText @Float 0.1)
  assertEqual "float infinity" "-.inf\n" (encodeText @Float (-(1 / 0)))
  assertEqual "float not a number" ".nan\n" (encodeText @Float (0 / 0))
  assertEqual "negative zero" "-0.0\n" (encodeText @Double (-0))
  assertEqual "float negative zero" "-0.0\n" (encodeText @Float (-0))

-- | A float has decimal notation from 10^-6 up to 10^21, as Number::toString
-- of ECMAScript, and exponential notation otherwise, with the sign of the
-- exponent.
prop_floatFormat :: Integer -> Property
prop_floatFormat c = forAll ((,) <$> chooseInt (0, 3) <*> chooseInt (-30, 30)) $ \(zeros, e) ->
  let s = Sci.scientific (c * 10 ^ zeros) e
      expected
        | s == 0 || (abs s >= Sci.scientific 1 (-6) && abs s < Sci.scientific 1 21) = Sci.formatScientific Sci.Fixed Nothing s
        | otherwise = case break (== 'e') (Sci.formatScientific Sci.Exponent Nothing s) of
            (m, 'e' : ex@(d : _)) | d /= '-' -> m ++ "e+" ++ ex
            _ -> Sci.formatScientific Sci.Exponent Nothing s
  in encodeText s === T.pack expected <> "\n"

-- | The time to write a float is not quadratic in the number of its digits.
test_longFloats :: Assertion
test_longFloats = do
  let nines = 10 ^ (1000000 :: Int) - 1
  assertEqual
    "digits"
    ("9." <> T.replicate 999999 "9" <> "\n")
    (encodeText (Sci.scientific nines (-999999)))
  assertEqual
    "trailing zeros"
    "1.5e+1000000\n"
    (encodeText (Sci.scientific (15 * 10 ^ (1000000 :: Int)) (-1)))

test_literal :: Assertion
test_literal = do
  assertEqual "clip" "key: |\n  a\n  b\n" (encodeText (mapping ["key" .= ("a\nb\n" :: T.Text)]))
  assertEqual "strip" "key: |-\n  a\n  b\n" (encodeText (mapping ["key" .= ("a\nb" :: T.Text)]))
  assertEqual "keep" "key: |+\n  a\n\n" (encodeText (mapping ["key" .= ("a\n\n" :: T.Text)]))
  assertEqual "only line breaks" "key: \"\\n\\n\"\n" (encodeText (mapping ["key" .= ("\n\n" :: T.Text)]))
  assertEqual "indentation indicator" "- |2-\n    a\n  b\n" (encodeText ["  a\nb" :: T.Text])
  assertEqual "indentation indicator for a tab" "- |2-\n  \ta\n  b\n" (encodeText ["\ta\nb" :: T.Text])
  assertEqual "indentation indicator after empty lines" "- |2\n\n  \ta\n" (encodeText ["\n\ta\n" :: T.Text])
  assertEqual "no indentation indicator at the top level" "\" a\\nb\"\n" (encodeText @T.Text " a\nb")
  assertEqual "no indentation indicator for a tab at the top level" "\"\\ta\\nb\"\n" (encodeText @T.Text "\ta\nb")
  let keep = mapping ["key" .= ("a\n\n" :: T.Text), "next" .= ("b" :: T.Text)]
  assertEqual
    "keep in a syntax tree"
    (encodeText keep)
    (S.renderSyntax S.defaultRenderOptions [S.document keep])

test_tags :: Assertion
test_tags = do
  let local = Tagged "!point" (Mapping [(String "x", Int 1)])
  assertEqual "local tag" "!point\nx: 1\n" (encodeText local)
  let str = Tagged "!name" (String "foo")
  assertEqual "tagged scalar" "- !name foo\n" (encodeText [str])
  let readBack :: T.Text -> Either (NE.NonEmpty Error) T.Text
      readBack t = valueTag <$> decodeText @Value (encodeText (Tagged t (String "x")))
      exact :: T.Text -> Assertion
      exact t = assertEqual (T.unpack t) (Right t) (readBack t)
  exact "!a b!c%"
  exact "!!x"
  exact "tag:yaml.org,2002:a,b é"
  exact "tag:example.com,2000:a%41,[b]"
  exact "tag:example.com,2000:a b>%"
  exact "foo"
  assertEqual
    "directives after a document"
    (Right [strTag, "foo"])
    (map valueTag <$> decodeAllText @Value (encodeAllText [String "a", Tagged "foo" (String "b")]))

test_syntax :: Assertion
test_syntax =
  assertEqual "output" expected $
    S.renderSyntax
      S.defaultRenderOptions
      [S.document (edit value)]
  where
    value :: S.Node
    value = mapping ["name" .= ("x" :: T.Text), "paths" .= ["a" :: T.Text, "b"]]

    -- Add a comment above the first key and use the flow style for the list.
    edit :: S.Node -> S.Node
    edit n = case n.content of
      S.Mapping style [(k1, v1), (k2, v2)] ->
        n
          { S.content =
              S.Mapping
                style
                [ (k1 {S.comments = S.noComments {S.before = [S.Comment "The name."]}}, v1)
                , (k2, v2 {S.content = flow v2.content})
                ]
          }
      _ -> n

    flow :: S.Content -> S.Content
    flow = \case
      S.Sequence _ xs -> S.Sequence S.Flow xs
      c -> c

    expected :: T.Text
    expected = T.unlines ["# The name.", "name: x", "paths: [a, b]"]

-- | A record that keeps a part of its document as it was written.
data Workflow = Workflow {name :: T.Text, jobs :: Int, matrix :: Node}

instance FromYaml Workflow where
  parseYaml = withMapping $ \o ->
    Workflow
      <$> parseField o "name"
      <*> parseField o "jobs"
      <*> parseField o "matrix"

instance ToYaml Workflow where
  toYaml w = mapping ["name" .= w.name, "jobs" .= w.jobs, "matrix" .= w.matrix]

test_keptNodes :: Assertion
test_keptNodes = do
  case decodeText @Workflow input of
    Left errs -> assertFailure (unlines (map (prettyError "input") (NE.toList errs)))
    Right w -> do
      assertEqual "decoded field" "demo" w.name
      assertEqual "output" expected (encodeText (Workflow w.name 8 w.matrix))
  assertEqual
    "kept nodes in a list"
    (Right "- ['9.10', \"9.12\"] # versions\n- {a: 1}\n")
    (encodeText <$> decodeText @[Node] "- ['9.10', \"9.12\"] # versions\n- {a: 1}\n")
  assertEqual
    "comments inside an alias"
    (Right "a: &x\n  k: v # c1\nb: # c2\n  k: v\n")
    (encodeText <$> decodeText @Node "a: &x\n  k: v # c1\nb: *x # c2\n")
  assertEqual
    "comment after the tag of a mapping"
    (Right "# c1\na: 1\n")
    (encodeText <$> decodeText @(M.Map T.Text (Commented Node)) "!!map # c1\na: 1\n")
  assertEqual
    "comment after the tag of a list"
    (Right "# c1\n- 1\n")
    (encodeText <$> decodeText @[Commented Node] "!!seq # c1\n- 1\n")
  let commentedRoot = "# c1\n1 # c2\n# c3\n"
  assertEqual "commented scalar root" (Right commentedRoot) (encodeText <$> decodeText @(Commented Int) commentedRoot)
  let linesAfter = M.fromList [("a" :: T.Text, Commented (1 :: Int) noComments {after = [Comment "c"]}), ("b", Commented 2 noComments)]
  assertEqual "lines after a commented scalar value" "a: 1\n  # c\nb: 2\n" (encodeText linesAfter)
  assertEqual
    "lines after a commented scalar value read back"
    (Right (M.map (.comments) linesAfter))
    (M.map (.comments) <$> decodeText @(M.Map T.Text (Commented Int)) (encodeText linesAfter))
  let linesAbove = [Commented [1, 2 :: Int] noComments {before = [Comment "above"], inline = Just "inline"}]
  assertEqual "lines above a commented list item" "- # inline\n  # above\n\n  - 1\n  - 2\n" (encodeText linesAbove)
  assertEqual
    "lines above a commented list item read back"
    (Right [noComments {before = [Comment "above", EmptyLine], inline = Just "inline"}])
    (map (.comments) <$> decodeText @[Commented [Int]] (encodeText linesAbove))
  let scalarRoot = "|\n  text\n# end\n"
  assertEqual "lines at the end of a scalar root" (Right scalarRoot) (encodeText <$> decodeText @Node scalarRoot)
  assertEqual
    "scalar root in a list"
    (Right "- |\n  text\n# end\n- 1\n")
    (encodeText . (: [toYaml @Int 1]) <$> decodeText @Node scalarRoot)
  where
    input :: T.Text
    input =
      T.unlines
        [ "name: demo"
        , "jobs: 4"
        , "matrix:"
        , "  # The operating systems."
        , "  os: [ubuntu, macos] # two for now"
        , "  ghc: ['9.10', \"9.12\"]"
        , "  base: &b"
        , "    x: 1"
        , "  copy: *b"
        ]

    -- The alias becomes a copy of the node that it refers to.
    expected :: T.Text
    expected =
      T.unlines
        [ "name: demo"
        , "jobs: 8"
        , "matrix:"
        , "  # The operating systems."
        , "  os: [ubuntu, macos] # two for now"
        , "  ghc: ['9.10', \"9.12\"]"
        , "  base: &b"
        , "    x: 1"
        , "  copy:"
        , "    x: 1"
        ]

-- | A record that keeps the comments of a key.
data Job = Job {name :: T.Text, permissions :: Commented Node}

instance FromYaml Job where
  parseYaml = withMapping $ \o ->
    Job
      <$> parseField o "name"
      <*> parseField o "permissions"

instance ToYaml Job where
  toYaml j = mapping ["name" .= j.name, "permissions" .= j.permissions]

test_commentedKeys :: Assertion
test_commentedKeys = do
  let job = T.unlines ["name: build", "# The test reporter writes check runs.", "permissions: # read-only", "  contents: read"]
  assertEqual "record" (Right job) (encodeText <$> decodeText @Job job)
  assertEqual
    "map"
    (Right "# one\na: 1\nb: 2 # two\n")
    (encodeText <$> decodeText @(M.Map T.Text (Commented Int)) "# one\na: 1\nb: 2 # two\n")
  -- The parser gives the lines before the marker and at the end to the
  -- document, and the decoder gives them to the root. The renderer separates
  -- the lines of a block root from its first entry, so that they read back
  -- as the lines of the root.
  let top = "# top\n\na: 1\n# end\n"
  assertEqual
    "comments of the document"
    (Right top)
    (encodeText <$> decodeText @(Commented (M.Map T.Text Int)) "# top\n---\na: 1\n# end\n")
  assertEqual
    "comments of the document read back"
    (Right top)
    (encodeText <$> decodeText @(Commented (M.Map T.Text Int)) top)
  let either_ = "# The name.\nLeft: foo # current\n"
  assertEqual
    "key of an either"
    (Right either_)
    (encodeText <$> decodeText @(Either (Commented T.Text) Int) either_)
  let set = "# first\n- a # one\n- b\n"
  assertEqual "set" (Right set) (encodeText <$> decodeText @(Set.Set (Commented T.Text)) set)
  -- The comment after 1 belongs to the value, which an integer cannot keep.
  assertEqual
    "keys of a map"
    (Right "# above\na: 1\n")
    (encodeText <$> decodeText @(M.Map (Commented T.Text) Int) "# above\na: 1 # c\n")
  -- Both take the comments of the key, and the encoder writes them once.
  assertEqual
    "keys and values of a map"
    (Right "# above\na: 1 # c\n")
    (encodeText <$> decodeText @(M.Map (Commented T.Text) (Commented Int)) "# above\na: 1 # c\n")
  let nodes = "os: [a, b] # two\nsteps:\n- x\n  # end\n"
  assertEqual
    "nodes keep their comments once"
    (Right nodes)
    (encodeText <$> decodeText @(M.Map T.Text (Commented Node)) nodes)
  assertEqual
    "list items"
    (Right [S.Comments [S.Comment "c"] Nothing [], S.Comments [] (Just "d") []])
    (map (.comments) <$> decodeText @[Commented Int] "# c\n- 1\n- 2 # d\n")
  -- The comment after the list belongs to the entry, and the comment above
  -- the first item belongs to the item.
  let branches = "branches: # which branches\n# the main branch\n- main # the old default\n- dev\n  # more later\n"
  assertEqual
    "commented items"
    (Right branches)
    (encodeText <$> decodeText @(M.Map T.Text (Commented [Commented T.Text])) branches)

-- | The faster renderer of the encoder gives the same output as the renderer
-- of syntax trees.
prop_fastRenderer :: Doc -> Property
prop_fastRenderer (Doc n) =
  encodeText n === S.renderSyntax S.defaultRenderOptions [S.document (toYaml n)]

-- | The same for several documents.
prop_fastRendererAll :: [Doc] -> Property
prop_fastRendererAll docs =
  encodeAllText ns === S.renderSyntax S.defaultRenderOptions (map (S.document . toYaml) ns)
  where
    ns :: [Value]
    ns = [n | Doc n <- docs]

-- | Encoding a value and decoding the result gives the same value.
prop_roundTrip :: Doc -> Property
prop_roundTrip (Doc n) = readsBack (encodeText n) n

-- | Rendering the syntax tree of a value and decoding the result gives the
-- same value.
prop_syntaxRoundTrip :: Doc -> Property
prop_syntaxRoundTrip (Doc n) = readsBack output n
  where
    output :: T.Text
    output = S.renderSyntax S.defaultRenderOptions [S.document (toYaml n)]

readsBack :: T.Text -> Value -> Property
readsBack output n = case decodeAllText @Value output of
  Right [n'] -> counterexample (T.unpack output) $ n' === n
  r -> counterexample (T.unpack output ++ "\n" ++ show r) False

newtype Doc = Doc Value
  deriving stock (Show)

instance Arbitrary Doc where
  arbitrary = Doc <$> sized genValue

genValue :: Int -> Gen Value
genValue size
  | size <= 1 = genScalar
  | otherwise =
      frequency
        [ (3, genScalar)
        , (1, Sequence <$> genList)
        , (1, Mapping <$> genEntries)
        , (1, tagged <$> genScalar)
        , (1, Tagged <$> genTag <*> (Sequence <$> genList))
        ]
  where
    genList :: Gen [Value]
    genList = do
      k <- choose (0, 4)
      vectorOf k (genValue (size `div` 3))

    genEntries :: Gen [(Value, Value)]
    genEntries = do
      k <- choose (0, 4)
      keys <- L.nub <$> vectorOf k genKey
      mapM (\key -> (key,) <$> genValue (size `div` 3)) keys

    genKey :: Gen Value
    genKey = frequency [(4, genScalar), (1, elements [Sequence [], Mapping []])]

    -- A tag that is not a valid URI needs a %TAG directive.
    genTag :: Gen T.Text
    genTag = elements ["!custom", "xy"]

    tagged :: Value -> Value
    tagged v = case v of
      String _ -> Tagged "!custom" v
      _ -> v

genScalar :: Gen Value
genScalar =
  oneof
    [ pure Null
    , Bool <$> arbitrary
    , Int <$> arbitrary
    , Float . Finite <$> (Sci.scientific <$> arbitrary <*> chooseInt (-30, 30))
    , Float <$> elements [NegativeZero, Infinity, NegativeInfinity, NaN]
    , String <$> genText
    ]

genText :: Gen T.Text
genText =
  oneof
    [ elements tricky
    , T.pack <$> listOf genChar
    , T.intercalate "\n" <$> listOf (T.pack <$> listOf genChar)
    ]
  where
    tricky :: [T.Text]
    tricky =
      [ ""
      , " "
      , "-"
      , "- a"
      , "? a"
      , ": a"
      , "a: b"
      , "a:b"
      , "#"
      , "a #b"
      , "true"
      , "null"
      , "1"
      , "0x1F"
      , "0o7"
      , ".5"
      , "~"
      , "---"
      , "..."
      , "@x"
      , "`x"
      , "foo\n"
      , "\nfoo"
      , "  lead"
      , "trail  "
      , "a\n\nb\n\n"
      , "\t"
      , "é"
      , "\x85"
      , "\x2028"
      , "\xFEFF"
      , "\n"
      , "\n\n"
      , " \n"
      , "a\n "
      , "|"
      , ">"
      , "%x"
      , "&a"
      , "*a"
      , "!a"
      , "{}"
      , "[]"
      , "a, b"
      , "key:"
      , "'quoted'"
      , "\"dq\""
      , "\r\n"
      , "\\"
      , "a\tb"
      ]

    genChar :: Gen Char
    genChar =
      frequency
        [ (10, elements "abc xyz-:#,[]{}'\"!&*?|>%@`\\")
        , (2, elements "\t\r\x85\xA0\x2028\xFEFF\x01\x7F")
        , (1, arbitrary)
        ]
