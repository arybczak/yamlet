module EncodeTests (encodeTests) where

import Data.Fixed
import Data.Functor.Const
import Data.Functor.Identity
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
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
import Data.Version
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
  assertEqual "version" "1.10.2\n" (encodeText (makeVersion [1, 10, 2]))
  assertEqual "proxy" "null\n" (encodeText (Proxy @Int))
  assertEqual "ratio" "numerator: 1\ndenominator: 3\n" (encodeText (1 % 3 :: Rational))
  assertEqual "fixed" "1.25\n" (encodeText (1.25 :: Centi))
  assertEqual "fixed with a trailing zero" "1.5\n" (encodeText (1.5 :: Milli))
  assertEqual "whole fixed" "3.0\n" (encodeText (3 :: Uni))
  assertEqual "newtype" "- 1\n- 2\n" (encodeText (Identity [1, 2 :: Int]))
  assertEqual "string in a newtype" "ab\n" (encodeText (Sem.Min ("ab" :: String)))
  roundTrip "ordering" [LT, EQ, GT]
  roundTrip "version" (makeVersion [1, 10])
  roundTrip "negative ratio" (negate 7 % 4 :: Rational)
  roundTrip "fixed" (-123.456 :: Milli)
  roundTrip "nano" (0.000000001 :: Nano)
  roundTrip "resolution of a power of 2" (MkFixed 3 :: Fixed Quarters)
  assertEqual "resolution of 2s and 5s" "2.5e-2\n" (encodeText (MkFixed 1 :: Fixed Fortieths))
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
  assertEqual "time" "12:30:00\n" (encodeText (TimeOfDay 12 30 0))
  assertEqual "local time" "2026-09-25T12:30:05.250\n" (encodeText noon)
  assertEqual "UTC time" "2026-09-25T12:30:00Z\n" (encodeText (UTCTime (fromGregorian 2026 9 25) (12 * 3600 + 30 * 60)))
  assertEqual "zoned time" "2026-09-25T12:30:05.250-02:30\n" (encodeText (ZonedTime noon (minutesToTimeZone (-150))))
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
    value :: Node
    value =
      mapping
        [ "source_paths" .= ["." :: T.Text]
        , "exclude_paths" .= ["dist" :: T.Text, "dist-newstyle"]
        , "language" .= ("Haskell2010" :: T.Text)
        , "nested" .= mapping ["a" .= (1 :: Int), "b" .= [[True, False]]]
        , "records" .= [mapping ["x" .= (1.5 :: Double), "y" .= ()]]
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
        , "  y: null"
        , "empty_list: []"
        , "empty_map: {}"
        ]

test_quoting :: Assertion
test_quoting = do
  let check :: T.Text -> T.Text -> Assertion
      check expected s = assertEqual (show s) (expected <> "\n") (encodeText s)
  check "dist-newstyle" "dist-newstyle"
  check "-foo" "-foo"
  check "\"-\"" "-"
  check "\"- a\"" "- a"
  check "\"\"" ""
  check "\"true\"" "true"
  check "\"null\"" "null"
  check "\"12\"" "12"
  check "\"0x1F\"" "0x1F"
  check "\"1.5\"" "1.5"
  check "\"a: b\"" "a: b"
  check "a:b" "a:b"
  check "\"a #b\"" "a #b"
  check "a#b" "a#b"
  check "\"#a\"" "#a"
  check "\" a\"" " a"
  check "\"a \"" "a "
  check "\"---\"" "---"
  check "\"a\\tb\"" "a\tb"
  check "\"\\x01\"" "\x01"
  check "\"\\uFEFF\"" "\xFEFF"
  check "zażółć" "zażółć"

-- | A float reads back as a float, not as an integer.
test_floats :: Assertion
test_floats = do
  assertEqual "integral double" "12.0\n" (encodeText (12 :: Double))
  assertEqual "double" "0.1\n" (encodeText (0.1 :: Double))
  assertEqual "large scientific" "1.0e30\n" (encodeText (Sci.scientific 1 30))
  assertEqual
    "exact scientific"
    "1.2345678901234567890123e19\n"
    (encodeText (Sci.scientific 12345678901234567890123 (-3)))
  assertEqual "exponent beyond the limit" "1.0e10001\n" (encodeText (Sci.scientific 1 10001))
  assertEqual "exponent beyond Int" "1.0e9223372036854775808\n" (encodeText (Sci.scientific 10 maxBound))
  assertEqual "negative exponent beyond Int" "-1.23e9223372036854775810\n" (encodeText (Sci.scientific (-1230) maxBound))
  assertEqual "zero with a large exponent" "0.0\n" (encodeText (Sci.scientific 0 maxBound))
  assertEqual "infinity" "-.inf\n" (encodeText (-(1 / 0) :: Double))
  assertEqual "not a number" ".nan\n" (encodeText (0 / 0 :: Double))
  assertEqual "float" "0.1\n" (encodeText @Float 0.1)
  assertEqual "float infinity" "-.inf\n" (encodeText @Float (-(1 / 0)))
  assertEqual "float not a number" ".nan\n" (encodeText @Float (0 / 0))
  assertEqual "negative zero" "-0.0\n" (encodeText @Double (-0))
  assertEqual "float negative zero" "-0.0\n" (encodeText @Float (-0))

-- | A float has the generic format of the scientific package.
prop_floatFormat :: Integer -> Property
prop_floatFormat c = forAll ((,) <$> chooseInt (0, 3) <*> chooseInt (-30, 30)) $ \(zeros, e) ->
  let s = Sci.scientific (c * 10 ^ zeros) e
  in encodeText s === T.pack (Sci.formatScientific Sci.Generic Nothing s) <> "\n"

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
    "1.5e1000000\n"
    (encodeText (Sci.scientific (15 * 10 ^ (1000000 :: Int)) (-1)))

test_literal :: Assertion
test_literal = do
  assertEqual "clip" "key: |\n  a\n  b\n" (encodeText (mapping ["key" .= ("a\nb\n" :: T.Text)]))
  assertEqual "strip" "key: |-\n  a\n  b\n" (encodeText (mapping ["key" .= ("a\nb" :: T.Text)]))
  assertEqual "keep" "key: |+\n  a\n\n" (encodeText (mapping ["key" .= ("a\n\n" :: T.Text)]))
  assertEqual "indentation indicator" "- |2-\n    a\n  b\n" (encodeText ["  a\nb" :: T.Text])
  assertEqual "no indentation indicator at the top level" "\" a\\nb\"\n" (encodeText @T.Text " a\nb")
  let keep = mapping ["key" .= ("a\n\n" :: T.Text), "next" .= ("b" :: T.Text)]
  assertEqual
    "keep in a syntax tree"
    (encodeText keep)
    (S.renderSyntax S.defaultRenderOptions [S.document (toSyntax keep)])

test_tags :: Assertion
test_tags = do
  let local = Node noOffset "!point" (Mapping ["x" .= (1 :: Int)])
  assertEqual "local tag" "!point\nx: 1\n" (encodeText local)
  let str = Node noOffset "!name" (String "foo")
  assertEqual "tagged scalar" "- !name foo\n" (encodeText [str])
  let readBack :: T.Text -> Either Error T.Text
      readBack t = (.tag) <$> decodeText @Node (encodeText (Node noOffset t (String "x")))
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
    (map (.tag) <$> decodeAllText @Node (encodeAllText [node (String "a"), Node noOffset "foo" (String "b")]))

test_syntax :: Assertion
test_syntax =
  assertEqual "output" expected $
    S.renderSyntax
      S.defaultRenderOptions
      [S.document (edit (toSyntax value))]
  where
    value :: Node
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

-- | Encoding a node and decoding the result gives the same node.
prop_roundTrip :: Doc -> Property
prop_roundTrip (Doc n) = readsBack (encodeText n) n

-- | Rendering the syntax tree of a node and decoding the result gives the same
-- node.
prop_syntaxRoundTrip :: Doc -> Property
prop_syntaxRoundTrip (Doc n) = readsBack output n
  where
    output :: T.Text
    output = S.renderSyntax S.defaultRenderOptions [S.document (toSyntax n)]

readsBack :: T.Text -> Node -> Property
readsBack output n = case decodeNodes output of
  Right [n'] -> counterexample (T.unpack output) $ withoutOffsets n' === withoutOffsets n
  r -> counterexample (T.unpack output ++ "\n" ++ show r) False

newtype Doc = Doc Node
  deriving stock (Show)

instance Arbitrary Doc where
  arbitrary = Doc <$> sized genNode

genNode :: Int -> Gen Node
genNode size
  | size <= 1 = genScalar
  | otherwise =
      frequency
        [ (3, genScalar)
        , (1, node . Sequence <$> genList)
        , (1, node . Mapping <$> genEntries)
        , (1, tagged <$> genScalar)
        ]
  where
    genList :: Gen [Node]
    genList = do
      k <- choose (0, 4)
      vectorOf k (genNode (size `div` 3))

    genEntries :: Gen [(Node, Node)]
    genEntries = do
      k <- choose (0, 4)
      keys <- L.nubBy (\a b -> a.value == b.value) <$> vectorOf k genScalar
      mapM (\key -> (key,) <$> genNode (size `div` 3)) keys

    tagged :: Node -> Node
    tagged n = case n.value of
      String _ -> n {tag = "!custom"}
      _ -> n

genScalar :: Gen Node
genScalar =
  node
    <$> oneof
      [ pure Null
      , Bool <$> arbitrary
      , Int <$> arbitrary
      , Float <$> elements (map Finite [0, 1.5, -2.25e-10, 123456.789, 1e30, 12] ++ [Infinity, NegativeInfinity, NaN])
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
