module EncodeTests (encodeTests) where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.Scientific qualified as Sci
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet
import Yamlet.Syntax qualified as S

encodeTests :: TestTree
encodeTests =
  testGroup
    "Encode"
    [ testCase "block style" test_blockStyle
    , testCase "quoting" test_quoting
    , testCase "floats" test_floats
    , testCase "literal block scalars" test_literal
    , testCase "tags" test_tags
    , testCase "syntax tree" test_syntax
    , testCase "containers" test_containers
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
  roundTrip "tuple of 10" (1 :: Int, 'a', True, "b" :: T.Text, 2.5 :: Double, [1 :: Int], Just 'c', (), 'd', -1 :: Int)

test_time :: Assertion
test_time = do
  let noon = LocalTime (fromGregorian 2026 9 25) (TimeOfDay 12 30 5.25)
  assertEqual "day" "2026-09-25\n" (encodeText (fromGregorian 2026 9 25))
  assertEqual "time" "12:30:00\n" (encodeText (TimeOfDay 12 30 0))
  assertEqual "local time" "2026-09-25T12:30:05.25\n" (encodeText noon)
  assertEqual "UTC time" "2026-09-25T12:30:00Z\n" (encodeText (UTCTime (fromGregorian 2026 9 25) (12 * 3600 + 30 * 60)))
  assertEqual "zoned time" "2026-09-25T12:30:05.25-02:30\n" (encodeText (ZonedTime noon (minutesToTimeZone (-150))))
  assertEqual "duration" "1.5\n" (encodeText (1.5 :: NominalDiffTime))
  roundTrip "local time" noon
  roundTrip "UTC time" (UTCTime (fromGregorian (-44) 3 15) 0.000000000001)
  roundTrip "diff time" (picosecondsToDiffTime 123456789)
  assertEqual
    "zoned time with a large offset"
    (Right (noon, 900))
    ( (\z -> (zonedTimeToLocalTime z, timeZoneMinutes (zonedTimeZone z)))
        <$> decodeText (encodeText (ZonedTime noon (minutesToTimeZone 900)))
    )

-- | Encoding a value and decoding the result gives the same value.
roundTrip :: (Eq a, Show a, ToYAML a, FromYAML a) => String -> a -> Assertion
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

test_literal :: Assertion
test_literal = do
  assertEqual "clip" "key: |\n  a\n  b\n" (encodeText (mapping ["key" .= ("a\nb\n" :: T.Text)]))
  assertEqual "strip" "key: |-\n  a\n  b\n" (encodeText (mapping ["key" .= ("a\nb" :: T.Text)]))
  assertEqual "keep" "key: |+\n  a\n\n" (encodeText (mapping ["key" .= ("a\n\n" :: T.Text)]))
  assertEqual "indentation indicator" "- |2-\n    a\n  b\n" (encodeText ["  a\nb" :: T.Text])
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
