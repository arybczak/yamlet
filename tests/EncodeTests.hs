module EncodeTests (encodeTests) where

import Data.List qualified as L
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet

encodeTests :: TestTree
encodeTests = testGroup "Encode"
  [ testCase "block style" test_blockStyle
  , testCase "quoting" test_quoting
  , testCase "floats" test_floats
  , testCase "literal block scalars" test_literal
  , testCase "tags" test_tags
  , testProperty "round trip" prop_roundTrip
  ]

test_blockStyle :: Assertion
test_blockStyle = assertEqual "output" expected (encodeText value)
  where
    value :: Node
    value = mapping
      [ "source_paths" .= ["." :: T.Text]
      , "exclude_paths" .= ["dist" :: T.Text, "dist-newstyle"]
      , "language" .= ("Haskell2010" :: T.Text)
      , "nested" .= mapping ["a" .= (1 :: Int), "b" .= [[True, False]]]
      , "records" .= [mapping ["x" .= (1.5 :: Double), "y" .= ()]]
      , "empty_list" .= ([] :: [Int])
      , "empty_map" .= mapping []
      ]

    expected :: T.Text
    expected = T.unlines
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
  assertEqual "exact scientific" "1.2345678901234567890123e19\n"
    (encodeText (Sci.scientific 12345678901234567890123 (-3)))
  assertEqual "infinity" "-.inf\n" (encodeText (-1 / 0 :: Double))
  assertEqual "not a number" ".nan\n" (encodeText (0 / 0 :: Double))

test_literal :: Assertion
test_literal = do
  assertEqual "clip" "key: |\n  a\n  b\n" (encodeText (mapping ["key" .= ("a\nb\n" :: T.Text)]))
  assertEqual "strip" "key: |-\n  a\n  b\n" (encodeText (mapping ["key" .= ("a\nb" :: T.Text)]))
  assertEqual "keep" "key: |+\n  a\n\n" (encodeText (mapping ["key" .= ("a\n\n" :: T.Text)]))
  assertEqual "indentation indicator" "- |2-\n    a\n  b\n" (encodeText ["  a\nb" :: T.Text])

test_tags :: Assertion
test_tags = do
  let local = Node noOffset "!point" (Mapping ["x" .= (1 :: Int)])
  assertEqual "local tag" "!point\nx: 1\n" (encodeText local)
  let str = Node noOffset "!name" (String "foo")
  assertEqual "tagged scalar" "- !name foo\n" (encodeText [str])

-- | Encoding a node and decoding the result gives the same node.
prop_roundTrip :: Doc -> Property
prop_roundTrip (Doc n) = case decodeNodes (encodeText n) of
  Right [n'] -> counterexample (T.unpack (encodeText n)) $ strip n' === strip n
  r -> counterexample (T.unpack (encodeText n) ++ "\n" ++ show r) False
  where
    -- Drop the offsets.
    strip :: Node -> Node
    strip x = Node noOffset x.tag $ case x.value of
      Sequence xs -> Sequence (map strip xs)
      Mapping kvs -> Mapping [ (strip k, strip v) | (k, v) <- kvs ]
      v -> v

newtype Doc = Doc Node
  deriving stock Show

instance Arbitrary Doc where
  arbitrary = Doc <$> sized genNode

genNode :: Int -> Gen Node
genNode size
  | size <= 1 = genScalar
  | otherwise = frequency
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
      String _ -> n { tag = "!custom" }
      _ -> n

genScalar :: Gen Node
genScalar = node <$> oneof
  [ pure Null
  , Bool <$> arbitrary
  , Int <$> arbitrary
  , Float <$> elements (map Finite [0, 1.5, -2.25e-10, 123456.789, 1e30, 12] ++ [Infinity, NegativeInfinity, NaN])
  , String <$> genText
  ]

genText :: Gen T.Text
genText = oneof
  [ elements tricky
  , T.pack <$> listOf genChar
  , T.intercalate "\n" <$> listOf (T.pack <$> listOf genChar)
  ]
  where
    tricky :: [T.Text]
    tricky =
      [ "", " ", "-", "- a", "? a", ": a", "a: b", "a:b", "#", "a #b", "true", "null"
      , "1", "0x1F", "0o7", ".5", "~", "---", "...", "@x", "`x", "foo\n", "\nfoo"
      , "  lead", "trail  ", "a\n\nb\n\n", "\t", "é", "\x85", "\x2028", "\xFEFF"
      , "\n", "\n\n", " \n", "a\n ", "|", ">", "%x", "&a", "*a", "!a", "{}", "[]"
      , "a, b", "key:", "'quoted'", "\"dq\"", "\r\n", "\\", "a\tb"
      ]

    genChar :: Gen Char
    genChar = frequency
      [ (10, elements "abc xyz-:#,[]{}'\"!&*?|>%@`\\")
      , (2, elements "\t\r\x85\xA0\x2028\xFEFF\x01\x7F")
      , (1, arbitrary)
      ]
