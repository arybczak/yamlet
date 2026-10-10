module Yamlet.Test.Encode (encodeTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet.Test.Encode.Comments
import Yamlet.Test.Encode.Properties
import Yamlet.Test.Encode.Values
import Yamlet.Test.Helpers

encodeTests :: TestTree
encodeTests =
  testGroup
    "Encode"
    [ testCase "block style" test_blockStyle
    , testCase "quoting" test_quoting
    , testCase "floats" test_floats
    , testProperty "float format" prop_floatFormat
    , slow $ testCase "long floats" test_longFloats
    , slow $ testCase "long strings like numbers" test_longNumberLikeStrings
    , testCase "literal block scalars" test_literal
    , testCase "tags" test_tags
    , testCase "syntax tree" test_syntax
    , testCase "kept nodes" test_keptNodes
    , testCase "comments of keys" test_commentedKeys
    , -- The renderers differ only in rare cases, e.g. for a key that needs an
      -- explicit entry. 10000 cases take about 0.2 s.
      localOption (QuickCheckTests 10000) $ testProperty "fast renderer" prop_fastRenderer
    , testProperty "fast renderer of several documents" prop_fastRendererAll
    , localOption (QuickCheckTests 10000) $
        testProperty "fast renderer of syntax trees" prop_fastRendererNodes
    , testCase "containers" test_containers
    , testCase "base" test_base
    , testCase "time" test_time
    , testProperty "round trip" prop_roundTrip
    , testProperty "syntax round trip" prop_syntaxRoundTrip
    ]
