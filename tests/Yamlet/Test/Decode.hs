module Yamlet.Test.Decode (decodeTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet.Test.Decode.Errors
import Yamlet.Test.Decode.Input
import Yamlet.Test.Decode.Limits
import Yamlet.Test.Decode.Scalars
import Yamlet.Test.Decode.SyntaxErrors
import Yamlet.Test.Decode.Values
import Yamlet.Test.Helpers

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
    , slow $ testCase "time" test_time
    , testCase "copies" test_copies
    , testCase "JSON" test_json
    , testCase "aliases" test_aliases
    , slow $ testCase "nesting" test_nesting
    , slow $ testCase "many keys" test_manyKeys
    , slow $ testCase "nested duplicates" test_nestedDuplicates
    , slow $ testCase "alias keys" test_aliasKeys
    , slow $ testCase "alias limit" test_aliasLimit
    , testCase "tag prefix limit" test_tagPrefixLimit
    , slow $ testCase "long numbers" test_longNumbers
    , -- 0.2 s with the check of the lengths, 8 s without it.
      localOption (mkTimeout 2000000) $
        testCase "long unknown names" test_longUnknownNames
    , testCase "optional keys" test_optionalKeys
    , testCase "located values" test_located
    , testCase "syntax tree" test_syntaxTree
    , testCase "empty stream" test_emptyStream
    , testCase "encodings" test_encodings
    , testCase "byte order marks" test_byteOrderMarks
    , testCase "files" test_files
    , testCase "no thunks" test_noThunks
    , testGroup
        "errors"
        [ testCase "syntax" test_syntaxErrors
        , testCase "directives and tags" test_directiveErrors
        , testCase "types" test_typeErrors
        , testCase "keys" test_keyErrors
        , testCase "collected" test_collectedErrors
        , testCase "pretty" test_prettyError
        , testCase "paths" test_errorPaths
        , testProperty "locations of several errors" prop_errorsAt
        , testCase "paths of several errors" test_nodePaths
        , slow $ testCase "many errors" test_manyErrors
        , slow $ testCase "deep errors" test_deepErrors
        ]
    ]
