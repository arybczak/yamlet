module Yamlet.Test.Render (renderTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet.Test.Helpers
import Yamlet.Test.Render.Attachment
import Yamlet.Test.Render.Comments
import Yamlet.Test.Render.Documents
import Yamlet.Test.Render.Properties
import Yamlet.Test.Render.Styles

renderTests :: TestTree
renderTests =
  testGroup
    "Render"
    [ testCase "workflow" test_workflow
    , testCase "styles" test_styles
    , testCase "fallbacks" test_fallbacks
    , testCase "force block" test_forceBlock
    , testCase "documents" test_documents
    , testCase "lines of scalars" test_scalarLines
    , slow $ testCase "many invalid anchor names" test_manyAnchors
    , slow $ testCase "deep comment" test_deepComment
    , slow $ testCase "nested keys" test_nestedKeys
    , slow $ testCase "comments above nesting" test_commentsAboveNesting
    , slow $ testCase "many escaped line breaks" test_escapedBreaks
    , testGroup
        "comments"
        [ testCase "attachment" test_attachment
        , testCase "configuration" test_configuration
        , testCase "round trip" test_commentRoundTrip
        , testCase "several hashes" test_hashes
        , testCase "moved comments" test_movedComments
        , testCase "lines after a list" test_linesAfterList
        , testCase "lines below an indicator" test_linesBelowIndicator
        , testCase "no thunks" test_noThunks
        , testProperty "no thunks in generated documents" prop_noThunks
        ]
    , testProperty "round trip" prop_roundTrip
    ]
