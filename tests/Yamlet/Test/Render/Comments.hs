-- | The comments that rendering writes and moves.
module Yamlet.Test.Render.Comments
  ( commentTests
  ) where

import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet.Syntax
import Yamlet.Test.Helpers.Thunks
import Yamlet.Test.Render.Helpers

commentTests :: TestTree
commentTests =
  testGroup
    "comments"
    [ testCase "configuration" test_configuration
    , testCase "round trip" test_commentRoundTrip
    , testCase "several hashes" test_hashes
    , testCase "moved comments" test_movedComments
    , testCase "lines after a list" test_linesAfterList
    , testCase "lines below an indicator" test_linesBelowIndicator
    , testCase "no thunks" test_noThunks
    ]

-- | The parser returns documents with comments without thunks, as it does for
-- documents without comments.
test_noThunks :: Assertion
test_noThunks =
  mapM_
    check
    [ ("configuration", configuration)
    , ("flow collections", "a: [1, # b\n  2] # c\nd: {e: f, # g\n  h: i}\n")
    , ("explicit keys", "? a\n# b\n? c\n: d\n\n# e\n")
    , ("documents", "# a\n--- # b\nc\n...\n# d\n---\ne: 1\n")
    , ("empty quoted keys", "'': a\n? \"\"\n: b\nc: {'': d}\n")
    , ("pairs in flow sequences", "[[]: a, '': b]\n")
    , ("empty nodes", "a:\nb: !t\n? c\nd: {e: , &f : g}\n")
    ]
  where
    check :: (String, T.Text) -> Assertion
    check (preface, input) = case parseDocumentsText input of
      Right docs -> thunks docs >>= assertEqual preface []
      Left e -> assertFailure (preface ++ ": " ++ show e)

-- | The configuration of the haskell-gha test with comments.
test_configuration :: Assertion
test_configuration = case parseDocumentsText configuration of
  Right [doc] ->
    assertEqual
      "comments"
      expected
      (commentsOf doc)
  r -> assertFailure (show r)
  where
    expected :: [(String, String, T.Text)]
    expected =
      [ ("/matrix:key", "before", "The oldest and the newest supported Postgres.")
      , ("/services:key", "before", "The services of each build job.")
      , ("/services/postgres:key", "before", "The database for the tests.")
      , ("/services/postgres/env/POSTGRES_PASSWORD", "inline", "Only for CI.")
      , ("/permissions:key", "before", "The test reporter writes check runs.")
      , ("/permissions/checks:key", "before", "For the annotations of the test results.")
      , ("/hooks:key", "before", "The steps for Postgres.")
      , ("/hooks/before-build/1", "before", "Wait until the database accepts connections.")
      , ("/hooks/before-build", "after", "The database is ready for the build.")
      , ("/hooks/after-build", "after", "The tests come next.")
      ]

configuration :: T.Text
configuration =
  T.unlines
    [ "name: Postgres CI"
    , "branches: [master, 'release/**']"
    , "# The oldest and the newest supported Postgres."
    , "matrix:"
    , "  postgres: ['15', '18']"
    , "  exclude:"
    , "    - ghc: '9.10'"
    , "      postgres: '15'"
    , "apt: [libpq-dev, postgresql-client]"
    , "# The services of each build job."
    , "services:"
    , "  # The database for the tests."
    , "  postgres:"
    , "    image: postgres:${{ matrix.postgres }}"
    , "    env:"
    , "      POSTGRES_PASSWORD: postgres # Only for CI."
    , "    ports: ['5432:5432']"
    , "# The test reporter writes check runs."
    , "permissions:"
    , "  contents: read"
    , "  # For the annotations of the test results."
    , "  checks: write"
    , "# The steps for Postgres."
    , "hooks:"
    , "  before-build:"
    , "    - name: Show the Postgres version"
    , "      run: psql --version"
    , "    # Wait until the database accepts connections."
    , "    - name: Wait for Postgres"
    , "      run: |"
    , "        until pg_isready -h localhost; do"
    , "          sleep 1"
    , "        done"
    , "    # The database is ready for the build."
    , "  after-build:"
    , "    - name: Show the executables"
    , "      run: cabal list-bin all"
    , "    # The tests come next."
    , "ghc-options: -Werror -Wno-unused-imports"
    , "jobs: 2"
    ]

-- | Rendering keeps every comment, and a second round trip gives the same
-- text. The end comments of an indentless sequence belong to its last item
-- after the round trip.
test_commentRoundTrip :: Assertion
test_commentRoundTrip = case parseDocumentsText configuration of
  Right docs -> do
    let out = renderSyntax defaultRenderOptions docs
        texts :: Document -> [T.Text]
        texts d = [t | (_, _, t) <- commentsOf d]
    case parseDocumentsText out of
      Right docs' -> do
        assertEqual
          ("comments\n" ++ T.unpack out)
          (map texts docs)
          (map texts docs')
        assertEqual
          "text"
          out
          (renderSyntax defaultRenderOptions docs')
      Left err -> assertFailure (T.unpack out ++ "\n" ++ show err)
  Left err -> assertFailure (show err)

-- | A comment on a line of its own keeps its # characters. A comment at the
-- end of a line keeps them in its text.
test_hashes :: Assertion
test_hashes = do
  let input = "## a\n### b ###\n####\n# #c\nk: 1 ##d\n  ## e\n"
  assertEqual
    "lines"
    (Right [[CommentLine 2 "a", CommentLine 3 "b ###", CommentLine 4 "", Comment "#c"]])
    $ map
      ( \d -> case d.root.content of
          MappingContent _ ((k, _) : _) -> k.comments.before
          _ -> []
      )
      <$> parseDocumentsText input
  assertEqual
    "inline and below a value"
    (Right [(Just "#d", [CommentLine 2 "e"])])
    $ map
      ( ( \case
            MappingContent _ [(_, v)] -> (v.comments.inline, v.comments.after)
            _ -> (Nothing, [])
        )
          . (.root.content)
      )
      <$> parseDocumentsText input
  assertEqual
    "rendered"
    (Right "## a\n### b ###\n####\n# #c\nk: 1 # #d\n  ## e\n")
    (renderSyntax defaultRenderOptions <$> parseDocumentsText input)
  assertEqual
    "count below 1"
    "# a\n---\nk: 1\n"
    $ renderSyntax
      defaultRenderOptions
      [ (document (mappingNode [(plainNode "k", plainNode "1")]))
          { docComments = noComments {before = [CommentLine (-1) "a"]}
          }
      ]

-- | The lines after a list under a key stay at the end of the list. Without
-- indentation, a block collection as the last item would take them in.
test_linesAfterList :: Assertion
test_linesAfterList = do
  rendersBack "mapping as the last item" "a:\n  - b: 1\n  # c\n"
  rendersBack "list as the last item" "a:\n  - - b\n  # c\n"
  rendersBack "block scalar as the last item" "a:\n  - |\n    b\n  # c\n"
  rendersBack "scalar as the last item" "a:\n- b\n  # c\n"
  rendersBack "no lines after the list" "a:\n- b: 1\n"

-- | The lines above and below the indicator of a block collection with
-- properties stay with their nodes. Above the indicator of a first entry,
-- the collection around it would take the lines up to the last empty line.
test_linesBelowIndicator :: Assertion
test_linesBelowIndicator = do
  let check :: String -> T.Text -> Assertion
      check preface input = case parseDocumentsText input of
        Right docs -> do
          let out = renderSyntax defaultRenderOptions docs
          case parseDocumentsText out of
            Right docs' -> do
              assertEqual
                (preface ++ "\n" ++ T.unpack out)
                (map commentsOf docs)
                (map commentsOf docs')
              assertEqual
                preface
                out
                (renderSyntax defaultRenderOptions docs')
            Left err -> assertFailure (preface ++ ": " ++ show err)
        Left err -> assertFailure (preface ++ ": " ++ show err)
  check "first item with a tag" "k:\n- !!map\n  # a\n\n  # b\n  c: 1\n- d\n"
  check "first item with a comment" "k:\n- &x # a\n  # b\n\n  c: 1\n- d\n"
  check "explicit key" "- ? &x\n    # a\n\n    b: 1\n  : c\n"
  check "nested first items" "- &x\n  - &y\n    # a\n\n    b: 1\n"
  check "second item" "k:\n- a\n- !!map\n  # b\n  c: 1\n"
  check "second item with a comment" "- a\n- &x # a\n  # b\n  c: 1\n"
  let owners :: String -> [(String, String, T.Text)] -> T.Text -> Assertion
      owners preface expected input =
        assertEqual
          preface
          (Right [expected])
          (map commentsOf <$> parseDocumentsText input)
      ownersRenderBack :: String -> [(String, String, T.Text)] -> T.Text -> Assertion
      ownersRenderBack preface expected input = do
        owners
          preface
          expected
          input
        check preface input
  ownersRenderBack
    "below the indicator of a second item"
    [("/jobs/1/name:key", "before", "c")]
    "jobs:\n- name: a\n- &b\n  # c\n  name: b\n"
  -- The lines of the first entries read back the same above the indicator,
  -- where they stay.
  ownersRenderBack
    "above a first item with an anchor"
    [("/jobs/0/name:key", "before", "c")]
    "jobs:\n# c\n- &b\n  name: b\n"
  rendersBack
    "above a first item with an anchor, the text"
    "jobs:\n# c\n- &b\n  name: b\n"
  rendersBack "above nested first items with tags" "# c\n- !a\n  - !b\n    - 2\n"
  rendersBack
    "above a second item with nested first items"
    "- x\n# c\n- !a\n  - !b\n    - 2\n"
  rendersBack "above an explicit key with an anchor" "# c\n? &a\n  - a\n: b\n"
  rendersBack "below a first item with a comment on its line" "- &a # i\n  # c\n  k: v\n"
  -- A comment on the line of the indicator keeps the lines above it from
  -- the first entries.
  rendersBack
    "above a first item with a comment and nested first items"
    "# c\n- # d\n  - !!map\n    k: v\n"
  rendersBack
    "above a second item with a comment and nested first items"
    "- x\n# c\n\n# e\n- # d\n  - &b\n    - y\n"
  rendersBack "above a first item with a comment" "# c\n- # d\n  # f\n  - a\n"
  rendersBack "above a first item with a comment under a key" "k:\n# c\n- # d\n  - a\n"
  rendersBack "above an explicit key with a comment" "# c\n? # d\n  - a\n: v\n"
  rendersBack
    "below an indicator above a first item with a comment"
    "- # a\n  # h\n  - # b\n    - x\n"
  rendersBack
    "below properties above a first item with a comment"
    "- !!seq\n  # h\n  - # b\n    - x\n"
  rendersBack
    "below an indicator above an explicit key with a comment"
    "- # a\n  # h\n  ? # b\n    - x\n  : v\n"
  rendersBack
    "below root properties above a first item with a comment"
    "!!seq\n# h\n- # b\n  - x\n"
  -- A collection on the line of its indicator would take the lines of its
  -- first entry, so it starts below the indicator.
  ownersRenderBack
    "above a nested first item"
    [("/0/0", "before", "c")]
    "# c\n-\n  - 1\n"
  rendersBack "above a nested first item, the text" "# c\n-\n  - 1\n"
  ownersRenderBack
    "above a first key"
    [("/1/a:key", "before", "c")]
    "- x\n# c\n-\n  a: 1\n  b: 2\n"
  rendersBack "above a first key, the text" "- x\n# c\n-\n  a: 1\n  b: 2\n"
  rendersAs
    "above an explicit key"
    "- ?\n    # c\n    - a\n  : v\n"
    "-\n  ?\n    # c\n    - a\n  : v\n"
  ownersRenderBack
    "below the indicator of a scalar"
    [("/1", "before", "c")]
    "- a\n- !!str\n  # c\n  x\n"
  ownersRenderBack
    "below the indicator of an explicit key"
    [("/x:key", "before", "c")]
    "k: a\n? &k\n  # c\n  x\n: v\n"
  ownersRenderBack
    "below the indicator of an explicit value"
    [("/?/b:key", "before", "c")]
    "? a: 1\n: &x\n  # c\n  b: 2\n"
  ownersRenderBack
    "below an indicator with a comment"
    [("/1", "inline", "i"), ("/1/c:key", "before", "b")]
    "- a\n- # i\n  # b\n  c: 1\n"
  -- The renderer writes these items on the line of the indicator, with the
  -- lines above it, so the lines read back as the lines of the item.
  owners
    "below a bare indicator"
    [("/1/name:key", "before", "c")]
    "-\n  name: a\n-\n  # c\n  name: b\n"
  owners
    "below the indicator of a nested list"
    [("/1/0", "before", "c")]
    "- - a\n-\n  # c\n  - x\n"
  let withAbove :: Node -> Node
      withAbove n = n {comments = noComments {before = [Comment "a"]}}
      list :: Node
      list = sequenceNode [plainNode "1"]
  assertEqual
    "lines of a first item below its indicator"
    (Right [[("/0", "before", "a"), ("/0", "inline", "i")]])
    $ map commentsOf
      <$> parseDocumentsText
        ( render $
            sequenceNode
              [ list
                  { comments = noComments {before = [Comment "a"], inline = Just "i"}
                  }
              ]
        )
  let anchored :: Node -> Node
      anchored n = n {props = noProps {anchor = Just "x"}}
  assertEqual
    "lines of a first item with nested first items"
    (Right [[("/0", "before", "a")]])
    $ map commentsOf
      <$> parseDocumentsText
        (render (sequenceNode [withAbove (anchored (sequenceNode [anchored list]))]))
  assertEqual
    "lines of a block value below its key"
    "k:\n# a\n\n- 1\n"
    (render (mappingNode [(plainNode "k", withAbove list)]))
  assertEqual
    "lines of a block value below its key read back"
    (Right [[("/k", "before", "a")]])
    $ map commentsOf
      <$> parseDocumentsText (render (mappingNode [(plainNode "k", withAbove list)]))

-- | A comment without a place at its node moves to one that has it.
test_movedComments :: Assertion
test_movedComments = do
  let withInline :: T.Text -> Node -> Node
      withInline t n = n {comments = n.comments {inline = Just t}}
      withBefore :: T.Text -> Node -> Node
      withBefore t n = n {comments = n.comments {before = [Comment t]}}
      withAfter :: T.Text -> Node -> Node
      withAfter t n = n {comments = n.comments {after = [Comment t]}}
  assertEqual
    "lines above a value on the line of the key"
    "# v\na: 1\n"
    (render (mappingNode [(plainNode "a", withBefore "v" (plainNode "1"))]))
  assertEqual
    "lines after a scalar key"
    "# b\nk: 1\n  # a\n"
    . render
    $ mappingNode [(withAfter "a" (withBefore "b" (plainNode "k")), plainNode "1")]
  assertEqual
    "lines after a scalar key with a block scalar value"
    "# a\nk: |\n  text\n"
    . render
    $ mappingNode
      [
        ( withAfter "a" (plainNode "k")
        , contentNode (ScalarContent Literal "text\n")
        )
      ]
  assertEqual
    "lines after a block scalar value"
    "k: |\n  text\n# a\nx: 1\n"
    . render
    $ mappingNode
      [
        ( plainNode "k"
        , withAfter "a" (contentNode (ScalarContent Literal "text\n"))
        )
      , (plainNode "x", plainNode "1")
      ]
  assertEqual
    "lines after a list item"
    "- 1\n  # a\n- 2\n"
    . render
    . contentNode
    $ SequenceContent Block [withAfter "a" (plainNode "1"), plainNode "2"]
  assertEqual
    "empty lines at the end of an empty flow collection"
    "a: [\n  # c\n  ]\n\nb: 1\n"
    . render
    $ mappingNode
      [
        ( plainNode "a"
        , (contentNode (SequenceContent Flow []))
            { comments = noComments {after = [Comment "c", EmptyLine]}
            }
        )
      , (plainNode "b", plainNode "1")
      ]
  assertEqual
    "two comments on one line"
    "# k\na: 1 # v\n"
    . render
    $ mappingNode [(withInline "k" (plainNode "a"), withInline "v" (plainNode "1"))]
  assertEqual
    "two comments on one line, one with a line break"
    "# k l\na: 1 # v\n"
    . render
    $ mappingNode
      [(withInline "k\nl" (plainNode "a"), withInline "v" (plainNode "1"))]
  assertEqual
    "comment in a flow sequence"
    "a:\n- 1 # c\n- 2\n"
    . render
    $ mappingNode
      [
        ( plainNode "a"
        , contentNode
            (SequenceContent Flow [withInline "c" (plainNode "1"), plainNode "2"])
        )
      ]
  assertEqual
    "YAML 1.1 line breaks in comments"
    "# a\n# b\n# c\n# d\nk: v # e f g h\n"
    . render
    $ mappingNode
      [
        ( withBefore "a\x85\&b\x2028\&c\x2029\&d" (plainNode "k")
        , withInline "e\x85\&f\x2028\&g\x2029\&h" (plainNode "v")
        )
      ]
  assertEqual
    "comment on a block root"
    "--- # c\na: 1\n"
    (render (withInline "c" (mappingNode [(plainNode "a", plainNode "1")])))
  rendersBack "comment on the properties of a block root" "# r\n!!map # c\n# f\na: 1\n"
  rendersBack
    "comment on the properties of a block root below a marker with a comment"
    "--- # d\n&a # c\n- x\n"
  rendersAs
    "lines below the properties of a block root with a comment"
    "# e\n\n!!map # c\n# f\na: 1\n"
    "!!map # c\n# e\n\n# f\na: 1\n"
  let rootWithLines = withBefore "r" (mappingNode [(plainNode "a", plainNode "1")])
  assertEqual
    "lines of a block root without an empty line"
    "# r\n\na: 1\n"
    (render rootWithLines)
  assertEqual
    "lines of a block root read back"
    (Right [[Comment "r", EmptyLine]])
    (map (\d -> d.root.comments.before) <$> parseDocumentsText (render rootWithLines))
  assertEqual
    "comment on a block root below the comment of the marker"
    "--- # d\n# c\n\na: 1\n"
    $ renderSyntax
      defaultRenderOptions
      [ (document (withInline "c" (mappingNode [(plainNode "a", plainNode "1")])))
          { docComments = noComments {inline = Just "d"}
          }
      ]
  let list :: Node -> Node
      list item =
        mappingNode
          [ (plainNode "a", withAfter "c" (sequenceNode [item]))
          , (plainNode "b", plainNode "2")
          ]
  assertEqual
    "end of a list with a mapping"
    "a:\n  - x: 1\n  # c\nb: 2\n"
    (render (list (mappingNode [(plainNode "x", plainNode "1")])))
  assertEqual
    "end of a list with a block scalar"
    "a:\n  - |\n    x\n  # c\nb: 2\n"
    (render (list (scalarNode Literal "x\n")))
