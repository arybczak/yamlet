-- | The encodings of syntax trees, kept nodes and comments.
module Yamlet.Test.Encode.Comments
  ( test_syntax
  , test_keptNodes
  , test_commentedKeys
  ) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Tasty.HUnit

import Yamlet
import Yamlet.Syntax qualified as S
import Yamlet.Test.Helpers

test_syntax :: Assertion
test_syntax =
  assertEqual
    "output"
    expected
    (S.renderSyntax S.defaultRenderOptions [S.document (edit value)])
  where
    value :: S.Node
    value = mapping ["name" .= ("x" :: T.Text), "paths" .= ["a" :: T.Text, "b"]]

    -- Add a comment above the first key and use the flow style for the list.
    edit :: S.Node -> S.Node
    edit n = case n.content of
      S.MappingContent style [(k1, v1), (k2, v2)] ->
        n
          { S.content =
              S.MappingContent
                style
                [
                  ( k1 {S.comments = S.noComments {S.before = [S.Comment "The name."]}}
                  , v1
                  )
                , (k2, v2 {S.content = flow v2.content})
                ]
          }
      _ -> n

    flow :: S.Content -> S.Content
    flow = \case
      S.SequenceContent _ xs -> S.SequenceContent S.Flow xs
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
      assertEqual
        "decoded field"
        "demo"
        w.name
      assertEqual
        "output"
        expected
        (encodeText (Workflow w.name 8 w.matrix))
  assertEqual
    "kept nodes in a list"
    (Right "- ['9.10', \"9.12\"] # versions\n- {a: 1}\n")
    (encodeText <$> decodeText @[Node] "- ['9.10', \"9.12\"] # versions\n- {a: 1}\n")
  assertEqual
    "comments inside an alias"
    (Right "a: &x\n  k: v # c1\nb: # c2\n  k: v\n")
    (encodeText <$> decodeText @Node "a: &x\n  k: v # c1\nb: *x # c2\n")
  -- The comment belongs to the mapping, and a map has no place for it.
  assertEqual
    "comment after the tag of a mapping"
    (Right "a: 1\n")
    (encodeText <$> decodeText @(M.Map T.Text (Commented Node)) "!!map # c1\na: 1\n")
  assertEqual
    "lines above the first key of a mapping"
    (Right (M.fromList [("b", [Comment "c2"])]))
    $ M.map (.comments.before)
      <$> decodeText @(M.Map T.Text (Commented Int)) "# c1\n\n# c2\nb: 1\n"
  assertEqual
    "comment after the tag of a list"
    (Right "- 1\n")
    (encodeText <$> decodeText @[Commented Node] "!!seq # c1\n- 1\n")
  let valueLines = "# c1\nk:\n  # c2\n\n  # c3\n  - 1\n"
  assertEqual
    "lines above the first item of a value"
    (Right "# c1\nk:\n# c2\n\n# c3\n- 1\n")
    $ encodeText
      <$> decodeText @(M.Map T.Text (Commented (Commented [Commented Int]))) valueLines
  -- The lines belong to the list, and the comments of the entry have no place
  -- for them.
  assertEqual
    "lines above the first item of a value without a commented list"
    (Right "# c1\nk:\n# c3\n- 1\n")
    (encodeText <$> decodeText @(M.Map T.Text (Commented [Commented Int])) valueLines)
  let scalarRoot = "|\n  text\n# end\n"
  assertEqual
    "lines at the end of a scalar root"
    (Right scalarRoot)
    (encodeText <$> decodeText @Node scalarRoot)
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
  let job =
        T.unlines
          [ "name: build"
          , "# The test reporter writes check runs."
          , "permissions: # read-only"
          , "  contents: read"
          ]
  assertEqual
    "record"
    (Right job)
    (encodeText <$> decodeText @Job job)
  assertEqual
    "map"
    (Right "# one\na: 1\nb: 2 # two\n")
    $ encodeText
      <$> decodeText @(M.Map T.Text (Commented Int)) "# one\na: 1\nb: 2 # two\n"
  -- The parser gives the lines before the marker and at the end to the
  -- document, and the decoder gives them to the root. The renderer separates
  -- the lines of a block root from its first entry, so that they read back
  -- as the lines of the root.
  let top = "# top\n\na: 1\n# end\n"
  assertEqual
    "comments of the document"
    (Right top)
    $ encodeText
      <$> decodeText @(Commented (M.Map T.Text Int)) "# top\n---\na: 1\n# end\n"
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
  assertEqual
    "set"
    (Right set)
    (encodeText <$> decodeText @(Set.Set (Commented T.Text)) set)
  -- The comment after 1 belongs to the value, which an integer cannot keep.
  assertEqual
    "keys of a map"
    (Right "# above\na: 1\n")
    (encodeText <$> decodeText @(M.Map (Commented T.Text) Int) "# above\na: 1 # c\n")
  -- Both take the comments of the key, and the encoder writes them once.
  assertEqual
    "keys and values of a map"
    (Right "# above\na: 1 # c\n")
    $ encodeText
      <$> decodeText @(M.Map (Commented T.Text) (Commented Int)) "# above\na: 1 # c\n"
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
  let branches =
        "branches: # which branches\n# the main branch\n- main # the old default\n- dev\n  # more later\n"
  assertEqual
    "commented items"
    (Right branches)
    (encodeText <$> decodeText @(M.Map T.Text (Commented [Commented T.Text])) branches)
  let commentedRoot = "# c1\n1 # c2\n# c3\n"
  assertEqual
    "commented scalar root"
    (Right commentedRoot)
    (encodeText <$> decodeText @(Commented Int) commentedRoot)
  let linesAfter =
        M.fromList @T.Text @(Commented Int)
          [ ("a", Commented 1 noComments {after = [Comment "c"]})
          , ("b", Commented 2 noComments)
          ]
  assertEqual
    "lines after a commented scalar value"
    "a: 1\n  # c\nb: 2\n"
    (encodeText linesAfter)
  assertEqual
    "lines after a commented scalar value read back"
    (Right (M.map (.comments) linesAfter))
    $ M.map (.comments)
      <$> decodeText @(M.Map T.Text (Commented Int)) (encodeText linesAfter)
  let quotedLinesAfter = "a: \"x\\r\\ny\"\n  # c\nb: d\n"
  assertEqual
    "lines after a text of several lines in double quotes"
    (Right quotedLinesAfter)
    (encodeText <$> decodeText @(M.Map T.Text (Commented T.Text)) quotedLinesAfter)
  let linesAbove =
        [ Commented @[Int]
            [1, 2]
            noComments {before = [Comment "above"], inline = Just "inline"}
        ]
  assertEqual
    "lines above a commented list item"
    "# above\n- # inline\n  - 1\n  - 2\n"
    (encodeText linesAbove)
  assertEqual
    "lines above a commented list item read back"
    (Right [noComments {before = [Comment "above"], inline = Just "inline"}])
    (map (.comments) <$> decodeText @[Commented [Int]] (encodeText linesAbove))
  -- A collection on the line of its indicator takes the lines above the
  -- indicator, so it starts below the indicator to leave them to its first
  -- entry.
  let above = noComments {before = [Comment "c"]}
      nestedItem = [[Commented @Int 1 above]]
      firstKey =
        [ M.fromList @T.Text
            [("a", Commented @Int 1 above), ("b", Commented 2 noComments)]
        ]
  assertEqual
    "lines above a nested first item"
    "# c\n-\n  - 1\n"
    (encodeText nestedItem)
  roundTrip "lines above a nested first item read back" nestedItem
  assertEqual
    "lines above a first key"
    "# c\n-\n  a: 1\n  b: 2\n"
    (encodeText firstKey)
  roundTrip "lines above a first key read back" firstKey
