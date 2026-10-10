-- | The rules that attach comment lines to the nodes.
module Yamlet.Test.Render.Attachment
  ( test_attachment
  ) where

import Data.Text qualified as T
import Test.Tasty.HUnit

import Yamlet.Syntax
import Yamlet.Test.Render.Helpers

-- | Each rule of the documentation.
test_attachment :: Assertion
test_attachment = do
  let check :: String -> [(String, String, T.Text)] -> T.Text -> Assertion
      check preface expected input = case parseDocumentsText input of
        Right [doc] ->
          assertEqual
            preface
            expected
            (commentsOf doc)
        r -> assertFailure (preface ++ ": " ++ show r)
  check
    "above a key"
    [("/b:key", "before", "c")]
    "a: 1\n# c\nb: 2\n"
  check
    "above the first key"
    [("/a:key", "before", "c")]
    "# c\na: 1\n"
  check
    "above the first key of a value"
    [("/a/b:key", "before", "c")]
    "a:\n  # c\n  b: 1\n"
  check
    "above an empty line above the first key"
    [("", "before", "c"), ("/a:key", "before", "d")]
    "# c\n\n# d\na: 1\n"
  check
    "above the first key after an indicator"
    [("/0", "inline", "c"), ("/0/a:key", "before", "d")]
    "- # c\n  # d\n  a: 1\n"
  check
    "above an item with a mapping"
    [("/0", "before", "c"), ("/1", "before", "d")]
    "# c\n- a: 1\n# d\n- b: 2\n"
  check
    "at the end of a value"
    [("/a", "inline", "c")]
    "a: 1 # c\n"
  check
    "at the end of a key"
    [("/a:key", "inline", "c")]
    "a: # c\n  b: 1\n"
  check
    "on a block scalar header"
    [("/a", "inline", "c")]
    "a: | # c\n  text\n"
  check
    "after a block scalar in a key"
    [("/?:key/0", "inline", "c"), ("/?", "inline", "d")]
    "? - | # c\n    text\n: # d\n  - x\n"
  check
    "after a kept block scalar in a key"
    [("/?", "inline", "c")]
    "? - |+\n    text\n\n: # c\n  k: v\n"
  check
    "above a sequence item"
    [("/a/1", "before", "c")]
    "a:\n- 1\n# c\n- 2\n"
  check
    "after a flow collection"
    [("/a", "inline", "c")]
    "a: [1, 2] # c\n"
  check
    "on an item line"
    [("/0", "inline", "c")]
    "- # c\n  a: 1\n"
  check
    "on an item line and after the item"
    [("/0", "before", "c"), ("/0", "inline", "d")]
    "- # c\n  a # d\n"
  check
    "on a bracket line and after the item"
    [("/a/0", "before", "c"), ("/a/0", "inline", "d")]
    "a: [ # c\n  1, # d\n  2]\n"
  check
    "on an item line and on a block scalar header"
    [("/0", "before", "c"), ("/0", "inline", "d")]
    "- # c\n  | # d\n  text\n"
  check
    "at the end of an indented list"
    [("/a", "after", "c")]
    "a:\n  - 1\n  # c\nb: 2\n"
  check
    "at the key column after a list"
    [("/b:key", "before", "c")]
    "a:\n- 1\n# c\nb: 2\n"
  check
    "at the end of a nested mapping"
    [("/a", "after", "c")]
    "a:\n  b: 1\n  # c\nd: 2\n"
  check
    "at the end of the root"
    [("", "after", "c")]
    "a: 1\n# c\n"
  check
    "after an empty line at the end of the root"
    [("", "after", "c"), ("", "after", "d")]
    "a: 1\n# c\n\n# d\n"
  check
    "at the end of a scalar root"
    [("", "after", "c")]
    "a\n# c\n"
  check
    "below a flow root"
    [("document", "after", "c")]
    "[a]\n# c\n"
  check
    "before the end marker"
    [("document", "after", "d"), ("", "after", "c")]
    "a\n# c\n...\n# d\n"
  check
    "below a flow root and the end marker"
    [("document", "after", "c"), ("document", "after", "d")]
    "[a]\n# c\n...\n# d\n"
  check
    "before the marker"
    [("document", "before", "c")]
    "# c\n---\na: 1\n"
  check
    "on the marker line"
    [("document", "inline", "c")]
    "--- # c\na: 1\n"
  check
    "after a root on the marker line"
    [("", "inline", "c")]
    "--- a # c\n"
  check
    "after a tag on the marker line"
    [("document", "inline", "c")]
    "--- !!map # c\na: 1\n"
  check
    "after the end marker"
    [("document", "after", "c")]
    "a\n...\n# c\n"
  check
    "after the end marker of a mapping"
    [("document", "after", "c")]
    "a: 1\n...\n# c\n"
  check
    "on the end marker line"
    [("document", "after", "c")]
    "a\n... # c\n"
  check
    "after two end markers"
    [("document", "after", "c"), ("document", "after", "d")]
    "a\n...\n# c\n...\n# d\n"
  check
    "on a second end marker line"
    [("document", "after", "c")]
    "a\n...\n... # c\n"
  check
    "after an empty flow sequence with lines inside"
    [("/0", "inline", "d"), ("/0", "after", "c")]
    "- [\n  # c\n  ] # d\n- 2\n"
  check
    "after a flow mapping with lines inside"
    [("/k", "inline", "d"), ("/k", "after", "c")]
    "k: {a: 1,\n  # c\n  } # d\n"
  check
    "inside a flow sequence"
    [("/0", "inline", "c"), ("/1", "before", "d")]
    "[a, # c\n # d\n b]\n"
  check
    "below an explicit key without a value"
    [("/b:key", "before", "c")]
    "? a\n# c\n? b\n"
  check
    "at the end of a list item with an explicit key"
    [("/0", "after", "c")]
    "- ? a\n  # c\n- b\n"
  check
    "at the end of a mapping with an explicit key"
    [("/x", "after", "c")]
    "x:\n  ? a\n  # c\ny: 1\n"
  check
    "empty lines"
    []
    "a: 1\n\n\nb: 2\n"
  assertEqual
    "empty lines"
    (Right [EmptyLine, EmptyLine])
    $ ( \case
          [d] | MappingContent _ [_, (k, _)] <- d.root.content -> k.comments.before
          _ -> []
      )
      <$> parseDocumentsText "a: 1\n\n\nb: 2\n"
  assertEqual
    "empty line below the end of a collection"
    (Right [([Comment "c"], [EmptyLine])])
    $ map
      ( \d -> case d.root.content of
          MappingContent _ [(_, v), (k, _)] -> (v.comments.after, k.comments.before)
          _ -> ([], [])
      )
      <$> parseDocumentsText "a:\n  b: 1\n  # c\n\nd: 2\n"
  assertEqual
    "empty line below an explicit key without a value"
    (Right "a:\n\n# c\nb:\n")
    (renderSyntax defaultRenderOptions <$> parseDocumentsText "? a\n\n# c\n? b\n")
  assertEqual
    "empty line at the end of the root"
    (Right [([Comment "c"], [EmptyLine, Comment "d"], [])])
    $ map
      ( \d -> case d.root.content of
          MappingContent _ [(_, v)] ->
            (v.comments.after, d.root.comments.after, d.docComments.after)
          _ -> ([], [], [])
      )
      <$> parseDocumentsText "a:\n  b: 1\n  # c\n\n# d\n"
  let between :: String -> [([Line], [Line])] -> T.Text -> Assertion
      between preface expected input =
        assertEqual
          preface
          (Right expected)
          $ map
            ( \d ->
                ( d.root.comments.after ++ d.docComments.after
                , d.docComments.before ++ d.root.comments.before
                )
            )
            <$> parseDocumentsText input
  between
    "above the marker of the next document"
    [([Comment "c"], []), ([], [])]
    "a\n# c\n---\nb\n"
  between
    "empty line above the marker of the next document"
    [([Comment "c"], []), ([], [EmptyLine, Comment "d"])]
    "a\n# c\n\n# d\n---\nb\n"
  between
    "empty line after the end marker"
    [([Comment "c"], []), ([], [EmptyLine, Comment "d"])]
    "a\n...\n# c\n\n# d\n---\nb\n"
  between
    "empty line after the end marker above a bare document"
    [([Comment "c"], []), ([], [EmptyLine, Comment "d"])]
    "a\n...\n# c\n\n# d\nb\n"
  between
    "empty line below a flow root"
    [([Comment "c"], []), ([], [EmptyLine, Comment "d"])]
    "[a]\n# c\n\n# d\n---\nb\n"
  between
    "empty line below a flow root above the last end marker"
    [([Comment "c", EmptyLine], [])]
    "[a]\n# c\n\n...\n\n"
  assertEqual
    "empty lines above the first key"
    (Right [([Comment "a", EmptyLine, Comment "b", EmptyLine, EmptyLine], [Comment "c"])])
    $ map (\d -> (d.root.comments.before, firstKey d.root))
      <$> parseDocumentsText "# a\n\n# b\n\n\n# c\nk: v\n"
  where
    firstKey :: Node -> [Line]
    firstKey n = case n.content of
      MappingContent _ ((k, _) : _) -> k.comments.before
      _ -> []
