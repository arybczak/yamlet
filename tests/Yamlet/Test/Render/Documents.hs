-- | The markers, directives and comments between the documents of a stream.
module Yamlet.Test.Render.Documents
  ( documentTests
  ) where

import Control.Monad
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet.Syntax
import Yamlet.Test.Render.Helpers

documentTests :: TestTree
documentTests = testCase "documents" test_documents

test_documents :: Assertion
test_documents = do
  rendersBack "markers" $
    T.unlines
      [ "first"
      , "---"
      , "second"
      , "..."
      , "%YAML 1.2"
      , "---"
      , "a: b"
      , "---"
      ]
  -- YAML 1.1 parsers need a start marker after an end marker.
  rendersAs
    "document after an end marker"
    "a\n...\n---\nb\n"
    "a\n...\nb\n"
  rendersAs
    "comment after the end marker"
    "a: b\n...\n# c\n---\nd: e\n"
    "a: b\n...\n# c\nd: e\n"
  rendersBack "comment before the directives" "a\n...\n# b\n%YAML 1.2\n---\nc\n"
  rendersBack
    "comments at the end of a root collection and a document"
    "a: 1\n# b\n\n# c\n...\n"
  rendersBack
    "comments around an end marker between documents"
    "a\n# b\n...\n# c\n---\nd\n"
  rendersAs
    "empty line before the bracket of a flow root"
    "key: value\n# zq\n...\n"
    "{\n key: value\n # zq\n\n}\n...\n"
  rendersAs
    "empty line before the bracket of a flow value"
    "a:\n  key: value\n  # zq\n\nb: 1\n"
    "a: {\n key: value\n # zq\n\n }\nb: 1\n"
  rendersAs
    "empty line before the bracket and a comment after it"
    "a: {key: value} # c\n\nb: 1\n"
    "a: {\n key: value\n\n } # c\nb: 1\n"
  assertEqual
    "comment after a byte order mark between documents"
    (Right [[("document", "after", "c")], [("document", "before", "d")]])
    $ map commentsOf
      <$> parseDocumentsText "a: 1\n...\n\xFEFF# c\n\n\xFEFF# d\n---\nb: 2\n"
  let commented :: Document -> Document
      commented d = d {docComments = noComments {before = [Comment "c"]}}
  assertEqual
    "comment above a document without an end marker above it"
    "a\n\n# c\n---\nb\n"
    $ renderSyntax
      defaultRenderOptions
      [document (plainNode "a"), commented (document (plainNode "b"))]
  assertEqual
    "comment above a document with directives"
    "a\n...\n\n# c\n%YAML 1.2\n---\nb\n"
    $ renderSyntax
      defaultRenderOptions
      [ document (plainNode "a")
      , commented (document (plainNode "b")) {version = Just (YamlVersion 1 2)}
      ]
  let flowWithLines =
        (document (contentNode (SequenceContent Flow [plainNode "a"])))
          { docComments = noComments {after = [Comment "c"]}
          }
      beforeDirectives =
        renderSyntax
          defaultRenderOptions
          [flowWithLines, (document (plainNode "b")) {version = Just (YamlVersion 1 2)}]
  assertEqual
    "lines of a flow root above directives"
    "[a]\n...\n# c\n%YAML 1.2\n---\nb\n"
    beforeDirectives
  rendersBack "lines of a flow root above directives, rendered again" beforeDirectives
  -- A block scalar without content would take the comment in.
  forM_ [Literal, Folded] $ \style ->
    assertEqual
      ("comment above a document below an empty " ++ show style ++ " root")
      "\"\"\n\n# c\n---\nb\n"
      $ renderSyntax
        defaultRenderOptions
        [document (scalarNode style ""), commented (document (plainNode "b"))]
  let keyComment :: Document
      keyComment =
        document $
          mappingNode
            [
              ( (plainNode "k") {comments = noComments {before = [Comment "c"]}}
              , plainNode "v"
              )
            ]
      afterEnd :: T.Text
      afterEnd =
        renderSyntax
          defaultRenderOptions
          [(document (plainNode "a")) {explicitEnd = True}, keyComment]
      firstKeyLines :: Document -> [Line]
      firstKeyLines d = case d.root.content of
        MappingContent _ ((k, _) : _) -> k.comments.before
        _ -> []
  assertEqual
    "comment above the first key after an end marker"
    "a\n...\n---\n# c\nk: v\n"
    afterEnd
  assertEqual
    "comment above the first key after an end marker, read back"
    (Right [[], [Comment "c"]])
    (map firstKeyLines <$> parseDocumentsText afterEnd)
  let rootWithGap :: Bool -> Document
      rootWithGap end =
        ( document
            (contentNode (SequenceContent Block [plainNode "a"]))
              { comments = noComments {after = [Comment "c", EmptyLine]}
              }
        )
          { explicitEnd = end
          }
  assertEqual
    "empty line at the end of a block root before an end marker"
    "- a\n# c\n\n...\n"
    (renderSyntax defaultRenderOptions [rootWithGap True])
  assertEqual
    "empty line at the end of a block root before a document"
    "- a\n# c\n\n---\nb\n"
    (renderSyntax defaultRenderOptions [rootWithGap False, document (plainNode "b")])
  -- The end marker keeps the lines of a document from the next document.
  rendersBack
    "empty line below a flow root above an end marker"
    "[a]\n\n# c\n...\n---\nx\n"
  rendersBack
    "empty line at the end of a flow root above an end marker"
    "[a]\n# c\n\n...\n---\nx\n"
  rendersAs
    "empty line at the end of a flow root in the block style"
    "key: value\n\n# c\n...\n---\nx\n"
    "{\n key: value\n\n# c\n}\n---\nx\n"
  let boundary
        :: String -> T.Text -> [[(String, String, T.Text)]] -> [Document] -> Assertion
      boundary preface expected comments docs = do
        let rendered = renderSyntax defaultRenderOptions docs
        assertEqual
          preface
          expected
          rendered
        assertEqual
          (preface ++ ", read back")
          (Right comments)
          (map commentsOf <$> parseDocumentsText rendered)
      withLines :: Comments -> Node -> Node
      withLines c n = n {comments = c}
  boundary
    "empty line at the end of a block root before a document"
    "k: v\n\n# c\n...\n---\nb\n"
    [[("", "after", "c")], []]
    [ document $
        withLines
          noComments {after = [EmptyLine, Comment "c"]}
          (mappingNode [(plainNode "k", plainNode "v")])
    , document (plainNode "b")
    ]
  boundary
    "empty line at the end of the last value of a block root before a document"
    "k: v\n\n# c\n...\n---\nb\n"
    [[("", "after", "c")], []]
    [ document
        . withLines noComments {after = [Comment "c"]}
        $ mappingNode
          [
            ( plainNode "k"
            , withLines noComments {after = [EmptyLine]} (plainNode "v")
            )
          ]
    , document (plainNode "b")
    ]
  boundary
    "empty line below a flow root before a document"
    "[a]\n\n# c\n...\n---\nb\n"
    [[("document", "after", "c")], []]
    [ (document (contentNode (SequenceContent Flow [plainNode "a"])))
        { docComments = noComments {after = [EmptyLine, Comment "c"]}
        }
    , document (plainNode "b")
    ]
  boundary
    "empty line after the last key of a block root before a document"
    "k: v\n\n  # c\n...\n---\nb\n"
    [[("/k", "after", "c")], []]
    [ document $
        mappingNode
          [
            ( withLines noComments {after = [EmptyLine, Comment "c"]} (plainNode "k")
            , plainNode "v"
            )
          ]
    , document (plainNode "b")
    ]
  boundary
    "empty line above the comment of an empty root before a document"
    "a\n---\n\n# c\n...\n---\nb\n"
    [[], [("", "after", "c")], []]
    [ document (plainNode "a")
    , document (withLines noComments {before = [EmptyLine, Comment "c"]} (plainNode ""))
    , document (plainNode "b")
    ]
  boundary
    "keep literal root above a comment of the next document"
    "|+\n  a\n\n...\n\n# c\n---\nb\n"
    [[], [("document", "before", "c")]]
    [document (scalarNode Literal "a\n\n"), commented (document (plainNode "b"))]
  boundary
    "keep literal at the end of a root above a comment of the next document"
    "k: |+\n  a\n\n...\n\n# c\n---\nb\n"
    [[], [("document", "before", "c")]]
    [ document (mappingNode [(plainNode "k", scalarNode Literal "a\n\n")])
    , commented (document (plainNode "b"))
    ]
  let versioned :: YamlVersion -> T.Text
      versioned v =
        renderSyntax defaultRenderOptions [(document (plainNode "a")) {version = Just v}]
  assertEqual
    "supported version"
    "%YAML 1.3\n---\na\n"
    (versioned (YamlVersion 1 3))
  assertEqual
    "unsupported version"
    "a\n"
    (versioned (YamlVersion 2 0))
  assertEqual
    "negative minor version"
    "a\n"
    (versioned (YamlVersion 1 (-1)))
  assertEqual
    "minor version beyond the limit"
    "a\n"
    (versioned (YamlVersion 1 1000001))
  -- Without the marker, the next parse would give the comment to the root.
  let first :: String -> T.Text -> Node -> Assertion
      first preface expected root = do
        let out = renderSyntax defaultRenderOptions [commented (document root)]
        assertEqual
          preface
          expected
          out
        assertEqual
          (preface ++ ", read back")
          (Right [[Comment "c"]])
          (map (\d -> d.docComments.before) <$> parseDocumentsText out)
  first
    "comment above the first document"
    "# c\n---\na: 1\n"
    (mappingNode [(plainNode "a", plainNode "1")])
  first
    "comment above the first document with a scalar root"
    "# c\n---\na\n"
    (plainNode "a")
