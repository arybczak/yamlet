-- | The helpers that several render test modules use.
module Yamlet.Test.Render.Helpers
  ( render
  , rendersBack
  , rendersAs
  , commentsOf
  ) where

import Data.Text qualified as T
import Test.Tasty.HUnit

import Yamlet.Syntax

-- | The text of a document with the node as its root.
render :: Node -> T.Text
render n = renderSyntax defaultRenderOptions [document n]

-- | Parsing and rendering gives back the input.
rendersBack :: String -> T.Text -> Assertion
rendersBack preface input =
  assertEqual
    preface
    (Right input)
    (renderSyntax defaultRenderOptions <$> parseDocumentsText input)

-- | Parsing and rendering gives the expected text, which gives itself back.
rendersAs :: String -> T.Text -> T.Text -> Assertion
rendersAs preface expected input = do
  assertEqual
    preface
    (Right expected)
    (renderSyntax defaultRenderOptions <$> parseDocumentsText input)
  rendersBack (preface ++ ", rendered again") expected

-- | The comments of a document with the path of their nodes.
commentsOf :: Document -> [(String, String, T.Text)]
commentsOf doc = lines_ "document" doc.docComments ++ node "" doc.root
  where
    node :: String -> Node -> [(String, String, T.Text)]
    node path n =
      lines_ path n.comments {after = []}
        ++ inner
        ++ lines_ path noComments {after = n.comments.after}
      where
        inner :: [(String, String, T.Text)]
        inner = case n.content of
          SequenceContent _ xs ->
            concat (zipWith (\i x -> node (path ++ "/" ++ show i) x) [0 :: Int ..] xs)
          MappingContent _ kvs -> concatMap (entry path) kvs
          _ -> []

    entry :: String -> (Node, Node) -> [(String, String, T.Text)]
    entry path (k, v) =
      let path' = path ++ "/" ++ keyText k
      in node (path' ++ ":key") k ++ node path' v

    keyText :: Node -> String
    keyText k = case k.content of
      ScalarContent _ t -> T.unpack t
      _ -> "?"

    lines_ :: String -> Comments -> [(String, String, T.Text)]
    lines_ path c =
      [(path, "before", t) | Comment t <- c.before]
        ++ [(path, "inline", t) | Just t <- [c.inline]]
        ++ [(path, "after", t) | Comment t <- c.after]
