-- | The properties of rendering generated documents.
module Yamlet.Test.Render.Properties
  ( prop_noThunks
  , prop_roundTrip
  ) where

import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Test.Tasty.QuickCheck

import Yamlet.Syntax
import Yamlet.Test.Helpers.Thunks
import Yamlet.Test.Render.Helpers

prop_noThunks :: Tree -> Property
prop_noThunks (Tree doc) =
  let out = renderSyntax defaultRenderOptions [doc]
  in counterexample (T.unpack out) $ case parseDocumentsText out of
       Right docs -> ioProperty ((=== []) <$> thunks docs)
       Left e -> counterexample (show e) False

-- | Rendering a tree and parsing the result gives the same tree, except for
-- the styles, the offsets and the places of the comments. Every comment
-- stays, and rendering the result again gives the same text.
prop_roundTrip :: Tree -> Property
prop_roundTrip (Tree doc) =
  let out = renderSyntax defaultRenderOptions [doc]
  in counterexample (T.unpack out) $ case parseDocumentsText out of
       Right [doc'] ->
         checkCoverage . cover 10 (hasLines doc'.root) "scalars on several lines" $
           conjoin
             [ counterexample "tree" $
                 strip doc'.root === strip (flowItems False doc.root)
             , counterexample "comments" $
                 L.sort (allComments doc') === L.sort (allComments doc)
             , let out' = renderSyntax defaultRenderOptions [doc']
               in counterexample ("text: " ++ firstDifference out out') (out' == out)
             ]
       r -> counterexample (show r) False
  where
    strip :: Node -> Node
    strip n =
      ( contentNode $ case n.content of
          ScalarContent _ t -> ScalarContent Plain t
          SequenceContent _ xs -> SequenceContent Block (map strip xs)
          MappingContent _ kvs ->
            MappingContent Block [(strip k, strip v) | (k, v) <- kvs]
          AliasContent a -> AliasContent a
      )
        { props = n.props
        }

    -- The first line where two texts differ, with the line before it.
    firstDifference :: T.Text -> T.Text -> String
    firstDifference a b = go 1 "" (T.lines a) (T.lines b)
      where
        go :: Int -> T.Text -> [T.Text] -> [T.Text] -> String
        go n prev xs ys = case (xs, ys) of
          (x : xs', y : ys') | x == y -> go (n + 1) x xs' ys'
          _ ->
            "line "
              ++ show n
              ++ " after "
              ++ show prev
              ++ ": "
              ++ show (take 1 xs)
              ++ " /= "
              ++ show (take 1 ys)

    allComments :: Document -> [T.Text]
    allComments d = [t | (_, _, t) <- commentsOf d]

    hasLines :: Node -> Bool
    hasLines n = case n.content of
      ScalarLinesContent _ _ starts -> not (null starts)
      SequenceContent _ xs -> any hasLines xs
      MappingContent _ kvs -> any (\(k, v) -> hasLines k || hasLines v) kvs
      AliasContent _ -> False

    -- The renderer gives an empty item of a flow sequence a tag.
    flowItems :: Bool -> Node -> Node
    flowItems inFlow n = case n.content of
      SequenceContent s xs ->
        let inFlow' = inFlow || (s == Flow && not (hasComments n))
        in n {content = SequenceContent s (map (item inFlow' . flowItems inFlow') xs)}
      MappingContent s kvs ->
        let inFlow' = inFlow || (s == Flow && not (hasComments n))
        in n
             { content =
                 MappingContent
                   s
                   [(flowItems inFlow' k, flowItems inFlow' v) | (k, v) <- kvs]
             }
      _ -> n

    item :: Bool -> Node -> Node
    item inFlow n = case (n.props, n.content) of
      (Props Nothing NoTag, ScalarContent Plain "")
        | inFlow ->
            n {props = Props Nothing (Tag "tag:yaml.org,2002:null")}
      _ -> n

    -- A flow collection with comments inside becomes a block collection.
    hasComments :: Node -> Bool
    hasComments n =
      not (null [() | Comment _ <- n.comments.after]) || case n.content of
        SequenceContent _ xs -> any inner xs
        MappingContent _ kvs -> any (\(k, v) -> inner k || inner v) kvs
        _ -> False
      where
        inner :: Node -> Bool
        inner x =
          not (null [() | Comment _ <- x.comments.before])
            || isJust x.comments.inline
            || hasComments x

newtype Tree = Tree Document
  deriving stock (Show)

instance Arbitrary Tree where
  arbitrary = do
    root <- sized genNode
    c <- genComments
    pure . Tree $ (document root) {docComments = c}

genNode :: Int -> Gen Node
genNode size = do
  n <-
    if size <= 1
      then genScalar
      else
        frequency
          [ (3, genScalar)
          , (1, contentNode . AliasContent <$> genAnchor)
          , (1, contentNode <$> (SequenceContent <$> genStyle <*> genList))
          , (1, contentNode <$> (MappingContent <$> genStyle <*> genEntries))
          ]
  p <- case n.content of
    AliasContent _ -> pure noProps
    _ -> genProps
  c <- genComments
  -- The text has no place for the lines after a block scalar.
  pure n {props = p, comments = if isBlock n then c {after = []} else c}
  where
    genList :: Gen [Node]
    genList = do
      k <- choose (0, 4)
      vectorOf k (genNode (size `div` 3))

    -- The lines below a scalar or an alias key read back as the lines above
    -- the value.
    genEntries :: Gen [(Node, Node)]
    genEntries = do
      k <- choose (0, 4)
      vectorOf
        k
        ((,) . noLinesAfterScalar <$> genNode (size `div` 4) <*> genNode (size `div` 3))

    noLinesAfterScalar :: Node -> Node
    noLinesAfterScalar k = case k.content of
      SequenceContent {} -> k
      MappingContent {} -> k
      _ -> k {comments = k.comments {after = []}}

    isBlock :: Node -> Bool
    isBlock n = case n.content of
      ScalarLinesContent style _ _ -> style == Literal || style == Folded
      _ -> False

    genStyle :: Gen CollectionStyle
    genStyle = elements [Block, Flow]

    -- A scalar, often with positions of new lines. Some positions are not
    -- valid, e.g. outside the text or twice the same.
    genScalar :: Gen Node
    genScalar = do
      style <- elements [minBound .. maxBound]
      t <- genText
      starts <-
        frequency [(1, pure []), (2, L.sort <$> listOf (choose (0, T.length t + 1)))]
      pure (contentNode (ScalarLinesContent style t starts))

    genProps :: Gen Props
    genProps =
      Props
        <$> oneof [pure Nothing, Just <$> genAnchor]
        <*> elements
          [ NoTag
          , NoTag
          , NonSpecificTag
          , Tag "tag:yaml.org,2002:str"
          , Tag "!local"
          , Tag "tag:example.com,2000:x"
          ]

    genAnchor :: Gen T.Text
    genAnchor = elements ["a", "b", "anchor"]

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
          , "12"
          , "---"
          , "..."
          , "foo\n"
          , "\nfoo"
          , "  lead"
          , "trail  "
          , "a\n\nb\n\n"
          , "\n"
          , "\n\n"
          , " \n"
          , "a\n "
          , "|"
          , ">"
          , "[a]"
          , "{a: b}"
          , "a, b"
          , "key:"
          , "\r\n"
          , "a\n  b\nc"
          , "  a\nb"
          , "a\n\n  b\n\nc\n"
          ]

        genChar :: Gen Char
        genChar =
          frequency
            [ (10, elements "abc xyz-:#,[]{}'\"!&*?|>%@`\\")
            , (2, elements "\t\r\x85\xA0\x2028\xFEFF\x01")
            , (1, arbitrary)
            ]

-- | Comments for a node.
genComments :: Gen Comments
genComments =
  frequency
    [ (3, pure noComments)
    ,
      ( 1
      , Comments
          <$> genLines
          <*> oneof [pure Nothing, Just <$> genCommentText]
          <*> genLines
      )
    ]
  where
    genLines :: Gen [Line]
    genLines = do
      k <- choose (0, 2)
      vectorOf k $
        frequency
          [ (3, CommentLine <$> elements [1, 1, 2, 3] <*> genCommentText)
          , (1, pure EmptyLine)
          ]

    genCommentText :: Gen T.Text
    genCommentText =
      elements ["a comment", "", "x", "# hash", "key: value", "- item", "'quoted'"]
