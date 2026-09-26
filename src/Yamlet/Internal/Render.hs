{-# OPTIONS_HADDOCK not-home #-}

-- | Rendering of the syntax tree.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Render
  ( RenderOptions (..)
  , defaultRenderOptions
  , renderSyntax
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Containers.ListUtils
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as B

import Yamlet.Internal.Emit
import Yamlet.Internal.Syntax
import Yamlet.Internal.Utils

-- | The options of 'renderSyntax'.
newtype RenderOptions = RenderOptions
  { forceBlock :: Bool
  -- ^ Write every non-empty collection in the block style. A collection in a
  -- key then becomes an explicit key, e.g. @? - a@.
  }

-- | The collection styles of the tree.
defaultRenderOptions :: RenderOptions
defaultRenderOptions =
  RenderOptions
    { forceBlock = False
    }

-- | Render documents with their comments and empty lines.
--
-- A scalar keeps its style if the style can hold its text, otherwise it gets
-- quotes. An empty line from the comments after a block scalar with the @+@
-- indicator goes away, because it would become part of the scalar. A flow
-- collection with comments inside becomes a block collection, so that every
-- comment has a line.
--
-- A comment that has no place at its node moves to a place that has one,
-- e.g. the lines above the value of a key go above the key if the value is
-- on the line of the key.
--
-- An anchor name with a character that YAML does not allow in it, e.g. a
-- space, becomes a new name in the anchor and in its aliases.
renderSyntax :: RenderOptions -> [Document] -> T.Text
renderSyntax opts = emptyLines . B.runBuilder . go True
  where
    -- The parser reads several empty lines in a row as one, and gives empty
    -- lines at the start or the end of the output to no node. So they go
    -- away, but not the empty lines in the content of a block scalar. Only
    -- the content of a block scalar with the keep indicator ends with an
    -- empty line.
    emptyLines :: T.Text -> T.Text
    emptyLines t =
      T.unlines
        . map (\l -> if l == emptyLine then "" else l)
        . dropEnd
        . dropWhile (== emptyLine)
        . collapse
        $ T.lines t

    collapse :: [T.Text] -> [T.Text]
    collapse = \case
      a : b : rest | b == emptyLine && (a == emptyLine || T.null a) -> collapse (a : rest)
      a : rest -> a : collapse rest
      [] -> []

    dropEnd :: [T.Text] -> [T.Text]
    dropEnd = reverse . dropWhile (== emptyLine) . reverse

    go :: Bool -> [Document] -> B.Builder
    go afterEnd = \case
      [] -> mempty
      doc : docs -> document opts afterEnd (validAnchors doc) <> go doc.explicitEnd docs

-- | The document with anchor names that read back. A name that an anchor
-- cannot have becomes a name that no other anchor of the document has, in
-- its anchors and in its aliases.
validAnchors :: Document -> Document
validAnchors doc
  | all isAnchorName names = doc
  | otherwise = doc {root = rename doc.root}
  where
    names :: [T.Text]
    names = collect doc.root []

    collect :: Node -> [T.Text] -> [T.Text]
    collect n acc =
      maybe id (:) n.props.anchor $ case n.content of
        Alias a -> a : acc
        Sequence _ xs -> foldr collect acc xs
        Mapping _ kvs -> foldr (\(k, v) -> collect k . collect v) acc kvs
        Scalar _ _ -> acc

    newNames :: M.Map T.Text T.Text
    newNames = snd $ L.foldl' add (S.fromList (filter isAnchorName names), M.empty) names

    add :: (S.Set T.Text, M.Map T.Text T.Text) -> T.Text -> (S.Set T.Text, M.Map T.Text T.Text)
    add (used, m) a
      | isAnchorName a || M.member a m = (used, m)
      | otherwise =
          let base = if T.null a then "anchor" else T.map (\c -> if isAnchorChar c then c else '_') a
              new = fresh used base (2 :: Int)
          in (S.insert new used, M.insert a new m)

    fresh :: S.Set T.Text -> T.Text -> Int -> T.Text
    fresh used base i
      | S.notMember base used = base
      | S.notMember candidate used = candidate
      | otherwise = fresh used base (i + 1)
      where
        candidate :: T.Text
        candidate = base <> "_" <> T.pack (show i)

    rename :: Node -> Node
    rename n =
      n
        { props = n.props {anchor = newName <$> n.props.anchor}
        , content = case n.content of
            Alias a -> Alias (newName a)
            Sequence style xs -> Sequence style (map rename xs)
            Mapping style kvs -> Mapping style (map (bimap rename rename) kvs)
            c -> c
        }

    newName :: T.Text -> T.Text
    newName a = M.findWithDefault a a newNames

    isAnchorName :: T.Text -> Bool
    isAnchorName a = not (T.null a) && T.all isAnchorChar a

    isAnchorChar :: Char -> Bool
    isAnchorChar c = isPrintable c && c /= ' ' && c `notElem` (",[]{}" :: String)

-- | A document. The flag tells if it starts the stream or follows a document
-- end marker.
document :: RenderOptions -> Bool -> Document -> B.Builder
document opts afterEnd doc =
  mconcat
    [ if needsEnd then "...\n" else mempty
    , lines_ 0 doc.docComments.before
    , if directives
        then
          foldMap
            (\v -> "%YAML " <> B.fromUnboundedDec v.major <> "." <> B.fromUnboundedDec v.minor <> "\n")
            doc.version
            <> foldMap tagDirective handles
        else mempty
    , body
    , lines_ 0 doc.docComments.after
    , if doc.explicitEnd then "...\n" else mempty
    ]
  where
    r :: Node
    r = case doc.root.content of
      Scalar style t
        | style == Literal || style == Folded
        , needsIndentIndicator t ->
            doc.root {content = Scalar DoubleQuoted t}
      _ -> doc.root

    -- The handles for the tags that are not valid URIs.
    handles :: [Char]
    handles = nubOrd . mapMaybe tagHandle $ tags r []

    tags :: Node -> [T.Text] -> [T.Text]
    tags n acc =
      (case n.props.tag of Tag t -> (t :); _ -> id) $ case n.content of
        Sequence _ xs -> foldr tags acc xs
        Mapping _ kvs -> foldr (\(k, v) -> tags k . tags v) acc kvs
        _ -> acc

    directives :: Bool
    directives = isJust doc.version || not (null handles)

    -- Without an end marker, the previous document takes the comments above
    -- this one.
    needsEnd :: Bool
    needsEnd = not afterEnd && (directives || any isComment doc.docComments.before)

    isComment :: Line -> Bool
    isComment = \case
      Comment _ -> True
      EmptyLine -> False

    -- A document needs a start marker after another document, after
    -- directives, for a comment on the marker line, and if it is empty. A
    -- block collection has no line of its own for its comment.
    marker :: Bool
    marker =
      doc.explicitStart
        || directives
        || not afterEnd
        || isEmpty r
        || isJust doc.docComments.inline
        || (isBlock opts r && isJust r.comments.inline)

    -- The marker line holds one comment. The comment of a block collection
    -- goes below it if the document has one too.
    (markerComment, rootLines) = case (doc.docComments.inline, r.comments.inline) of
      (Just dc, Just rc) | isBlock opts r -> (Just dc, Comment rc : r.comments.before)
      (dc, rc) -> (dc <|> (if isBlock opts r then rc else Nothing), r.comments.before)

    body :: B.Builder
    body
      | isBlock opts r =
          mconcat
            [ if marker
                then
                  "---"
                    <> maybe mempty (" " <>) (props r)
                    <> comment markerComment
                    <> "\n"
                    <> lines_ 0 rootLines
                else
                  lines_ 0 (rootLines ++ (if isJust (props r) then firstLines opts r else []))
                    <> maybe mempty (<> "\n") (props r)
            , block opts 0 0 True (not marker && isJust (props r)) r
            ]
      | isEmpty r = case (doc.docComments.inline, r.comments.inline) of
          (Just dc, Just rc) -> "---" <> comment (Just dc) <> "\n" <> lines_ 0 (r.comments.before ++ [Comment rc])
          (dc, rc) -> "---" <> comment (dc <|> rc) <> "\n" <> lines_ 0 r.comments.before
      | marker && null r.comments.before && isNothing doc.docComments.inline =
          "--- " <> inline opts InValue 2 r r.comments.inline <> "\n"
      | marker =
          "---"
            <> comment doc.docComments.inline
            <> "\n"
            <> lines_ 0 r.comments.before
            <> inline opts InValue 2 r r.comments.inline
            <> "\n"
      | otherwise = lines_ 0 r.comments.before <> inline opts InValue 2 r r.comments.inline <> "\n"

-- | The entries of a block collection at the given indentation, and the lines
-- after them at the given column. The first entry does not start with
-- indentation if the collection continues a line, and the lines above it are
-- not written if the caller wrote them already.
block :: RenderOptions -> Int -> Int -> Bool -> Bool -> Node -> B.Builder
block opts indent afterColumn atLineStart hoisted n = case n.content of
  Sequence _ xs -> mconcat (zipWith item [0 :: Int ..] xs) <> lines_ afterColumn n.comments.after
  Mapping _ kvs -> mconcat (zipWith entry [0 :: Int ..] kvs) <> lines_ afterColumn n.comments.after
  _ -> mempty
  where
    start :: Int -> [Line] -> B.Builder
    start i ls
      | i == 0 && not atLineStart = mempty
      | i == 0 && hoisted = spaces indent
      | otherwise = lines_ indent ls <> spaces indent

    item :: Int -> Node -> B.Builder
    item i x = start i (aboveIndicator opts x) <> "-" <> after opts indent x

    entry :: Int -> (Node, Node) -> B.Builder
    entry i (k, v) = case implicitKey opts k of
      Just key ->
        let (above, lineComment, below) = entryComments opts k v
        in start i above <> key <> ":" <> value opts indent v lineComment below
      Nothing ->
        start i (aboveIndicator opts k)
          <> "?"
          <> after opts indent k
          <> lines_ indent (aboveIndicator opts v)
          <> spaces indent
          <> ":"
          <> after opts indent v

-- | The lines above an indicator of a sequence item or an explicit entry. The
-- lines above the first entry of a block collection after the indicator go
-- there too, where the parser gives them to the collection.
aboveIndicator :: RenderOptions -> Node -> [Line]
aboveIndicator opts x = x.comments.before ++ if isBlock opts x then firstLines opts x else []

-- | The lines above the first entry of a collection.
firstLines :: RenderOptions -> Node -> [Line]
firstLines opts x = case x.content of
  Sequence _ (y : _) -> aboveIndicator opts y
  Mapping _ ((k, v) : _) -> case implicitKey opts k of
    Just _ -> let (above, _, _) = entryComments opts k v in above
    Nothing -> aboveIndicator opts k
  _ -> []

-- | The lines above an entry with an implicit key, the comment on its line
-- and the lines between the key and a block collection value. A line holds
-- one comment. If the value is on the line of the key, the lines above the
-- value and the comment of the key go above the entry. Otherwise the comment
-- of the value goes below the key.
entryComments :: RenderOptions -> Node -> Node -> ([Line], Maybe T.Text, [Line])
entryComments opts k v
  | isBlock opts v = case (k.comments.inline, v.comments.inline) of
      (Just kc, Just vc) -> (k.comments.before, Just kc, [Comment vc])
      (kc, vc) -> (k.comments.before, kc <|> vc, [])
  | otherwise = case (k.comments.inline, v.comments.inline) of
      (Just kc, Just vc) -> (k.comments.before ++ v.comments.before ++ [Comment kc], Just vc, [])
      (kc, vc) -> (k.comments.before ++ v.comments.before, vc <|> kc, [])

-- | The value of a mapping entry after the colon with the comment of the
-- line, and the line break. The lines go between the key and a block
-- collection.
value :: RenderOptions -> Int -> Node -> Maybe T.Text -> [Line] -> B.Builder
value opts indent v lineComment extra
  | isBlock opts v = case v.content of
      Sequence _ xs
        | null [() | Comment _ <- v.comments.after] || not (endsWithBlockScalar xs) ->
            header <> lines_ indent below <> block opts indent (indent + 2) True False v
        -- A block scalar as the last item would take in the lines after an
        -- indentless sequence.
        | otherwise -> header <> lines_ (indent + 2) below <> block opts (indent + 2) (indent + 2) True False v
      _ -> header <> lines_ (indent + 2) below <> block opts (indent + 2) (indent + 2) True False v
  | isEmpty v = comment lineComment <> "\n"
  | otherwise = " " <> inline opts InValue (indent + 2) v lineComment <> "\n"
  where
    header :: B.Builder
    header = maybe mempty (" " <>) (props v) <> comment lineComment <> "\n"

    below :: [Line]
    below = extra ++ v.comments.before

    endsWithBlockScalar :: [Node] -> Bool
    endsWithBlockScalar xs = case reverse xs of
      Node {content = Scalar Literal t} : _ -> isJust (literalBlock True 0 t)
      Node {content = Scalar Folded t} : _ -> isJust (foldedBlock 0 t)
      _ -> False

-- | A node after the indicator of a sequence item or an explicit entry, with
-- the line break. A block collection starts on the same line if it can.
after :: RenderOptions -> Int -> Node -> B.Builder
after opts indent n
  | isBlock opts n =
      if isNothing (props n) && isNothing n.comments.inline
        then " " <> block opts (indent + 2) (indent + 2) False True n
        else
          maybe mempty (" " <>) (props n)
            <> comment n.comments.inline
            <> "\n"
            <> block opts (indent + 2) (indent + 2) True True n
  | isEmpty n = comment n.comments.inline <> "\n"
  | otherwise = " " <> inline opts InValue (indent + 2) n n.comments.inline <> "\n"

-- | Where an inline node is.
data Position = InValue | InKey | InFlow
  deriving stock (Eq)

-- | A node on one line with the given comment at its end, except a block
-- scalar, whose content lines are at the given indentation.
inline :: RenderOptions -> Position -> Int -> Node -> Maybe T.Text -> B.Builder
inline opts pos indent n lineComment = case n.content of
  Alias name -> "*" <> B.fromText name <> comment lineComment
  Scalar style t
    | isBlockScalar style && pos == InValue -> withProps (blockScalar style t)
  _ -> withProps content_ <> comment lineComment
  where
    withProps :: B.Builder -> B.Builder
    withProps b = case props n of
      Just p
        | isEmpty' -> p
        | otherwise -> p <> " " <> b
      Nothing -> b

    isEmpty' :: Bool
    isEmpty' = case n.content of
      Scalar Plain t -> T.null t
      _ -> False

    -- The comment goes on the line of the header.
    blockScalar :: ScalarStyle -> T.Text -> B.Builder
    blockScalar style t = case style of
      Literal | Just (h, b) <- literalBlock True indent t -> h <> comment lineComment <> b
      Folded | Just (h, b) <- foldedBlock indent t -> h <> comment lineComment <> b
      _ -> doubleQuoted t <> comment lineComment

    content_ :: B.Builder
    content_ = case n.content of
      Scalar style t -> scalar pos style t
      Sequence _ [] | hasEndLines n -> "[\n" <> lines_ indent n.comments.after <> spaces indent <> "]"
      Mapping _ [] | hasEndLines n -> "{\n" <> lines_ indent n.comments.after <> spaces indent <> "}"
      Sequence _ xs -> "[" <> commas (map (\x -> inline opts InFlow indent (flowItem x) Nothing) xs) <> "]"
      Mapping _ kvs -> "{" <> commas (map flowEntry kvs) <> "}"
      Alias {} -> mempty

    -- An empty scalar cannot be an item of a flow sequence.
    flowItem :: Node -> Node
    flowItem x = case x.content of
      Scalar Plain ""
        | Props Nothing NoTag <- x.props ->
            x {props = Props Nothing (Tag "tag:yaml.org,2002:null")}
      _ -> x

    flowEntry :: (Node, Node) -> B.Builder
    flowEntry (k, v) =
      mconcat
        [ inline opts InFlow indent k Nothing
        , if endsWithName k then " :" else ":"
        , if isEmpty v then mempty else " " <> inline opts InFlow indent v Nothing
        ]

    commas :: [B.Builder] -> B.Builder
    commas = \case
      [] -> mempty
      b : bs -> b <> mconcat (map (", " <>) bs)

isBlockScalar :: ScalarStyle -> Bool
isBlockScalar s = s == Literal || s == Folded

-- | A scalar on one line in its style, or in a style that can hold its text.
scalar :: Position -> ScalarStyle -> T.Text -> B.Builder
scalar pos style t = case style of
  Plain
    | T.null t -> mempty
    | plainSyntax (pos == InFlow) t -> B.fromText t
    | otherwise -> quoted
  SingleQuoted -> quoted
  _ -> doubleQuoted t
  where
    quoted :: B.Builder
    quoted = fromMaybe (doubleQuoted t) (singleQuoted t)

-- | A key on one line, or 'Nothing' if it needs an explicit entry.
implicitKey :: RenderOptions -> Node -> Maybe B.Builder
implicitKey opts k
  | isBlock opts k = Nothing
  | isEmpty k = Nothing
  | hasEndLines k = Nothing
  | T.length (B.runBuilder key) > maxImplicitKeyLength = Nothing
  | otherwise = Just key
  where
    key :: B.Builder
    key = inline opts InKey 0 k Nothing <> if endsWithName k then " " else mempty

-- | The node is a collection that the renderer writes in the block style.
isBlock :: RenderOptions -> Node -> Bool
isBlock opts n = case n.content of
  Sequence style (_ : _) -> style == Block || opts.forceBlock || hasComments n
  Mapping style (_ : _) -> style == Block || opts.forceBlock || hasComments n
  _ -> False

-- | The node or a node inside it has a comment, other than the lines above
-- the node and its inline comment, which fit outside a flow collection.
hasComments :: Node -> Bool
hasComments n =
  not (null (commentLines n.comments.after)) || case n.content of
    Sequence _ xs -> any inner xs
    Mapping _ kvs -> any (\(k, v) -> inner k || inner v) kvs
    _ -> False
  where
    inner :: Node -> Bool
    inner x = not (null (commentLines x.comments.before)) || isJust x.comments.inline || hasComments x

    commentLines :: [Line] -> [Line]
    commentLines = filter (/= EmptyLine)

-- | The node is an empty collection with comments at its end, which go
-- between its brackets.
hasEndLines :: Node -> Bool
hasEndLines n = case n.content of
  Sequence _ [] -> hasComment
  Mapping _ [] -> hasComment
  _ -> False
  where
    hasComment :: Bool
    hasComment = not (null [() | Comment _ <- n.comments.after])

-- | The node is an empty plain scalar without properties.
isEmpty :: Node -> Bool
isEmpty n = case (n.props, n.content) of
  (Props Nothing NoTag, Scalar Plain t) -> T.null t
  _ -> False

-- | The node ends with an alias, an anchor or a tag. A colon right after it
-- would be part of the name.
endsWithName :: Node -> Bool
endsWithName n = case n.content of
  Alias {} -> True
  Scalar Plain t -> T.null t && (isJust n.props.anchor || n.props.tag /= NoTag)
  _ -> False

-- | The anchor and the tag of a node.
props :: Node -> Maybe B.Builder
props n = case n.content of
  Alias {} -> Nothing
  _ -> case (anchor, tag) of
    (Nothing, Nothing) -> Nothing
    (Just a, Nothing) -> Just a
    (Nothing, Just t) -> Just t
    (Just a, Just t) -> Just (a <> " " <> t)
  where
    anchor :: Maybe B.Builder
    anchor = ("&" <>) . B.fromText <$> n.props.anchor

    tag :: Maybe B.Builder
    tag = case n.props.tag of
      NoTag -> Nothing
      NonSpecificTag -> Just "!"
      Tag t -> Just (tagText t)

-- | A comment at the end of a line.
comment :: Maybe T.Text -> B.Builder
comment = \case
  Nothing -> mempty
  Just t -> " #" <> text (T.stripEnd (T.map (\c -> if c == '\n' || c == '\r' then ' ' else c) (printable t)))
  where
    text :: T.Text -> B.Builder
    text t = if T.null t then mempty else " " <> B.fromText t

-- | Lines of comments at the given indentation.
lines_ :: Int -> [Line] -> B.Builder
lines_ indent = mconcat . map line
  where
    line :: Line -> B.Builder
    line = \case
      EmptyLine -> B.fromText emptyLine <> "\n"
      Comment t ->
        mconcat . map commentLine $
          T.splitOn "\n" (T.replace "\r" "\n" (T.replace "\r\n" "\n" (printable t)))

    -- The parser drops the white space at the end of a comment.
    commentLine :: T.Text -> B.Builder
    commentLine l
      | T.null (T.stripEnd l) = spaces indent <> "#\n"
      | otherwise = spaces indent <> "# " <> B.fromText (T.stripEnd l) <> "\n"

-- | The mark of an empty line from the comments. The output has no other NUL
-- character.
emptyLine :: T.Text
emptyLine = "\0"

-- | The text of a comment with a replacement for the characters that YAML
-- does not allow.
printable :: T.Text -> T.Text
printable = T.map $ \c -> if c == '\t' || c == '\n' || c == '\r' || isPrintable c then c else '\xFFFD'
