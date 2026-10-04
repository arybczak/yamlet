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
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as B
import GHC.Generics

import Yamlet.Internal.Emit
import Yamlet.Internal.Parser.Chars hiding (isAnchorChar)
import Yamlet.Internal.Syntax
import Yamlet.Internal.Utils

-- A data type, so that a later release can add an option.
{- HLINT ignore RenderOptions "Use newtype instead of data" -}

-- | The options of 'renderSyntax'.
data RenderOptions = RenderOptions
  { forceBlock :: !Bool
  -- ^ Write every non-empty collection in the block style. A collection in a
  -- key then becomes an explicit key, e.g. @? - a@.
  }
  deriving stock (Generic)

-- | Keep the collection styles of the tree.
defaultRenderOptions :: RenderOptions
defaultRenderOptions =
  RenderOptions
    { forceBlock = False
    }

-- | Render documents with their comments and empty lines.
--
-- The output differs from the tree where YAML cannot hold it:
--
-- * A @---@ marker is on a line of its own, with at most a comment after
--   it.
--
-- * A scalar keeps its style if the style can hold its text, otherwise it
--   gets quotes.
--
-- * The empty lines from the comments right after a block scalar with the
--   @+@ indicator go away, because they would become part of the scalar.
--
-- * A flow collection with comments inside becomes a block collection, so
--   that every comment has a line.
--
-- * A flow collection without comments is on one line, so the empty lines
--   inside it go away.
--
-- * A comment that has no place at its node moves to a place that has one,
--   e.g. the lines above the value of a key go above the key if the value
--   is on the line of the key.
--
-- * An anchor name with a character that YAML does not allow in it, e.g. a
--   space, or that YAML 1.1 reads as a line break, e.g. U+2028, becomes a
--   new name in the anchor and in its aliases.
--
-- * A version that the parser does not support, e.g. 2.0, has no @%YAML@
--   directive.
--
-- With 'forceBlock', the flow collections become block collections:
--
-- >>> :{
-- case parseDocumentsText "a: [1, {b: 2}]\n" of
--   Left err -> putStrLn (prettyError "input.yaml" err)
--   Right docs ->
--     T.putStr (renderSyntax defaultRenderOptions {forceBlock = True} docs)
-- :}
-- a:
-- - 1
-- - b: 2
renderSyntax :: RenderOptions -> [Document] -> T.Text
renderSyntax opts = emptyLines . B.runBuilder . go True
  where
    -- The empty lines at the start or the end of the output go away, because
    -- the parser gives the lines there to no node. The empty lines in the
    -- content of a block scalar stay. Only the content of a block scalar
    -- with the keep indicator ends with an empty line, and the empty lines
    -- right after it go away, because they would become part of it.
    emptyLines :: T.Text -> T.Text
    emptyLines t
      | T.any (== '\0') t =
          T.unlines
            . map (\l -> if l == emptyLine then "" else l)
            . dropEnd
            . dropWhile (== emptyLine)
            . afterKeptLines
            $ T.lines t
      -- Every document ends with a line break, so the lines stay the same.
      | otherwise = t

    afterKeptLines :: [T.Text] -> [T.Text]
    afterKeptLines = \case
      a : rest | T.null a -> a : afterKeptLines (dropWhile (== emptyLine) rest)
      a : rest -> a : afterKeptLines rest
      [] -> []

    dropEnd :: [T.Text] -> [T.Text]
    dropEnd = reverse . dropWhile (== emptyLine) . reverse

    go :: Bool -> [Document] -> B.Builder
    go afterEnd = \case
      [] -> mempty
      doc : docs ->
        let nextLines = case docs of
              next : _ -> not (null next.docComments.before)
              [] -> False
            prepared = validAnchors doc {root = topLevel nextLines (commentedBlocks doc.root)}
            ends = writesEnd opts (not (null docs)) nextLines prepared
        in document afterEnd ends prepared <> go ends docs

    -- A block scalar without content at the top level would take the lines
    -- below it in, also those of the next document if the flag tells that it
    -- has lines above its start marker.
    topLevel :: Bool -> Node -> Node
    topLevel nextLines n = case n.content of
      ScalarLinesContent style t starts
        | isBlockScalar style
        , needsIndentIndicator t || T.all (== '\n') t && (not (null n.comments.after) || nextLines) ->
            n {content = ScalarLinesContent DoubleQuoted t starts}
      _ -> n

    -- A document. The flags tell if it starts the stream or follows a
    -- document end marker, and if it ends with a document end marker.
    document :: Bool -> Bool -> Document -> B.Builder
    document afterEnd ends doc =
      mconcat
        [ if needsEnd then "...\n" else mempty
        , gap
        , lines_ 0 doc.docComments.before
        , if directives
            then
              foldMap
                (\v -> "%YAML " <> B.fromUnboundedDec v.major <> "." <> B.fromUnboundedDec v.minor <> "\n")
                version
                <> foldMap tagDirective handles
            else mempty
        , body
        , if linesAboveEnd then lines_ 0 doc.docComments.after else mempty
        , if ends then "...\n" else mempty
        , if linesAboveEnd then mempty else lines_ 0 doc.docComments.after
        ]
      where
        r :: Node
        r = doc.root

        -- The lines between a flow collection root and the end marker belong
        -- to the document, as do the lines below the marker. Below the
        -- marker, an empty line would end them.
        linesAboveEnd :: Bool
        linesAboveEnd = isFlowCollection opts r && commentBelowEmptyLine doc.docComments.after

        -- The end of the document above takes the comments right below it.
        -- The lines above the first entry of a block root come first too.
        gap :: B.Builder
        gap = case if null doc.docComments.before && not marker then aboveIndicator opts r else doc.docComments.before of
          Comment _ : _ -> lines_ 0 [EmptyLine]
          _ -> mempty

        handles :: [Char]
        handles = tagHandles r

        -- The parser rejects the other versions.
        version :: Maybe YamlVersion
        version = case doc.version of
          Just v | v.major == 1, v.minor >= 0, v.minor <= maxVersion -> Just v
          _ -> Nothing

        directives :: Bool
        directives = isJust version || not (null handles)

        needsEnd :: Bool
        needsEnd = not afterEnd && directives

        -- A document needs a start marker after another document, after
        -- directives, for a comment on the marker line, and if it is empty. A
        -- block collection has no line of its own for its comment. Without
        -- the marker, the lines above a document read back as the root's.
        marker :: Bool
        marker =
          doc.explicitStart
            || directives
            || not afterEnd
            || isEmpty r
            || not (null doc.docComments.before)
            || isJust doc.docComments.inline
            || (isBlock opts r && isJust r.comments.inline)

        -- The marker line holds one comment. The comment of a block
        -- collection goes below it if the document has one too.
        (markerComment, rootLines) = case (doc.docComments.inline, r.comments.inline) of
          (Just dc, Just rc) | isBlock opts r -> (Just dc, Comment rc : r.comments.before)
          (dc, rc) -> (dc <|> (if isBlock opts r then rc else Nothing), r.comments.before)

        body :: B.Builder
        body
          | isBlock opts r =
              (if marker then "---" <> comment markerComment <> "\n" else mempty)
                <> lines_ 0 (separated rootLines ++ (if isJust (props r) then firstLines opts r else []))
                <> maybe mempty (<> "\n") (props r)
                <> block opts 0 0 True (isJust (props r)) [] r
          | otherwise = scalarBody <> linesBelow 0 r

        scalarBody :: B.Builder
        scalarBody
          | isEmpty r = let (c, ls) = emptyRootLines doc in "---" <> comment c <> "\n" <> lines_ 0 ls
          | marker =
              "---"
                <> comment doc.docComments.inline
                <> "\n"
                <> lines_ 0 r.comments.before
                <> inline opts InValue indentStep r r.comments.inline
                <> "\n"
          | otherwise = lines_ 0 r.comments.before <> inline opts InValue indentStep r r.comments.inline <> "\n"

-- | The node with every flow collection that has a comment inside
-- in the block style, so that every comment has a line. The comments of a
-- collection in its lines above and in its inline comment fit outside a flow
-- collection.
commentedBlocks :: Node -> Node
commentedBlocks = fst . go
  where
    -- The node, and whether it or a node inside it has a comment that does
    -- not fit outside a flow collection.
    go :: Node -> (Node, Bool)
    go n = case n.content of
      SequenceContent style xs ->
        let ys = map go xs
            has = linesAfter || any inner ys
        in (n {content = SequenceContent (styleOf has style) (map fst ys)}, has)
      MappingContent style kvs ->
        let ys = map (bimap go go) kvs
            has = linesAfter || any (\(k, v) -> inner k || inner v) ys
        in (n {content = MappingContent (styleOf has style) (map (bimap fst fst) ys)}, has)
      _ -> (n, linesAfter)
      where
        linesAfter :: Bool
        linesAfter = hasCommentLine n.comments.after

    inner :: (Node, Bool) -> Bool
    inner (x, has) = hasCommentLine x.comments.before || isJust x.comments.inline || has

    styleOf :: Bool -> CollectionStyle -> CollectionStyle
    styleOf has style = if has then Block else style

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
        AliasContent a -> a : acc
        SequenceContent _ xs -> foldr collect acc xs
        MappingContent _ kvs -> foldr (\(k, v) -> collect k . collect v) acc kvs
        ScalarContent _ _ -> acc

    newNames :: M.Map T.Text T.Text
    newNames = (\(_, _, m) -> m) $ L.foldl' add (S.fromList (filter isAnchorName names), M.empty, M.empty) names

    -- The state has the used names, the next suffix to try for each base, and
    -- the new names. A suffix below the next one is used already, so the
    -- search does not try it again.
    add
      :: (S.Set T.Text, M.Map T.Text Int, M.Map T.Text T.Text)
      -> T.Text
      -> (S.Set T.Text, M.Map T.Text Int, M.Map T.Text T.Text)
    add (used, next, m) a
      | isAnchorName a || M.member a m = (used, next, m)
      | otherwise =
          let base = if T.null a then "anchor" else T.map (\c -> if isAnchorChar c then c else '_') a
              (new, i) = fresh used base (M.findWithDefault firstSuffix base next)
          in (S.insert new used, M.insert base i next, M.insert a new m)

    -- The name without a suffix is the first one, so the suffixes start at 2.
    firstSuffix :: Int
    firstSuffix = 2

    -- The free name and the next suffix to try.
    fresh :: S.Set T.Text -> T.Text -> Int -> (T.Text, Int)
    fresh used base i
      | S.notMember base used = (base, i)
      | S.notMember candidate used = (candidate, i + 1)
      | otherwise = fresh used base (i + 1)
      where
        candidate :: T.Text
        candidate = base <> "_" <> T.pack (show i)

    rename :: Node -> Node
    rename n =
      n
        { props = n.props {anchor = newName <$> n.props.anchor}
        , content = case n.content of
            AliasContent a -> AliasContent (newName a)
            SequenceContent style xs -> SequenceContent style (map rename xs)
            MappingContent style kvs -> MappingContent style (map (bimap rename rename) kvs)
            c -> c
        }

    newName :: T.Text -> T.Text
    newName a = M.findWithDefault a a newNames

    isAnchorName :: T.Text -> Bool
    isAnchorName a = not (T.null a) && T.all isAnchorChar a

    -- YAML 1.1 reads U+2028 and U+2029 as line breaks.
    isAnchorChar :: Char -> Bool
    isAnchorChar c = isPrintable c && c /= ' ' && c /= '\x2028' && c /= '\x2029' && not (asciiChar isFlowIndicator c)

-- | The document ends with a @...@ marker. The flags tell if another
-- document follows and if it has lines above its start marker.
--
-- Without the marker, the lines at the end of the document read back as the
-- root's, unless the root is a flow collection. Before the next document,
-- an empty line at the end of the root would end the lines of the root, and
-- a literal block scalar with the keep indicator at the end would take the
-- empty line above the lines of the next document in.
writesEnd :: RenderOptions -> Bool -> Bool -> Document -> Bool
writesEnd opts next nextLines doc =
  doc.explicitEnd
    || not (null doc.docComments.after) && not (isFlowCollection opts doc.root)
    || next && commentBelowEmptyLine endLines
    || nextLines && endsWithKeep doc.root
  where
    -- The lines at the end of the root, which read back as its last lines.
    -- The lines of an empty root are all below its start marker.
    endLines :: [Line]
    endLines
      | isEmpty doc.root = snd (emptyRootLines doc) ++ doc.root.comments.after
      | isFlowCollection opts doc.root = []
      | otherwise = linesAtEnd doc.root

    -- The lines at the end of the node and of the nodes that end it.
    linesAtEnd :: Node -> [Line]
    linesAtEnd n = inner ++ n.comments.after
      where
        inner :: [Line]
        inner = case n.content of
          SequenceContent _ xs | isBlock opts n, x : _ <- reverse xs -> linesAtEnd x
          MappingContent _ kvs | isBlock opts n, (_, v) : _ <- reverse kvs -> linesAtEnd v
          _ -> []

    -- A comment line below the scalar ends its content.
    endsWithKeep :: Node -> Bool
    endsWithKeep n
      | hasCommentLine n.comments.after = False
      | otherwise = case n.content of
          ScalarContent Literal t -> isJust (literalBlock True 0 t) && hasKeepIndicator t
          SequenceContent _ xs | isBlock opts n, x : _ <- reverse xs -> endsWithKeep x
          MappingContent _ kvs | isBlock opts n, (_, v) : _ <- reverse kvs -> endsWithKeep v
          _ -> False

-- | The comment on the start marker line of a document with an empty root,
-- and the lines below the marker, without the lines at the end of the root.
-- The marker line holds one comment, so the comment of the root goes below
-- it if the document has one too.
emptyRootLines :: Document -> (Maybe T.Text, [Line])
emptyRootLines doc = case (doc.docComments.inline, doc.root.comments.inline) of
  (Just dc, Just rc) -> (Just dc, doc.root.comments.before ++ [Comment rc])
  (dc, rc) -> (dc <|> rc, doc.root.comments.before)

-- | The lines have a comment below an empty line.
commentBelowEmptyLine :: [Line] -> Bool
commentBelowEmptyLine ls = case dropWhile (/= EmptyLine) ls of
  _ : rest -> hasCommentLine rest
  [] -> False

-- | A collection that the renderer writes in the flow style.
isFlowCollection :: RenderOptions -> Node -> Bool
isFlowCollection opts n = case n.content of
  SequenceContent {} -> not (isBlock opts n)
  MappingContent {} -> not (isBlock opts n)
  _ -> False

-- | The entries of a block collection at the given indentation, and the lines
-- after them at the given column. The first entry does not start with
-- indentation if the collection continues a line, and the lines above it are
-- not written if the caller wrote them already. The given lines go to the
-- first entry if it starts below its indicator, as in @indicatorLines@.
block :: RenderOptions -> Int -> Int -> Bool -> Bool -> [Line] -> Node -> B.Builder
block opts indent afterColumn atLineStart hoisted carried n = case n.content of
  SequenceContent _ xs -> mconcat (zipWith item [0 :: Int ..] xs) <> lines_ afterColumn n.comments.after
  MappingContent _ kvs -> mconcat (zipWith entry [0 :: Int ..] kvs) <> lines_ afterColumn n.comments.after
  _ -> mempty
  where
    start :: Int -> [Line] -> B.Builder
    start i ls
      | i == 0 && not atLineStart = mempty
      | i == 0 && hoisted = spaces indent
      | otherwise = lines_ indent ls <> spaces indent

    -- Without the second case, the tuple of 'indicatorLines' makes the render
    -- benchmark of the config input allocate more.
    item :: Int -> Node -> B.Builder
    item i x
      | startsBelow opts x =
          let (above, below, rest) = indicatorLines (i == 0) (if i == 0 then carried else []) x
          in start i above <> "-" <> after opts indent (indent + indentStep) below rest x
      | otherwise = start i (aboveIndicator opts x) <> "-" <> after opts indent (indent + indentStep) [] [] x

    entry :: Int -> (Node, Node) -> B.Builder
    entry i (k, v) = case implicitKey opts k of
      Just key ->
        let (above, lineComment, below) = entryComments opts k v
        in start i above <> key <> ":" <> value v lineComment below
      Nothing ->
        let (keyAbove, keyBelow, keyRest) = indicatorLines (i == 0) (if i == 0 then carried else []) k
            (valueAbove, valueBelow, valueRest) = indicatorLines False [] v
        in start i keyAbove
             <> "?"
             <> after opts indent indent keyBelow keyRest k
             <> lines_ indent valueAbove
             <> spaces indent
             <> ":"
             <> after opts indent (indent + indentStep) valueBelow valueRest v

    -- The lines above the indicator of a sequence item or an explicit entry,
    -- the lines below it, and the lines for the first entry of a block
    -- collection that starts below its indicator. The flag is set for the
    -- first entry of a collection, and the given lines come first.
    --
    -- The parser gives the lines above and below the indicator of a block
    -- collection that starts below it to the collection up to the last empty
    -- line, and the rest to its first entry. Above the indicator of a first
    -- entry, the collection around it takes the lines up to the last empty
    -- line, so the lines of a first entry go below its indicator. A comment
    -- on the line of the indicator of a later entry keeps the lines above it
    -- from the first entry, so the lines of the first entry after the last
    -- empty line go below the indicator.
    indicatorLines :: Bool -> [Line] -> Node -> ([Line], [Line], [Line])
    indicatorLines isFirst given x
      | startsBelow opts x =
          let ls = given ++ x.comments.before
          in if
               | firstStartsBelow opts x ->
                   let (own, rest) = splitAtLastEmptyLine ls
                   in if isFirst then ([], own, rest) else (own, [], rest)
               | isFirst -> ([], separated ls ++ firstLines opts x, [])
               | isJust x.comments.inline ->
                   let (above, below) = splitAtLastEmptyLine (firstLines opts x)
                   in (ls ++ above, below, [])
               | otherwise -> (ls ++ firstLines opts x, [], [])
      | otherwise = (aboveIndicator opts x, [], [])

    -- The value of a mapping entry after the colon with the comment of the
    -- line, and the line break. The lines go between the key and a block
    -- collection, or below the entry, indented deeper than the key, where
    -- the lines after the value go too.
    value :: Node -> Maybe T.Text -> [Line] -> B.Builder
    value v lineComment extra
      | isBlock opts v =
          -- Without the bang, the render benchmark of the config input
          -- allocates more.
          let !column = case v.content of
                -- A sequence without indentation has no column of its own for
                -- the lines after its last item: a block collection or a block
                -- scalar as the last item takes in every line that is deeper
                -- than the key.
                SequenceContent _ xs
                  | not (hasCommentLine v.comments.after) || not (endsWithBlock xs) -> indent
                _ -> indent + indentStep
          in header <> lines_ column below <> block opts column (indent + indentStep) True False rest v
      | isEmpty v = comment lineComment <> "\n" <> entryBelow
      | otherwise = " " <> inline opts InValue (indent + indentStep) v lineComment <> "\n" <> entryBelow
      where
        -- The lines after a block scalar end it at the column of the key.
        -- Without the first case, the render benchmark of the config input
        -- allocates more.
        entryBelow :: B.Builder
        entryBelow
          | null extra && null v.comments.after = mempty
          | otherwise =
              let column = if isBlockScalarNode v then indent else indent + indentStep
              in lines_ column extra <> linesBelow column v

        header :: B.Builder
        header = maybe mempty (" " <>) (props v) <> comment lineComment <> "\n"

        -- The value takes the lines below the key as in 'indicatorLines'.
        below, rest :: [Line]
        (below, rest)
          | firstStartsBelow opts v = splitAtLastEmptyLine (extra ++ v.comments.before)
          | otherwise = (separated (extra ++ v.comments.before), [])

        endsWithBlock :: [Node] -> Bool
        endsWithBlock xs = case reverse xs of
          x : _ -> isBlockScalarNode x || isBlock opts x
          [] -> False

-- 'after' stays at top level, although 'block' is its only caller. In the
-- where clause of 'block', it made the render benchmark of the config input
-- allocate more.

-- | A node after the indicator of a sequence item or an explicit entry, with
-- the line break, and the lines below the indicator and the lines for the
-- first entry from @indicatorLines@. A block collection starts on the same
-- line if it can. The lines after a scalar go at the given column.
after :: RenderOptions -> Int -> Int -> [Line] -> [Line] -> Node -> B.Builder
after opts indent column below rest n
  | isBlock opts n =
      if startsBelow opts n
        then
          maybe mempty (" " <>) (props n)
            <> comment n.comments.inline
            <> "\n"
            <> lines_ (indent + indentStep) below
            <> block opts (indent + indentStep) (indent + indentStep) True True rest n
        else " " <> block opts (indent + indentStep) (indent + indentStep) False True [] n
  | isEmpty n = comment n.comments.inline <> "\n" <> linesBelow column n
  | otherwise =
      let column' = if isBlockScalarNode n then indent else column
      in " " <> inline opts InValue (indent + indentStep) n n.comments.inline <> "\n" <> linesBelow column' n

-- | The lines of a block collection that go directly above its first entry.
-- The parser gives the lines there to the entry, so the lines of the
-- collection end with an empty line.
separated :: [Line] -> [Line]
separated ls = case reverse ls of
  [] -> []
  EmptyLine : _ -> ls
  _ -> ls ++ [EmptyLine]

-- | The lines above an indicator of a node that does not start below it. The
-- lines above the first entry of a block collection after the indicator go
-- there too.
aboveIndicator :: RenderOptions -> Node -> [Line]
aboveIndicator opts x = x.comments.before ++ if isBlock opts x then firstLines opts x else []

-- | The lines above the first entry of a collection, unless the entry starts
-- below its indicator and has the lines there.
firstLines :: RenderOptions -> Node -> [Line]
firstLines opts x
  | firstStartsBelow opts x = []
  | otherwise = case x.content of
      SequenceContent _ (y : _) -> aboveIndicator opts y
      MappingContent _ ((k, v) : _) -> case implicitKey opts k of
        Just _ -> let (above, _, _) = entryComments opts k v in above
        Nothing -> aboveIndicator opts k
      _ -> []

-- | A block collection starts on the line after its indicator if it has
-- properties or a comment on the line of the indicator.
startsBelow :: RenderOptions -> Node -> Bool
startsBelow opts x = isBlock opts x && (isJust (props x) || isJust x.comments.inline)

-- | The first entry of a collection starts below its indicator.
firstStartsBelow :: RenderOptions -> Node -> Bool
firstStartsBelow opts x = case x.content of
  SequenceContent _ (y : _) -> startsBelow opts y
  MappingContent _ ((k, _) : _) -> startsBelow opts k
  _ -> False

-- | The lines up to the last empty line, and the lines after it.
splitAtLastEmptyLine :: [Line] -> ([Line], [Line])
splitAtLastEmptyLine ls =
  let (rest, own) = break (== EmptyLine) (reverse ls)
  in (reverse own, reverse rest)

-- | The lines above an entry with an implicit key, the comment on its line
-- and the lines below the key. A line holds one comment. If the value is on
-- the line of the key, the lines above the value and the comment of the key
-- go above the entry. Otherwise the comment of the value goes below the key.
--
-- A scalar key has no place for the lines after it, so they go below the
-- key: between the key and a block collection value, or below the entry, as
-- in @value@. They read back as the lines of the value. Below a block scalar
-- they would be part of the scalar, and below a flow collection they would
-- read back as the lines of the next entry, so they go above the entry.
entryComments :: RenderOptions -> Node -> Node -> ([Line], Maybe T.Text, [Line])
entryComments opts k v
  | isBlock opts v = case (k.comments.inline, v.comments.inline) of
      (Just kc, Just vc) -> (k.comments.before, Just kc, keyAfter ++ [Comment vc])
      (kc, vc) -> (k.comments.before, kc <|> vc, keyAfter)
  -- For a scalar value, only the place of the lines after the key differs.
  | not (isScalarLike v) || not (null keyAfter) && isBlockScalarNode v = case (k.comments.inline, v.comments.inline) of
      (Just kc, Just vc) -> (k.comments.before ++ keyAfter ++ v.comments.before ++ [Comment kc], Just vc, [])
      (kc, vc) -> (k.comments.before ++ keyAfter ++ v.comments.before, vc <|> kc, [])
  | otherwise = case (k.comments.inline, v.comments.inline) of
      (Just kc, Just vc) -> (k.comments.before ++ v.comments.before ++ [Comment kc], Just vc, keyAfter)
      (kc, vc) -> (k.comments.before ++ v.comments.before, vc <|> kc, keyAfter)
  where
    keyAfter :: [Line]
    keyAfter
      | isScalarLike k = k.comments.after
      | otherwise = []

-- | A scalar that the renderer writes in the literal or the folded style. A
-- text that a block scalar cannot hold goes in double quotes.
isBlockScalarNode :: Node -> Bool
isBlockScalarNode n = case n.content of
  ScalarContent Literal t -> isJust (literalBlock True 0 t)
  ScalarContent Folded t -> isJust (foldedBlock 0 [] t)
  _ -> False

-- | A scalar or an alias.
isScalarLike :: Node -> Bool
isScalarLike n = case n.content of
  SequenceContent {} -> False
  MappingContent {} -> False
  _ -> True

-- | Where an inline node is. A scalar in a key is on one line.
data Position = InValue | InKey | InFlow | InFlowKey
  deriving stock (Eq)

-- | A node with the given comment at the end of its last line. The lines of
-- a scalar after the first one are at the given indentation.
inline :: RenderOptions -> Position -> Int -> Node -> Maybe T.Text -> B.Builder
inline opts pos indent n lineComment = case n.content of
  AliasContent name -> "*" <> B.fromText name <> comment lineComment
  ScalarLinesContent style t starts
    | isBlockScalar style && pos == InValue -> withProps (blockScalar style t starts)
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
      ScalarContent Plain t -> T.null t
      _ -> False

    -- The comment goes on the line of the header.
    blockScalar :: ScalarStyle -> T.Text -> [Int] -> B.Builder
    blockScalar style t starts = case style of
      Literal | Just (h, b) <- literalBlock True indent t -> h <> comment lineComment <> b
      Folded | Just (h, b) <- foldedBlock indent starts t -> h <> comment lineComment <> b
      _ -> doubleQuotedLines indent starts t <> comment lineComment

    content_ :: B.Builder
    content_ = case n.content of
      ScalarLinesContent style t starts -> scalar (if inKey then [] else starts) style t
      SequenceContent _ [] | hasEndLines n -> "[\n" <> lines_ indent (fst (bracketLines n)) <> spaces indent <> "]"
      MappingContent _ [] | hasEndLines n -> "{\n" <> lines_ indent (fst (bracketLines n)) <> spaces indent <> "}"
      SequenceContent _ xs -> "[" <> commas (map (\x -> inline opts itemPos indent (flowItem x) Nothing) xs) <> "]"
      MappingContent _ kvs -> "{" <> commas (map flowEntry kvs) <> "}"
      AliasContent {} -> mempty

    inKey :: Bool
    inKey = pos == InKey || pos == InFlowKey

    -- The items of a flow collection in a key are in the key too.
    itemPos :: Position
    itemPos = if inKey then InFlowKey else InFlow

    -- An empty scalar cannot be an item of a flow sequence.
    flowItem :: Node -> Node
    flowItem x = case x.content of
      ScalarContent Plain ""
        | Props Nothing NoTag <- x.props ->
            x {props = Props Nothing (Tag (coreTagPrefix <> "null"))}
      _ -> x

    flowEntry :: (Node, Node) -> B.Builder
    flowEntry (k, v) =
      mconcat
        [ inline opts InFlowKey indent k Nothing
        , if endsWithName k then " :" else ":"
        , if isEmpty v then mempty else " " <> inline opts itemPos indent v Nothing
        ]

    commas :: [B.Builder] -> B.Builder
    commas = \case
      [] -> mempty
      b : bs -> b <> mconcat (map (", " <>) bs)

    -- A scalar in its style, or in a style that can hold its text, on the
    -- lines that start at the positions. The lines after the first one are at
    -- the indentation.
    scalar :: [Int] -> ScalarStyle -> T.Text -> B.Builder
    scalar starts style t = case style of
      Plain
        | T.null t -> mempty
        | null starts -> if plainSyntax inFlow t then B.fromText t else quotedPlain t
        | Just b <- plainLines inFlow indent starts t -> b
        | otherwise -> quotedPlainLines indent starts t
      SingleQuoted -> fromMaybe (doubleQuotedLines indent starts t) (singleQuotedLines indent starts t)
      _ -> doubleQuotedLines indent starts t
      where
        inFlow :: Bool
        inFlow = pos == InFlow || pos == InFlowKey

-- | A key on one line, or 'Nothing' if it needs an explicit entry.
implicitKey :: RenderOptions -> Node -> Maybe B.Builder
implicitKey opts k
  | isBlock opts k = Nothing
  | isEmpty k = Nothing
  -- An empty collection with lines inside is on several lines.
  | hasEndLines k, not (isScalarLike k) = Nothing
  | T.length key > maxImplicitKeyLength = Nothing
  | otherwise = Just (B.fromText key)
  where
    key :: T.Text
    key = case (k.props, k.content) of
      (Props Nothing NoTag, ScalarContent Plain t) | plainSyntax False t -> t
      _ -> B.runBuilder $ inline opts InKey 0 k Nothing <> if endsWithName k then " " else mempty

-- | The node is a collection that the renderer writes in the block style.
isBlock :: RenderOptions -> Node -> Bool
isBlock opts n = case n.content of
  SequenceContent style (_ : _) -> style == Block || opts.forceBlock
  MappingContent style (_ : _) -> style == Block || opts.forceBlock
  _ -> False

-- | The node has comments at its end, which go between the brackets of an
-- empty collection and below a scalar or an alias.
hasEndLines :: Node -> Bool
hasEndLines n = case n.content of
  SequenceContent _ (_ : _) -> False
  MappingContent _ (_ : _) -> False
  _ -> hasCommentLine n.comments.after

-- | The lines have a comment, not only empty lines.
hasCommentLine :: [Line] -> Bool
hasCommentLine = any (/= EmptyLine)

-- | The lines at the end of a scalar or an alias at the given indentation.
-- They cannot be deeper, because a block scalar would take them in.
linesBelow :: Int -> Node -> B.Builder
linesBelow indent n = case n.content of
  ScalarContent {} -> lines_ indent n.comments.after
  AliasContent {} -> lines_ indent n.comments.after
  SequenceContent _ [] -> lines_ indent (snd (bracketLines n))
  MappingContent _ [] -> lines_ indent (snd (bracketLines n))
  _ -> mempty

-- | The lines of an empty flow collection inside its brackets, and the
-- empty lines at their end, which go below the collection. The parser gives
-- the empty lines before a closing bracket to the node below.
bracketLines :: Node -> ([Line], [Line])
bracketLines n
  | hasEndLines n =
      let (empties, rest) = span (== EmptyLine) (reverse n.comments.after)
      in (reverse rest, empties)
  | otherwise = ([], n.comments.after)

-- | The node is an empty plain scalar without properties.
isEmpty :: Node -> Bool
isEmpty n = case (n.props, n.content) of
  (Props Nothing NoTag, ScalarContent Plain t) -> T.null t
  _ -> False

-- | The node ends with an alias, an anchor or a tag. A colon right after it
-- would be part of the name.
endsWithName :: Node -> Bool
endsWithName n = case n.content of
  AliasContent {} -> True
  ScalarContent Plain t -> T.null t && (isJust n.props.anchor || n.props.tag /= NoTag)
  _ -> False

-- | The anchor and the tag of a node.
props :: Node -> Maybe B.Builder
props n = case n.content of
  AliasContent {} -> Nothing
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
  Just t -> " #" <> text (T.stripEnd (T.map (\c -> if isCommentBreak c then ' ' else c) (printable t)))
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
      CommentLine n t ->
        let hashes = B.fromText (T.replicate (max 1 n) "#")
        in mconcat . map (commentLine hashes) $
             T.split isCommentBreak (T.replace "\r\n" "\n" (printable t))

    -- The parser drops the white space at the end of a comment.
    commentLine :: B.Builder -> T.Text -> B.Builder
    commentLine hashes l
      | T.null (T.stripEnd l) = spaces indent <> hashes <> "\n"
      | otherwise = spaces indent <> hashes <> " " <> B.fromText (T.stripEnd l) <> "\n"

-- | The mark of an empty line from the comments. The output has no other NUL
-- character.
emptyLine :: T.Text
emptyLine = "\0"

-- | The text of a comment with a replacement for the characters that YAML
-- does not allow.
printable :: T.Text -> T.Text
printable = T.map $ \c -> if c == '\t' || isCommentBreak c || isPrintable c then c else '\xFFFD'

-- | A line break in the text of a comment. YAML 1.1 reads U+0085, U+2028 and
-- U+2029 as line breaks, so the rest of a comment after one of them would
-- read as data.
isCommentBreak :: Char -> Bool
isCommentBreak c = c == '\n' || c == '\r' || c == '\x85' || c == '\x2028' || c == '\x2029'

-- $setup
-- >>> import Data.Text.IO qualified as T
-- >>> import Yamlet.Error
-- >>> import Yamlet.Syntax
