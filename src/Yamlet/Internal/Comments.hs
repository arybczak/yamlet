{-# OPTIONS_HADDOCK not-home #-}

-- | Attachment of comments and empty lines to the nodes of a document.
--
-- The rules are in the documentation of "Yamlet.Syntax". The parser skips
-- comments, so this module finds them again in the input, outside of the
-- scalars, and gives each one to a node.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Comments
  ( attachComments
  , gapEnd
  , linesAbove
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Array qualified as A

import Yamlet.Internal.Parser.Chars
import Yamlet.Internal.Parser.Monad hiding ((<|>))
import Yamlet.Internal.Syntax
import Yamlet.Internal.Utils

-- | A comment or an empty line. The indices are offsets of the input.
data Item = Item
  { at :: !Int
  -- ^ The index of the @#@, or of the start of the empty line.
  , lineStart :: !Int
  , own :: !Bool
  -- ^ Nothing else is on the line.
  , line :: !Line
  }

-- | Attach the comments of a document, and return the lines at its end that
-- belong to the next document. The flags tell if the document is the first
-- one and if another one follows it. The indices are the start of the lines
-- that belong to the document, its @---@ marker, the end of its root and its
-- end.
attachComments :: Env -> Bool -> Bool -> Int -> Maybe Int -> Int -> Int -> Document -> (Document, [Line])
attachComments e first hasNext start marker rootEnd end doc
  | not (mayHaveItems e start end) = (doc, [])
  | null items = (doc, [])
  | otherwise =
      ( doc
          { docComments =
              strictComments
                ((if first then dropWhile (== EmptyLine) else id) (map (.line) docItems))
                markerComment
                docEnd
          , root = root''
          }
      , next
      )
  where
    -- Nothing is above the first document, so the empty lines at its start
    -- separate it from nothing.
    items :: [Item]
    items =
      (if isJust marker || not first then id else dropWhile (\i -> isEmptyLine i && i.at < offsetOf doc.root.offset)) $
        scanItems e start end (skipRanges e doc.root)

    (docItems, afterMarker) = case marker of
      Just m -> span (\i -> i.at < m - e.base) items
      Nothing -> ([], items)

    rootStart, rootLine :: Int
    rootStart = offsetOf doc.root.offset
    rootLine = lineOf e rootStart

    -- The comment on the line of the marker, unless the root starts there.
    (markerComment, rest) = case (marker, afterMarker) of
      (Just m, i : is)
        | not i.own
        , i.lineStart == m - e.base
        , rootLine /= m - e.base
        , Comment t <- i.line ->
            (Just t, is)
      _ -> (Nothing, afterMarker)

    (root', leftover) = attachNode e (rootEnd - e.base) 0 (rootStart, rootLine) doc.root rest

    (below, afterEnd) = span (\i -> i.at < rootEnd - e.base) leftover

    -- The lines of a flow collection are inside its brackets, so the lines
    -- below a flow collection root belong to the document.
    holdsLines :: Bool
    holdsLines = case root'.content of
      Sequence Flow _ -> False
      Mapping Flow _ -> False
      _ -> True

    -- Without a @...@ marker, the first empty line ends the lines of the
    -- document if another document follows it.
    endLines, next :: [Line]
    (endLines, next)
      | doc.explicitEnd || not hasNext = (rootLines, [])
      | otherwise = break (== EmptyLine) rootLines
      where
        rootLines :: [Line]
        rootLines = (if holdsLines then root'.comments.after else []) ++ map (.line) below

    root'' :: Node
    root''
      | holdsLines =
          let c = root'.comments
          in Node root'.offset root'.endOffset root'.props (strictComments c.before c.inline (if doc.explicitEnd then endLines else atEnd endLines)) root'.content
      | otherwise = root'

    docEnd :: [Line]
    docEnd = atEnd ((if holdsLines then [] else endLines) ++ map (.line) afterEnd)

    -- The empty lines at the end of the stream belong to no node.
    atEnd :: [Line] -> [Line]
    atEnd ls
      | hasNext = ls
      | otherwise = reverse (dropWhile (== EmptyLine) (reverse ls))

-- | The start of the first line from the index that is empty or has more than
-- a comment or a @...@ marker. The index is the start of a line.
gapEnd :: Env -> Int -> Int
gapEnd e i
  | i < e.end && isMarker e b && byteAt e b == DOT = gapEnd e (nextLine b)
  | i < e.end && byteAt e (skipWhites e b) == HASH = gapEnd e (nextLine b)
  | otherwise = i
  where
    -- A byte order mark can start a line between documents.
    b :: Int
    b = skipBoms e i

    nextLine :: Int -> Int
    nextLine k
      | k >= e.end = k
      | isBreak (byteAt e k) = breakEnd e k
      | otherwise = nextLine (k + 1)

-- | The documents with the lines above the first one.
linesAbove :: [Line] -> [Document] -> [Document]
linesAbove ls = \case
  d : ds
    | not (null ls) ->
        let c = d.docComments
            !d' = d {docComments = strictComments (ls ++ c.before) c.inline c.after}
        in d' : ds
  ds -> ds

-- | Comments with their lists evaluated. The parser returns a document
-- without thunks, and a lazy list would keep the items of the input alive.
strictComments :: [Line] -> Maybe T.Text -> [Line] -> Comments
strictComments before inline after =
  let !before' = force before
      !after' = force after
  in Comments before' inline after'

isEmptyLine :: Item -> Bool
isEmptyLine i = case i.line of
  EmptyLine -> True
  Comment _ -> False

offsetOf :: Offset -> Int
offsetOf (Offset o) = o

-- | Attach the comments to a node and the nodes inside it. The limit is the
-- offset of the next node, and the column is the smallest one for the lines
-- after the last entry of a block collection. The pair is an offset at or
-- before the node and the start of its line.
attachNode :: Env -> Int -> Int -> (Int, Int) -> Node -> [Item] -> (Node, [Item])
attachNode e limit minColumn known n items0 = node `seq` items5 `seq` (node, items5)
  where
    node :: Node
    node =
      n
        { comments = strictComments [i.line | i <- pre, isJust own || not (isFallback i)] (own <|> fallback) afterLines
        , content = content'
        }

    s, en :: Int
    s = offsetOf n.offset
    en = offsetOf n.endOffset

    lineStart, column :: Int
    lineStart = lineFrom e known s
    column = s - lineStart

    -- The lines above the node. A comment at the end of a line that no node
    -- took, e.g. in "- # comment" above a mapping, belongs to the node. It is
    -- a line above the node if the node has a comment on its own line.
    (pre, items1) =
      let (ls, rest) = span (\i -> i.at < s) items0
      in case n.content of
           Sequence Block (_ : _) | startsLine -> toFirstEntry ls rest
           Mapping Block (_ : _) | startsLine -> toFirstEntry ls rest
           _ -> (ls, rest)

    -- A collection after "- " on the same line keeps the lines above the
    -- indicator, so that a comment above an item stays with the item. The walk
    -- goes back from the node, so that it stops at the indicator of an outer
    -- collection. A walk from the start of the line would cross the whole
    -- indentation for each nested collection, and the time would be quadratic.
    startsLine :: Bool
    startsLine = go (s + e.base)
      where
        go :: Int -> Bool
        go i
          | i == lineStart + e.base = True
          | isWhite (A.unsafeIndex e.array (i - 1)) = go (i - 1)
          | otherwise = False

    -- The lines on their own after the last empty line go to the first entry.
    toFirstEntry :: [Item] -> [Item] -> ([Item], [Item])
    toFirstEntry ls rest =
      let (ownLines, others) = span (.own) (reverse ls)
          (entry, kept) = break isEmptyLine ownLines
      in (reverse (kept ++ others), reverse entry ++ rest)

    fallbackItem :: Maybe Item
    fallbackItem = case reverse (filter (not . (.own)) pre) of
      i : _ -> Just i
      [] -> Nothing

    fallback :: Maybe T.Text
    fallback = fallbackItem >>= comment

    isFallback :: Item -> Bool
    isFallback i = maybe False (\f -> f.at == i.at) fallbackItem

    own :: Maybe T.Text
    own = header <|> trailing

    -- The comment on the line of a block scalar header.
    (header, items2) = case (n.content, items1) of
      (Scalar style _, i : is)
        | style == Literal || style == Folded
        , not i.own
        , i.lineStart == lineStart ->
            (comment i, is)
      _ -> (Nothing, items1)

    (content', items3) = case n.content of
      Sequence style xs ->
        let !(xs', is) = sequenceItems style xs items2 in (Sequence style xs', is)
      Mapping style kvs ->
        let !(kvs', is) = mappingEntries style kvs items2 in (Mapping style kvs', is)
      c -> (c, items2)

    -- The lines before the closing bracket come before the comment after it.
    (trailing, afterLines, items5) = case n.content of
      Sequence Block (_ : _) -> blockEnd
      Mapping Block (_ : _) -> blockEnd
      Sequence Flow _ -> flowEnd
      Mapping Flow _ -> flowEnd
      _ -> let (t, is) = trailingComment items3 in (t, [], is)

    blockEnd, flowEnd :: (Maybe T.Text, [Line], [Item])
    blockEnd =
      let (t, is) = trailingComment items3
          (ls, is') = blockAfter is
      in (t, ls, is')
    -- The empty lines before the bracket go to the node below, as after the
    -- last entry of a block collection. They go back after the comment
    -- after the bracket, which 'trailingComment' takes from the front.
    flowEnd =
      let (ls, empties, is) = flowAfter items3
          (t, is') = trailingComment is
      in (t, ls, empties ++ is')

    -- The comment at the end of the line of the node's end. A node that ends
    -- at the start of a line, e.g. a block scalar, ends on the line before,
    -- unless it is empty.
    trailingComment :: [Item] -> (Maybe T.Text, [Item])
    trailingComment = \case
      i : is
        | not i.own
        , i.at >= en
        , i.at < limit
        , i.lineStart < en || s == en
        , T.all (`elem` (" \t,:" :: String)) (between en i.at) ->
            (comment i, is)
      is -> (Nothing, is)

    -- The lines after the last entry, indented deep enough, and the empty
    -- lines between them.
    blockAfter :: [Item] -> ([Line], [Item])
    blockAfter is =
      let ok i = i.at < limit && i.own && (isEmptyLine i || i.at - i.lineStart >= max column minColumn)
          (taken, rest) = span ok is
          (empties, taken') = span isEmptyLine (reverse taken)
      in (map (.line) (reverse taken'), reverse empties ++ rest)

    -- The lines before the closing bracket.
    flowAfter :: [Item] -> ([Line], [Item], [Item])
    flowAfter is =
      let (taken, rest) = span (\i -> i.at < en) is
          (empties, taken') = span isEmptyLine (reverse taken)
      in (map (.line) (reverse taken'), reverse empties, rest)

    sequenceItems :: CollectionStyle -> [Node] -> [Item] -> ([Node], [Item])
    sequenceItems style = go []
      where
        -- The nodes are in reverse, so that the list is evaluated when the
        -- result is.
        go :: [Node] -> [Node] -> [Item] -> ([Node], [Item])
        go acc [] is = let !xs = reverse acc in (xs, is)
        go acc (x : rest) is =
          let !(x', is') = attachNode e (nextStart rest) itemColumn (s, lineStart) x is
              !(x'', is'')
                -- A list without indentation has no column of its own for the
                -- lines after its last item, so they stay with the list.
                | style == Block && not (null rest && minColumn > column) = linesBelow (nextStart rest) x' is'
                | otherwise = (x', is')
          in go (x'' : acc) rest is''

        nextStart :: [Node] -> Int
        nextStart = \case
          y : _ -> offsetOf y.offset
          [] -> if style == Flow then en else limit

        itemColumn :: Int
        itemColumn = if style == Flow then 0 else column + 1

    mappingEntries :: CollectionStyle -> [(Node, Node)] -> [Item] -> ([(Node, Node)], [Item])
    mappingEntries style = go []
      where
        -- The entries are in reverse, as in 'sequenceItems'.
        go :: [(Node, Node)] -> [(Node, Node)] -> [Item] -> ([(Node, Node)], [Item])
        go acc [] is = let !kvs = reverse acc in (kvs, is)
        go acc ((k, v) : rest) is =
          let !(k', is') = attachNode e (offsetOf v.offset) entryColumn (s, lineStart) k is
              !(v', is'') = attachNode e (nextStart rest) entryColumn (s, lineStart) v is'
              !(v'', is''')
                | style == Block = linesBelow (nextStart rest) v' is''
                | otherwise = (v', is'')
          in go ((k', v'') : acc) rest is'''

        nextStart :: [(Node, Node)] -> Int
        nextStart = \case
          (k, _) : _ -> offsetOf k.offset
          [] -> if style == Flow then en else limit

        entryColumn :: Int
        entryColumn = if style == Flow then 0 else column + 1

    -- The lines below a scalar or an alias in a block collection, before the
    -- limit, that are indented deeper than its entry, and the empty lines
    -- between them. They go after the node, as 'blockAfter' does for a
    -- collection. Below a block scalar, such a line is part of the scalar
    -- if it is indented as deep as its content.
    linesBelow :: Int -> Node -> [Item] -> (Node, [Item])
    linesBelow lim x is = case x.content of
      Sequence {} -> (x, is)
      Mapping {} -> (x, is)
      Scalar style _ | style == Literal || style == Folded -> (x, is)
      _ ->
        let ok i = i.at < lim && i.own && (isEmptyLine i || i.at - i.lineStart > column)
            (taken, rest) = span ok is
            (empties, taken') = span isEmptyLine (reverse taken)
        in case taken' of
             [] -> (x, is)
             _ ->
               let !x' = Node x.offset x.endOffset x.props (strictComments x.comments.before x.comments.inline (map (.line) (reverse taken'))) x.content
               in (x', reverse empties ++ rest)

    between :: Int -> Int -> T.Text
    between i j = slice e (i + e.base) (j + e.base)

comment :: Item -> Maybe T.Text
comment i = case i.line of
  Comment t -> Just t
  EmptyLine -> Nothing

-- | The offset of the start of the line with the given offset.
lineOf :: Env -> Int -> Int
lineOf e o = go (o + e.base) - e.base
  where
    go :: Int -> Int
    go i
      | i > e.base && not (isBreak (A.unsafeIndex e.array (i - 1))) = go (i - 1)
      | otherwise = i

-- | The offset of the start of the line with the second offset. The walk stops
-- at the first offset of the pair, and the pair gives the start of its line.
-- Without it, each nested block collection of a long line would walk back to
-- the start of the line, and the time would be quadratic.
lineFrom :: Env -> (Int, Int) -> Int -> Int
lineFrom e (p, ls) o = go (o + e.base)
  where
    go :: Int -> Int
    go i
      | i == p + e.base = ls
      | i > e.base && not (isBreak (A.unsafeIndex e.array (i - 1))) = go (i - 1)
      | otherwise = i - e.base

-- | A quick check for a comment or an empty line between the indices.
mayHaveItems :: Env -> Int -> Int -> Bool
mayHaveItems e = go True
  where
    go :: Bool -> Int -> Int -> Bool
    go blank i stop
      | i >= stop = False
      | otherwise = case A.unsafeIndex e.array i of
          HASH -> True
          w
            | isBreak w -> blank || go True (i + 1) stop
            | isWhite w -> go blank (i + 1) stop
            | otherwise -> go False (i + 1) stop

-- | The ranges of the scalars, which cannot contain comments, in the order of
-- the input. The range of a block scalar starts after its header.
skipRanges :: Env -> Node -> [(Int, Int)]
skipRanges e root = go root []
  where
    go :: Node -> [(Int, Int)] -> [(Int, Int)]
    go n acc = case n.content of
      Scalar style _
        | style == Literal || style == Folded ->
            let s = nextLine (offsetOf n.offset + e.base)
            in if s < en then (s, en) : acc else acc
        | offsetOf n.offset + e.base < en -> (offsetOf n.offset + e.base, en) : acc
        where
          en :: Int
          en = offsetOf n.endOffset + e.base
      Sequence _ xs -> foldr go acc xs
      Mapping _ kvs -> foldr (\(k, v) a -> go k (go v a)) acc kvs
      _ -> acc

    nextLine :: Int -> Int
    nextLine i
      | i >= e.end = i
      | isBreak (A.unsafeIndex e.array i) = i + 1
      | otherwise = nextLine (i + 1)

-- | The comments and the empty lines between the indices, outside the given
-- ranges. The offsets of the items are relative to the start of the input.
scanItems :: Env -> Int -> Int -> [(Int, Int)] -> [Item]
scanItems e start stop = go start start False False
  where
    -- The flags tell if the line has something other than white space and if
    -- the previous line was empty.
    go :: Int -> Int -> Bool -> Bool -> [(Int, Int)] -> [Item]
    go i ls content prevEmpty ranges
      | i >= stop = []
      | (rs, re) : rest <- ranges
      , rs <= i =
          if re > i
            -- A block scalar can end at the start of a line.
            then let ls' = lineBefore i re ls in go re ls' (ls' /= re) False rest
            else go i ls content prevEmpty rest
      -- The parser allows byte order marks at the start of a line only
      -- between documents, where a comment can follow them.
      | i == ls
      , isBom e i =
          let j = skipBoms e i in go j j content prevEmpty ranges
      | otherwise = case A.unsafeIndex e.array i of
          w
            | isBreak w ->
                let next =
                      if w == CR && i + 1 < stop && A.unsafeIndex e.array (i + 1) == LF
                        then i + 2
                        else i + 1
                    blank = not content
                    item = [Item (ls - e.base) (ls - e.base) True EmptyLine | blank, not prevEmpty]
                in item ++ go next next False blank ranges
            | w == HASH && (i == ls || isWhite (A.unsafeIndex e.array (i - 1))) ->
                let eol = lineEnd i
                    -- A comment at the end of a line keeps its text after
                    -- the first #, because 'Comments' has no count for it.
                    textStart = if content then i + 1 else hashesEnd i
                    text = T.stripEnd . dropSpace $ slice e textStart eol
                in Item (i - e.base) (ls - e.base) (not content) (CommentLine (textStart - i) text)
                     : go eol ls True prevEmpty ranges
            | isWhite w -> go (i + 1) ls content prevEmpty ranges
            | otherwise -> go (i + 1) ls True prevEmpty ranges

    hashesEnd :: Int -> Int
    hashesEnd i
      | i < stop && A.unsafeIndex e.array i == HASH = hashesEnd (i + 1)
      | otherwise = i

    lineEnd :: Int -> Int
    lineEnd i
      | i < stop && not (isBreak (A.unsafeIndex e.array i)) = lineEnd (i + 1)
      | otherwise = i

    -- The start of the line of the second index, or the given start if no
    -- line break is between the indices.
    lineBefore :: Int -> Int -> Int -> Int
    lineBefore i j ls
      | j <= i = ls
      | isBreak (A.unsafeIndex e.array (j - 1)) = j
      | otherwise = lineBefore i (j - 1) ls

    dropSpace :: T.Text -> T.Text
    dropSpace t = fromMaybe t (textStripPrefix " " t)
