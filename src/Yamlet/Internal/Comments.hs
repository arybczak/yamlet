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
  ) where

import Control.Applicative
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Word

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

-- | Attach the comments of a document. The indices are the start of the
-- lines that belong to the document, its @---@ marker and its end.
attachComments :: Env -> Int -> Maybe Int -> Int -> Document -> Document
attachComments e start marker end doc
  | not (mayHaveItems e start end) = doc
  | null items = doc
  | otherwise =
      doc
        { docComments =
            Comments
              { before = dropWhile (== EmptyLine) (map (.line) docItems)
              , inline = markerComment
              , after = map (.line) leftover
              }
        , root = root'
        }
  where
    items :: [Item]
    items =
      (if isJust marker then id else dropWhile (\i -> isEmptyLine i && i.at < offsetOf doc.root.offset)) $
        scanItems e start end (skipRanges e doc.root)

    (docItems, afterMarker) = case marker of
      Just m -> span (\i -> i.at < m - e.base) items
      Nothing -> ([], items)

    -- The comment on the line of the marker, unless the root starts there.
    (markerComment, rest) = case (marker, afterMarker) of
      (Just m, i : is)
        | not i.own
        , i.lineStart == m - e.base
        , lineOf e (offsetOf doc.root.offset) /= m - e.base
        , Comment t <- i.line ->
            (Just t, is)
      _ -> (Nothing, afterMarker)

    (root', leftover) = attachNode e (end - e.base) 0 doc.root rest

isEmptyLine :: Item -> Bool
isEmptyLine i = case i.line of
  EmptyLine -> True
  Comment _ -> False

offsetOf :: Offset -> Int
offsetOf (Offset o) = o

-- | Attach the comments to a node and the nodes inside it. The limit is the
-- offset of the next node, and the column is the smallest one for the lines
-- after the last entry of a block collection.
attachNode :: Env -> Int -> Int -> Node -> [Item] -> (Node, [Item])
attachNode e limit minColumn n items0 =
  ( n
      { comments =
          Comments
            { before = [i.line | i <- pre, not (isFallback i)]
            , inline = fallback <|> header <|> trailing
            , after = afterLines
            }
      , content = content'
      }
  , items5
  )
  where
    s, en :: Int
    s = offsetOf n.offset
    en = offsetOf n.endOffset

    -- The lines above the node. A comment at the end of a line that no node
    -- took, e.g. in "- # comment" above a mapping, belongs to the node.
    (pre, items1) = span (\i -> i.at < s) items0
    fallbackItem :: Maybe Item
    fallbackItem = case reverse (filter (not . (.own)) pre) of
      i : _ -> Just i
      [] -> Nothing

    fallback :: Maybe T.Text
    fallback = fallbackItem >>= comment

    isFallback :: Item -> Bool
    isFallback i = maybe False (\f -> f.at == i.at) fallbackItem

    -- The comment on the line of a block scalar header.
    (header, items2) = case (n.content, items1) of
      (Scalar style _, i : is)
        | style == Literal || style == Folded
        , not i.own
        , i.lineStart == lineOf e s ->
            (comment i, is)
      _ -> (Nothing, items1)

    (content', items3) = case n.content of
      Sequence style xs ->
        let (xs', is) = sequenceItems style xs items2 in (Sequence style xs', is)
      Mapping style kvs ->
        let (kvs', is) = mappingEntries style kvs items2 in (Mapping style kvs', is)
      c -> (c, items2)

    -- The comment at the end of the line of the node's end.
    (trailing, items4) = case items3 of
      i : is
        | not i.own
        , i.at >= en
        , i.at < limit
        , i.lineStart <= en
        , T.all (`elem` (" \t,:" :: String)) (between en i.at) ->
            (comment i, is)
      _ -> (Nothing, items3)

    (afterLines, items5) = case n.content of
      Sequence Block (_ : _) -> blockAfter
      Mapping Block (_ : _) -> blockAfter
      Sequence Flow _ -> flowAfter
      Mapping Flow _ -> flowAfter
      _ -> ([], items4)

    -- The lines after the last entry, indented deep enough, and the empty
    -- lines between them.
    blockAfter :: ([Line], [Item])
    blockAfter =
      let column = max (columnOf e s) minColumn
          ok i = i.at < limit && i.own && (isEmptyLine i || i.at - i.lineStart >= column)
          (taken, rest) = span ok items4
          (empties, taken') = span isEmptyLine (reverse taken)
      in (map (.line) (reverse taken'), reverse empties ++ rest)

    -- The lines before the closing bracket.
    flowAfter :: ([Line], [Item])
    flowAfter =
      let (taken, rest) = span (\i -> i.at < en) items4
      in (map (.line) taken, rest)

    sequenceItems :: CollectionStyle -> [Node] -> [Item] -> ([Node], [Item])
    sequenceItems style = go
      where
        go :: [Node] -> [Item] -> ([Node], [Item])
        go [] is = ([], is)
        go (x : rest) is =
          let (x', is') = attachNode e (nextStart rest) itemColumn x is
              (rest', is'') = go rest is'
          in (x' : rest', is'')

        nextStart :: [Node] -> Int
        nextStart = \case
          y : _ -> offsetOf y.offset
          [] -> if style == Flow then en else limit

        itemColumn :: Int
        itemColumn = if style == Flow then 0 else columnOf e s + 1

    mappingEntries :: CollectionStyle -> [(Node, Node)] -> [Item] -> ([(Node, Node)], [Item])
    mappingEntries style = go
      where
        go :: [(Node, Node)] -> [Item] -> ([(Node, Node)], [Item])
        go [] is = ([], is)
        go ((k, v) : rest) is =
          let column = if style == Flow then 0 else columnOf e s + 1
              (k', is') = attachNode e (offsetOf v.offset) column k is
              (v', is'') = attachNode e (nextStart rest) column v is'
              (rest', is''') = go rest is''
          in ((k', v') : rest', is''')

        nextStart :: [(Node, Node)] -> Int
        nextStart = \case
          (k, _) : _ -> offsetOf k.offset
          [] -> if style == Flow then en else limit

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

columnOf :: Env -> Int -> Int
columnOf e o = o - lineOf e o

-- | A quick check for a comment or an empty line between the indices.
mayHaveItems :: Env -> Int -> Int -> Bool
mayHaveItems e = go True
  where
    go :: Bool -> Int -> Int -> Bool
    go blank i stop
      | i >= stop = False
      | otherwise = case A.unsafeIndex e.array i of
          0x23 -> True
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
      | otherwise = case A.unsafeIndex e.array i of
          w
            | isBreak w ->
                let next =
                      if w == 0x0D && i + 1 < stop && A.unsafeIndex e.array (i + 1) == 0x0A
                        then i + 2
                        else i + 1
                    blank = not content
                    item = [Item (ls - e.base) (ls - e.base) True EmptyLine | blank, not prevEmpty]
                in item ++ go next next False blank ranges
            | w == 0x23 && (i == ls || isWhite (A.unsafeIndex e.array (i - 1))) ->
                let eol = lineEnd i
                    text = T.stripEnd . dropSpace $ slice e (i + 1) eol
                in Item (i - e.base) (ls - e.base) (not content) (Comment text)
                     : go eol ls True prevEmpty ranges
            | isWhite w -> go (i + 1) ls content prevEmpty ranges
            | otherwise -> go (i + 1) ls True prevEmpty ranges

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

isBreak :: Word8 -> Bool
isBreak w = w == 0x0A || w == 0x0D

isWhite :: Word8 -> Bool
isWhite w = w == 0x20 || w == 0x09
