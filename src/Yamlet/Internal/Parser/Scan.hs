{-# OPTIONS_HADDOCK not-home #-}

-- | Scans of the input around an index, outside of the parser monad.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Parser.Scan
  ( isBom
  , skipBoms
  , skipSpaces
  , skipWhites
  , breakEnd
  , isStartOfLine
  , lineStartAt
  , previousLineStart
  , contentLineAbove
  , markerLength
  , isMarker
  , isStartMarker
  , isEndMarker
  , startsPrefix
  , bomBeforeContent
  , fitsKey
  ) where

import Yamlet.Internal.Chars
import Yamlet.Internal.Parser.Monad
import Yamlet.Internal.Utils

isBom :: Env -> Int -> Bool
isBom e = isBomIn e.array e.end

skipBoms :: Env -> Int -> Int
skipBoms e = skipBomsIn e.array e.end

skipSpaces :: Env -> Int -> Int
skipSpaces e i = if byteAt e i == SPACE then skipSpaces e (i + 1) else i

skipWhites :: Env -> Int -> Int
skipWhites e i = if isWhite (byteAt e i) then skipWhites e (i + 1) else i

-- | The index after the line break at the index.
breakEnd :: Env -> Int -> Int
breakEnd e i
  | byteAt e i == CR && byteAt e (i + 1) == LF = i + 2
  | otherwise = i + 1

isStartOfLine :: Env -> Int -> Bool
isStartOfLine e i
  | i <= e.base = True
  | isBreak (byteBefore e i) = True
  | i - bomLength >= e.base && isBom e (i - bomLength) = isStartOfLine e (i - bomLength)
  | otherwise = False

-- | The start of the line that contains the index.
lineStartAt :: Env -> Int -> Int
lineStartAt e i
  | i > e.base && not (isBreak (byteBefore e i)) = lineStartAt e (i - 1)
  | otherwise = i

-- | The start of the line above the line that starts at the index, which is
-- not the first line.
previousLineStart :: Env -> Int -> Int
previousLineStart e i = lineStartAt e (breakStart (i - 1))
  where
    -- The start of the line break that ends at the index, e.g. of CR LF.
    breakStart :: Int -> Int
    breakStart j
      | j > e.base && byteBefore e j == CR && byteAt e j == LF = j - 1
      | otherwise = j

-- | The start of the closest line above the line that starts at the index
-- with content other than a comment. Byte order marks at the start of a line
-- do not count as content.
contentLineAbove :: Env -> Int -> Maybe Int
contentLineAbove e start
  | start <= e.base = Nothing
  | otherwise =
      let prev = previousLineStart e start
          b = byteAt e (skipWhites e (skipBoms e prev))
      in if isBreak b || b == HASH then contentLineAbove e prev else Just prev

-- | The number of characters of a @---@ or @...@ marker.
markerLength :: Int
markerLength = 3

-- | A @---@ or @...@ marker at the start of a line.
isMarker :: Env -> Int -> Bool
isMarker e i =
  let w = byteAt e i
      after = byteAt e (i + markerLength)
  in (w == MINUS || w == DOT)
       && all (\j -> byteAt e (i + j) == w) [1 .. markerLength - 1]
       && (after == 0 || isWhite after || isBreak after)
       && isStartOfLine e i

-- | A @---@ marker at the start of a line.
isStartMarker :: Env -> Int -> Bool
isStartMarker e i = isMarker e i && byteAt e i == MINUS

-- | A @...@ marker at the start of a line.
isEndMarker :: Env -> Int -> Bool
isEndMarker e i = isMarker e i && byteAt e i == DOT

-- | A byte order mark at the start of a line. Outside a quoted scalar, it
-- starts the prefix of the next document, so the content of a document ends
-- before it.
startsPrefix :: Env -> Int -> Bool
startsPrefix e i = isBom e i && isStartOfLine e i

-- | Byte order marks at the index before something other than a document
-- marker or a directive. Such marks at the start of a line inside a document
-- are an error.
bomBeforeContent :: Env -> Int -> Bool
bomBeforeContent e i =
  isBom e i && let j = skipBoms e i in not (isMarker e j || byteAt e j == PERCENT)

-- | The input between the indices fits in an implicit key.
fitsKey :: Env -> Int -> Int -> Bool
fitsKey e p q =
  q - p <= maxImplicitKeyLength
    || (q - p <= maxImplicitKeyLength * maxCharBytes && countChars <= maxImplicitKeyLength)
  where
    -- The longest UTF-8 encoding of a character.
    maxCharBytes :: Int
    maxCharBytes = 4

    countChars :: Int
    countChars =
      length
        [() | x <- [p .. q - 1], isCharStart (byteAt e x)]
