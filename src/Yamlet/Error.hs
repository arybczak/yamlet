{-# LANGUAGE DeriveAnyClass #-}

-- | Errors with the position in the input that caused them.
module Yamlet.Error
  ( -- * Errors
    Error (..)
  , Location (..)
  , prettyError

    -- * Construction
  , errorAt
  , locate
  ) where

import Control.DeepSeq
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Internal qualified as T
import Data.Word
import GHC.Generics

import Yamlet.Internal.Syntax

-- | An error of the parser or the decoder.
data Error = Error
  { location :: !Location
  , message :: !String
  , sourceLine :: !T.Text
  -- ^ The line of the input that contains the location.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A position in the input. Lines and columns count from 1, and a column
-- counts characters, not bytes.
data Location = Location
  { offset :: !Offset
  , line :: !Int
  , column :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Render an error in the format that editors recognize. The result does not
-- end with a line break.
--
-- @
-- config.yaml:3:5: expected a list, but got an integer
--   |
-- 3 |   - 42
--   |     ^
-- @
prettyError :: FilePath -> Error -> String
prettyError file err =
  concat
    [ file
    , ":"
    , show err.location.line
    , ":"
    , show err.location.column
    , ": "
    , err.message
    , "\n"
    , pad
    , " |\n"
    , lineNo
    , " | "
    , T.unpack err.sourceLine
    , "\n"
    , pad
    , " | "
    , caret
    , "^"
    ]
  where
    lineNo :: String
    lineNo = show err.location.line

    pad :: String
    pad = map (const ' ') lineNo

    -- A tab before the column keeps the caret aligned in a terminal.
    caret :: String
    caret =
      map (\c -> if c == '\t' then '\t' else ' ')
        . take (err.location.column - 1)
        $ T.unpack err.sourceLine

-- | Create an error at the given offset of the input.
errorAt :: T.Text -> Offset -> String -> Error
errorAt input off msg =
  Error
    { location = loc
    , message = msg
    , sourceLine = T.copy (lineAt input off)
    }
  where
    loc :: Location
    loc = locate input off

-- | Compute the line and the column of an offset. A byte order mark at the
-- start of a line is not a column, because it is not content.
locate :: T.Text -> Offset -> Location
locate (T.Text arr base len) (Offset off0) = go base 1 base
  where
    off :: Int
    off = base + max 0 (min len off0)

    go :: Int -> Int -> Int -> Location
    go i !ln lineStart
      | i >= off =
          Location
            { offset = Offset (off - base)
            , line = ln
            , column = 1 + countChars (min off (skipBom arr (base + len) lineStart)) off
            }
      | otherwise = case A.unsafeIndex arr i of
          10 -> go (i + 1) (ln + 1) (i + 1)
          13
            | i + 1 < base + len && A.unsafeIndex arr (i + 1) == 10 ->
                go (i + 1) ln lineStart
            | otherwise -> go (i + 1) (ln + 1) (i + 1)
          _ -> go (i + 1) ln lineStart

    countChars :: Int -> Int -> Int
    countChars i0 i1 =
      length
        [() | i <- [i0 .. i1 - 1], A.unsafeIndex arr i < 0x80 || A.unsafeIndex arr i >= 0xC0]

-- | The index after a byte order mark at the index, or the index.
skipBom :: A.Array -> Int -> Int -> Int
skipBom arr end i
  | i + 3 <= end
      && A.unsafeIndex arr i == 0xEF
      && A.unsafeIndex arr (i + 1) == 0xBB
      && A.unsafeIndex arr (i + 2) == 0xBF =
      i + 3
  | otherwise = i

-- | The line of the input that contains the offset, without the line break
-- and without a byte order mark at its start.
lineAt :: T.Text -> Offset -> T.Text
lineAt (T.Text arr base len) (Offset off0) = T.Text arr start (stop - start)
  where
    end, off, start, stop :: Int
    end = base + len
    off = base + max 0 (min len off0)
    start = skipBom arr end (findStart off)
    stop = max start (findStop off)

    findStart :: Int -> Int
    findStart i
      | i > base && not (isBreak (A.unsafeIndex arr (i - 1))) = findStart (i - 1)
      | otherwise = i

    findStop :: Int -> Int
    findStop i
      | i < end && not (isBreak (A.unsafeIndex arr i)) = findStop (i + 1)
      | otherwise = i

    isBreak :: Word8 -> Bool
    isBreak w = w == 10 || w == 13
