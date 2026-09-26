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
import GHC.Generics

import Yamlet.Internal.Parser.Chars
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
-- counts characters, not bytes. Line 0 and column 0 mean that the error has
-- no position, e.g. because it comes from a node that a program built.
data Location = Location
  { offset :: !Offset
  , line :: !Int
  , column :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Render an error in the format that editors recognize. The result does not
-- end with a line break. Of a line longer than 80 characters, the excerpt
-- shows only the 80 characters around the column.
--
-- @
-- config.yaml:3:5: expected a list, but got an integer
--   |
-- 3 |   - 42
--   |     ^
-- @
--
-- An error with no position gives only the file and the message, e.g.
-- @config.yaml: duplicate key \"a\"@.
prettyError :: FilePath -> Error -> String
prettyError file err
  | err.location.line == 0 = file ++ ": " ++ err.message
  | otherwise =
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
        , shown
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

    -- The usual width of a terminal.
    width :: Int
    width = 80

    full :: String
    full = T.unpack err.sourceLine

    start :: Int
    start = max 0 (min (err.location.column - 1 - width `div` 2) (length full - width))

    shown :: String
    shown
      | length full <= width = full
      | otherwise =
          (if start > 0 then ellipsis else "")
            ++ take width (drop start full)
            ++ (if start + width < length full then ellipsis else "")

    ellipsis :: String
    ellipsis = "..."

    before :: Int
    before
      | length full <= width = err.location.column - 1
      | otherwise = (if start > 0 then length ellipsis else 0) + err.location.column - 1 - start

    -- A tab before the column keeps the caret aligned in a terminal.
    caret :: String
    caret = map (\c -> if c == '\t' then '\t' else ' ') (take before shown)

-- | Create an error at the given offset of the input.
errorAt :: T.Text -> Offset -> String -> Error
errorAt input off msg =
  Error
    { location = loc
    , message = msg
    , sourceLine = if off == noOffset then T.empty else T.copy (lineAt input off)
    }
  where
    loc :: Location
    loc = locate input off

-- | Compute the line and the column of an offset. A byte order mark at the
-- start of a line is not a column, because it is not content. For
-- 'noOffset', the line and the column are 0.
locate :: T.Text -> Offset -> Location
locate input off
  | off == noOffset = Location {offset = off, line = 0, column = 0}
  | otherwise = locateIn input off

locateIn :: T.Text -> Offset -> Location
locateIn (T.Text arr base len) (Offset off0) = go base 1 base
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
          LF -> go (i + 1) (ln + 1) (i + 1)
          CR
            | i + 1 < base + len && A.unsafeIndex arr (i + 1) == LF ->
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
