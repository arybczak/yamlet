-- | Errors with the position in the input that caused them.
module Yamlet.Error
  ( -- * Errors
    Error (..)
  , Location (..)
  , Path
  , pathElements
  , pathFromElements
  , PathElement (..)
  , prettyError
  , renderPath

    -- * Construction
  , errorAt
  , errorsAt
  , documentErrors
  , locate
  , nodePath
  , nodePaths
  ) where

import Control.DeepSeq
import Data.Char
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Internal qualified as T
import GHC.Generics
import Numeric

import Yamlet.Internal.Chars
import Yamlet.Internal.Syntax

-- | An error of the parser or the decoder.
--
-- An error from the functions of this module keeps no part of the input
-- alive once it is in weak head normal form. Its message and its path are
-- evaluated then, because a message often contains a slice of the input,
-- e.g. a key, and a path comes from the syntax tree.
data Error = Error
  { location :: !Location
  , message :: !String
  , sourceLine :: !T.Text
  -- ^ The line of the input that contains the location.
  , sourceIndex :: !Int
  -- ^ The index of the location in the UTF-8 bytes of 'sourceLine'. It
  -- lets 'prettyError' find the column without a scan of the whole line.
  , path :: !Path
  -- ^ The path to the node of a decoder error. An error at a key has the
  -- path of its mapping. The path is empty for an error of the parser and
  -- for a node that a program built.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The keys and the indices from the root of a document to a node.
--
-- The path of a node shares the path of its parent, so the paths of many
-- errors in a deep document take memory linear in its size.
data Path
  = Root
  | Child !Path !PathElement
  deriving stock (Eq)

-- Written by hand, because every field is strict and has no lazy parts, and
-- a generic instance would walk the shared paths of all errors.
instance NFData Path where
  rnf = rwhnf

instance Show Path where
  showsPrec d p = showParen (d > 10) $ showString "pathFromElements " . shows (pathElements p)

-- | The steps of a path, from the root.
pathElements :: Path -> [PathElement]
pathElements = go []
  where
    go :: [PathElement] -> Path -> [PathElement]
    go acc = \case
      Root -> acc
      Child p e -> go (e : acc) p

-- | A path with the steps from the root.
pathFromElements :: [PathElement] -> Path
pathFromElements = L.foldl' Child Root

-- | A step of a path into a document.
data PathElement
  = -- | The value of a key that is a scalar, with the text of the key, e.g.
    -- @1@ for the integer key 1.
    Key !T.Text
  | -- | The item of a sequence, from 0.
    Index !Int
  | -- | The value of a key that is a collection, e.g. @? [1, 2]@. The path
    -- has no steps into the key.
    CollectionKey
  | -- | The value of a key that is an alias, with the name of the anchor.
    AliasKey !T.Text
  deriving stock (Eq, Show, Generic)

-- Written by hand, because GHC does not always remove the generic
-- representation of a sum type. Every field is strict and has no lazy parts.
instance NFData PathElement where
  rnf = rwhnf

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
-- end with a line break. If the line is longer than 80 characters, the
-- excerpt shows only the 80 characters around the column.
--
-- >>> either printErrors print (decodeText @(M.Map T.Text [[Int]]) "jobs:\n  - [1]\n  - 42\n")
-- input.yaml:3:5: jobs[1]: expected a list, but got an integer
--   |
-- 3 |   - 42
--   |     ^
--
-- An error with no position gives only the file and the message, e.g.
-- @input.yaml: duplicate key \"a\"@.
prettyError :: FilePath -> Error -> String
prettyError file err
  | err.location.line == 0 = file ++ ": " ++ message
  | otherwise =
      concat
        [ file
        , ":"
        , show err.location.line
        , ":"
        , show err.location.column
        , ": "
        , message
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
    message :: String
    message
      | err.path == Root = err.message
      | otherwise = renderPath err.path ++ ": " ++ err.message

    lineNo :: String
    lineNo = show err.location.line

    pad :: String
    pad = map (const ' ') lineNo

    -- The usual width of a terminal.
    width :: Int
    width = 80

    -- The characters of the line before and from the location. Each count
    -- stops one past the width, so that a long line takes no longer.
    back, ahead :: Int
    back = fst (stepBack (width + 1))
    ahead = fst (stepAhead (width + 1))

    short :: Bool
    short = back + ahead <= width

    -- The characters of the excerpt before the location.
    inExcerpt :: Int
    inExcerpt = min back (max (width `div` 2) (width - ahead))

    cutBefore, cutAfter :: Bool
    cutBefore = back > inExcerpt
    cutAfter = ahead > width - inExcerpt

    shown :: String
    shown
      | short = T.unpack err.sourceLine
      | otherwise =
          (if cutBefore then ellipsis else "")
            ++ T.unpack (T.Text arr excerptStart (excerptEnd - excerptStart))
            ++ (if cutAfter then ellipsis else "")
      where
        excerptStart, excerptEnd :: Int
        excerptStart = snd (stepBack inExcerpt)
        excerptEnd = snd (stepAhead (width - inExcerpt))

    ellipsis :: String
    ellipsis = "..."

    before :: Int
    before
      | short = back
      | otherwise = (if cutBefore then length ellipsis else 0) + inExcerpt

    T.Text arr lineStart lineLen = err.sourceLine

    lineEnd :: Int
    lineEnd = lineStart + lineLen

    -- The index of the location in the array, at the start of a character.
    index :: Int
    index = charStart (lineStart + max 0 (min lineLen err.sourceIndex))

    charStart :: Int -> Int
    charStart i
      | i > lineStart && i < lineEnd && not (isCharStart (A.unsafeIndex arr i)) = charStart (i - 1)
      | otherwise = i

    -- Step over at most the given number of characters before or from the
    -- location. Give the number of steps and the index in the array.
    stepBack, stepAhead :: Int -> (Int, Int)
    stepBack = go index 0
      where
        go :: Int -> Int -> Int -> (Int, Int)
        go i !k n
          | n == 0 || i <= lineStart = (k, i)
          | otherwise = go (charStart (i - 1)) (k + 1) (n - 1)
    stepAhead = go index 0
      where
        go :: Int -> Int -> Int -> (Int, Int)
        go i !k n
          | n == 0 || i >= lineEnd = (k, i)
          | otherwise = go (charEnd (i + 1)) (k + 1) (n - 1)

        charEnd :: Int -> Int
        charEnd i = if i < lineEnd && not (isCharStart (A.unsafeIndex arr i)) then charEnd (i + 1) else i

    -- A tab before the column keeps the caret aligned in a terminal.
    caret :: String
    caret = map (\c -> if c == '\t' then '\t' else ' ') (take before shown)

-- | A path in the form @jobs[1].name@. A key that is a collection is @?@,
-- and a key that is an alias is its alias, e.g. @*base@.
--
-- A key is in double quotes, e.g. @\"a.b\"@, if it:
--
-- * is empty,
--
-- * has white space, a character that cannot be printed, or one of the
--   characters @.[]\"\\@,
--
-- * starts with @?@ or @*@.
--
-- In the quotes, a character that cannot be printed has an escape as in
-- YAML, e.g. @\"a\\nb\"@.
--
-- >>> renderPath (pathFromElements [Key "jobs", Index 1, Key "name"])
-- "jobs[1].name"
--
-- >>> renderPath (pathFromElements [Key "a.b", Key ""])
-- "\"a.b\".\"\""
renderPath :: Path -> String
renderPath path = case pathElements path of
  [] -> ""
  e : rest -> step e ++ concatMap next rest
  where
    next :: PathElement -> String
    next = \case
      Index i -> index i
      e -> "." ++ step e

    step :: PathElement -> String
    step = \case
      Key k -> key k
      Index i -> index i
      CollectionKey -> "?"
      AliasKey name -> '*' : T.unpack name

    key :: T.Text -> String
    key k
      | not (T.null k) && T.all plain k && not (T.isPrefixOf "?" k || T.isPrefixOf "*" k) = T.unpack k
      | otherwise = "\"" ++ concatMap escape (T.unpack k) ++ "\""
      where
        plain :: Char -> Bool
        plain c = notElem @[] c ".[]\"\\" && isPrint c && not (isSpace c)

        -- The escapes of a double-quoted scalar, so that the path stays on
        -- the line of the error.
        escape :: Char -> String
        escape c
          | c == '"' || c == '\\' = ['\\', c]
          | c == '\n' = "\\n"
          | c == '\r' = "\\r"
          | c == '\t' = "\\t"
          | isPrint c = [c]
          | ord c <= 0xFF = hex 'x' 2
          | ord c <= 0xFFFF = hex 'u' 4
          | otherwise = hex 'U' 8
          where
            hex :: Char -> Int -> String
            hex p width =
              let h = showHex (ord c) ""
              in '\\' : p : replicate (width - length h) '0' ++ h

    index :: Int -> String
    index i = "[" ++ show i ++ "]"

-- | The path from the root to the node at the offset. If several nodes start
-- there, e.g. a block mapping and its first key, the outermost one counts. A
-- key does not add to the path, and a node inside a key that is a collection
-- has the path of the mapping.
nodePath :: Offset -> Node -> Path
nodePath off root = fromMaybe Root (listToMaybe (nodePaths [off] root))

-- | The paths of 'nodePath' for several offsets, in the order of the offsets,
-- from one walk of the tree.
nodePaths :: [Offset] -> Node -> [Path]
nodePaths offs root = map (\off -> M.findWithDefault Root off found) offs
  where
    found :: M.Map Offset Path
    found = walk (Set.delete noOffset (Set.fromList offs)) Root root M.empty

    walk :: Set.Set Offset -> Path -> Node -> M.Map Offset Path -> M.Map Offset Path
    walk wanted path n acc
      | Set.null inside = here
      | otherwise = case n.content of
          SequenceContent _ xs -> L.foldl' (\a (i, x) -> walk inside (Child path (Index i)) x a) here (zip [0 ..] xs)
          MappingContent _ kvs -> L.foldl' (\a (k, v) -> walk inside (Child path (keyElement k)) v (key inside path k a)) here kvs
          _ -> here
      where
        here :: M.Map Offset Path
        here
          | n.offset `Set.member` wanted = M.insertWith (\_ old -> old) n.offset path acc
          | otherwise = acc

        inside :: Set.Set Offset
        inside = within n wanted

    -- Every node of a key has the path of the mapping. An index or a key
    -- inside the key would read as a step into the mapping.
    key :: Set.Set Offset -> Path -> Node -> M.Map Offset Path -> M.Map Offset Path
    key wanted path k acc = Set.foldl' (\a off -> M.insertWith (\_ old -> old) off path a) acc offsets
      where
        -- A value can start at the end of its key, e.g. the empty value in
        -- "{a}", so the end is not a node of the key, unless the key is empty.
        offsets :: Set.Set Offset
        offsets =
          Set.takeWhileAntitone (\o -> o < k.endOffset || o == k.offset) $
            Set.dropWhileAntitone (< k.offset) wanted

    within :: Node -> Set.Set Offset -> Set.Set Offset
    within n = Set.takeWhileAntitone (<= n.endOffset) . Set.dropWhileAntitone (< n.offset)

    -- The texts of a parsed tree are slices of the input, which an error
    -- would keep alive.
    keyElement :: Node -> PathElement
    keyElement k = case k.content of
      ScalarContent _ t -> Key (T.copy t)
      AliasContent name -> AliasKey (T.copy name)
      _ -> CollectionKey

-- | Create an error at the given offset of the input.
errorAt :: T.Text -> Offset -> String -> Error
errorAt input off msg
  | off == noOffset = force $ Error (locate input off) msg T.empty 0 Root
  | otherwise =
      let (loc, index, _) = locateFrom input (startScan input) off
          sourceLine = T.copy (lineAt input off)
      in force $ Error loc msg sourceLine (min (lengthWord8 sourceLine) index) Root

-- | Create errors at the given offsets of a document, with their paths, in the
-- order of the list. The text is the input of the document, e.g. for the
-- offsets of t'Located' values.
documentErrors :: T.Text -> Document -> [(Offset, String)] -> [Error]
documentErrors input doc errs =
  zipWith (\err p -> force err {path = p}) (errorsAt input errs) (nodePaths (map fst errs) doc.root)

-- | Create errors at the given offsets of the input, in the order of the
-- list. One scan of the input locates all of them, and the errors on one
-- line share the copy of the line.
errorsAt :: T.Text -> [(Offset, String)] -> [Error]
errorsAt input errs =
  map snd . L.sortOn fst $ go (startScan input) Nothing (L.sortOn (fst . snd) (zip [0 :: Int ..] errs))
  where
    -- The line of the previous error, with the copy of its text.
    go :: Scan -> Maybe (Int, T.Text) -> [(Int, (Offset, String))] -> [(Int, Error)]
    go s prev = \case
      [] -> []
      (i, (off, msg)) : rest
        | off == noOffset -> (i, force $ Error (locate input off) msg T.empty 0 Root) : go s prev rest
        | otherwise ->
            let (loc, index, s') = locateFrom input s off
                sourceLine = case prev of
                  Just (ln, t) | ln == loc.line -> t
                  _ -> T.copy (lineAt input off)
            in (i, force $ Error loc msg sourceLine (min (lengthWord8 sourceLine) index) Root) : go s' (Just (loc.line, sourceLine)) rest

-- | Compute the line and the column of an offset. The byte order marks at the
-- start of a line are not columns, because they are not content. For
-- 'noOffset', the line and the column are 0.
locate :: T.Text -> Offset -> Location
locate input off
  | off == noOffset = Location {offset = off, line = 0, column = 0}
  | otherwise = let (loc, _, _) = locateFrom input (startScan input) off in loc

lengthWord8 :: T.Text -> Int
lengthWord8 (T.Text _ _ len) = len

-- | A scan of the input: the index, the line, the start of the columns of
-- the line, and an index on the line with its column. The columns of a line
-- start after its byte order marks.
data Scan = Scan !Int !Int !Int !Int !Int

startScan :: T.Text -> Scan
startScan (T.Text arr base len) = Scan base 1 start start 1
  where
    start :: Int
    start = skipBomsIn arr (base + len) base

-- | Locate an offset that is not before the index of the scan, and continue
-- the scan from there. Also give the index of the offset in the bytes of the
-- line from the start of its columns, or 0 for an offset in a byte order mark
-- at the start of the line.
locateFrom :: T.Text -> Scan -> Offset -> (Location, Int, Scan)
locateFrom (T.Text arr base len) s0 (Offset off0) = go s0
  where
    end, off :: Int
    end = base + len
    off = base + max 0 (min len off0)

    go :: Scan -> (Location, Int, Scan)
    go s@(Scan i ln ls ci col)
      | i >= off =
          if off <= ci
            -- An offset before the start of the columns is in a byte order
            -- mark.
            then (location ln (if off == ci then col else 1), max 0 (off - ls), s)
            else let col' = col + countChars ci off in (location ln col', off - ls, Scan i ln ls off col')
      | otherwise = case A.unsafeIndex arr i of
          LF -> newLine (i + 1)
          CR
            | i + 1 < end && A.unsafeIndex arr (i + 1) == LF -> go (Scan (i + 1) ln ls ci col)
            | otherwise -> newLine (i + 1)
          _ -> go (Scan (i + 1) ln ls ci col)
      where
        newLine :: Int -> (Location, Int, Scan)
        newLine j = let start' = skipBomsIn arr end j in go (Scan j (ln + 1) start' start' 1)

    location :: Int -> Int -> Location
    location ln col = Location {offset = Offset (off - base), line = ln, column = col}

    countChars :: Int -> Int -> Int
    countChars i0 i1 =
      length
        [() | i <- [i0 .. i1 - 1], isCharStart (A.unsafeIndex arr i)]

-- | The index after the byte order marks at the index.
skipBomsIn :: A.Array -> Int -> Int -> Int
skipBomsIn arr end i = if isBomIn arr end i then skipBomsIn arr end (i + bomLength) else i

-- | The line of the input that contains the offset, without the line break
-- and without the byte order marks at its start.
lineAt :: T.Text -> Offset -> T.Text
lineAt (T.Text arr base len) (Offset off0) = T.Text arr start (stop - start)
  where
    end, i0, off, start, stop :: Int
    end = base + len
    i0 = base + max 0 (min len off0)

    -- An offset between the characters of a CRLF line break is on the line
    -- before the break.
    off
      | i0 > base && i0 < end && A.unsafeIndex arr i0 == LF && A.unsafeIndex arr (i0 - 1) == CR = i0 - 1
      | otherwise = i0
    start = skipBomsIn arr end (findStart off)
    stop = max start (findStop off)

    findStart :: Int -> Int
    findStart i
      | i > base && not (isBreak (A.unsafeIndex arr (i - 1))) = findStart (i - 1)
      | otherwise = i

    findStop :: Int -> Int
    findStop i
      | i < end && not (isBreak (A.unsafeIndex arr i)) = findStop (i + 1)
      | otherwise = i

-- $setup
-- >>> import Yamlet
-- >>> printErrors = mapM_ (putStrLn . prettyError "input.yaml")
