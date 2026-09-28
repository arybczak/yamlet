-- | Errors with the position in the input that caused them.
module Yamlet.Error
  ( -- * Errors
    Error (..)
  , Location (..)
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

import Yamlet.Internal.Parser.Chars
import Yamlet.Internal.Syntax

-- | An error of the parser or the decoder.
data Error = Error
  { location :: !Location
  , message :: !String
  , sourceLine :: !T.Text
  -- ^ The line of the input that contains the location.
  , path :: [PathElement]
  -- ^ The keys and the indices from the root of the document to the node of
  -- a decoder error. An error at a key has the path of its mapping. The path
  -- is empty for an error of the parser and for a node that a program built.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

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
-- end with a line break. If the line is longer than 80 characters, the
-- excerpt shows only the 80 characters around the column.
--
-- >>> either (mapM_ (putStrLn . prettyError "config.yaml")) print (decodeText @(M.Map T.Text [[Int]]) "jobs:\n  - [1]\n  - 42\n")
-- config.yaml:3:5: jobs[1]: expected a list, but got an integer
--   |
-- 3 |   - 42
--   |     ^
--
-- An error with no position gives only the file and the message, e.g.
-- @config.yaml: duplicate key \"a\"@.
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
      | null err.path = err.message
      | otherwise = renderPath err.path ++ ": " ++ err.message

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

-- | A path in the form @jobs[1].name@. A key that is a collection is @?@,
-- and a key that is an alias is its alias, e.g. @*base@.
--
-- A key is in double quotes, e.g. @\"a.b\"@, if it:
--
-- * is empty,
-- * has white space, a character that cannot be printed, or one of the
--   characters @.[]\"\\@,
-- * starts with @?@ or @*@.
--
-- In the quotes, a character that cannot be printed has an escape as in
-- YAML, e.g. @\"a\\nb\"@.
--
-- >>> renderPath [Key "jobs", Index 1, Key "name"]
-- "jobs[1].name"
--
-- >>> renderPath [Key "a.b", Key ""]
-- "\"a.b\".\"\""
renderPath :: [PathElement] -> String
renderPath = \case
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
        plain c = c `notElem` (".[]\"\\" :: String) && isPrint c && not (isSpace c)

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
nodePath :: Offset -> Node -> [PathElement]
nodePath off root = fromMaybe [] (listToMaybe (nodePaths [off] root))

-- | The paths of 'nodePath' for several offsets, in the order of the offsets,
-- from one walk of the tree.
nodePaths :: [Offset] -> Node -> [[PathElement]]
nodePaths offs root = map (\off -> M.findWithDefault [] off found) offs
  where
    found :: M.Map Offset [PathElement]
    found = walk (Set.delete noOffset (Set.fromList offs)) [] root M.empty

    -- The path is in reverse.
    walk :: Set.Set Offset -> [PathElement] -> Node -> M.Map Offset [PathElement] -> M.Map Offset [PathElement]
    walk wanted rpath n acc
      | Set.null inside = here
      | otherwise = case n.content of
          Sequence _ xs -> L.foldl' (\a (i, x) -> walk inside (Index i : rpath) x a) here (zip [0 ..] xs)
          Mapping _ kvs -> L.foldl' (\a (k, v) -> walk inside (keyElement k : rpath) v (key inside rpath k a)) here kvs
          _ -> here
      where
        here :: M.Map Offset [PathElement]
        here
          | n.offset `Set.member` wanted = M.insertWith (\_ old -> old) n.offset (reverse rpath) acc
          | otherwise = acc

        inside :: Set.Set Offset
        inside = within n wanted

    -- Every node of a key has the path of the mapping. An index or a key
    -- inside the key would read as a step into the mapping.
    key :: Set.Set Offset -> [PathElement] -> Node -> M.Map Offset [PathElement] -> M.Map Offset [PathElement]
    key wanted rpath k acc = Set.foldl' (\a off -> M.insertWith (\_ old -> old) off path a) acc (within k wanted)
      where
        path :: [PathElement]
        path = reverse rpath

    within :: Node -> Set.Set Offset -> Set.Set Offset
    within n = Set.takeWhileAntitone (<= n.endOffset) . Set.dropWhileAntitone (< n.offset)

    keyElement :: Node -> PathElement
    keyElement k = case k.content of
      Scalar _ t -> Key t
      Alias name -> AliasKey name
      _ -> CollectionKey

-- | Create an error at the given offset of the input.
errorAt :: T.Text -> Offset -> String -> Error
errorAt input off msg =
  Error
    { location = loc
    , message = msg
    , sourceLine = if off == noOffset then T.empty else T.copy (lineAt input off)
    , path = []
    }
  where
    loc :: Location
    loc = locate input off

-- | Create errors at the given offsets of a document, with their paths, in the
-- order of the list. The text is the input of the document, e.g. for the
-- offsets of t'Located' values.
documentErrors :: T.Text -> Document -> [(Offset, String)] -> [Error]
documentErrors input doc errs =
  zipWith (\err p -> err {path = p}) (errorsAt input errs) (nodePaths (map fst errs) doc.root)

-- | Create errors at the given offsets of the input, in the order of the
-- list. One scan of the input locates all of them, and the errors on one
-- line share the copy of the line.
errorsAt :: T.Text -> [(Offset, String)] -> [Error]
errorsAt input@(T.Text arr base len) errs =
  map snd . L.sortOn fst $ go (startScan input) Nothing (L.sortOn (fst . snd) (zip [0 :: Int ..] errs))
  where
    -- The line of the previous error, with the copy of its text.
    go :: Scan -> Maybe (Int, T.Text) -> [(Int, (Offset, String))] -> [(Int, Error)]
    go s prev = \case
      [] -> []
      (i, (off, msg)) : rest
        | off == noOffset -> (i, Error (locate input off) msg T.empty []) : go s prev rest
        | otherwise ->
            let (loc, s') = locateFrom input s off
                sourceLine = case prev of
                  Just (ln, t) | ln == loc.line, not (betweenCrLf off) -> t
                  _ -> T.copy (lineAt input off)
            in (i, Error loc msg sourceLine []) : go s' (Just (loc.line, sourceLine)) rest

    -- 'lineAt' gives no text for an offset between the characters of a CRLF
    -- line break, but the line of the offset is the line before the break.
    betweenCrLf :: Offset -> Bool
    betweenCrLf (Offset o) =
      o > 0 && o < len && A.unsafeIndex arr (base + o) == LF && A.unsafeIndex arr (base + o - 1) == CR

-- | Compute the line and the column of an offset. A byte order mark at the
-- start of a line is not a column, because it is not content. For
-- 'noOffset', the line and the column are 0.
locate :: T.Text -> Offset -> Location
locate input off
  | off == noOffset = Location {offset = off, line = 0, column = 0}
  | otherwise = fst (locateFrom input (startScan input) off)

-- | A scan of the input: the index, the line, the index where the columns of
-- the line start, and an index on the line with its column.
data Scan = Scan !Int !Int !Int !Int !Int

startScan :: T.Text -> Scan
startScan (T.Text arr base len) = Scan base 1 start start 1
  where
    start :: Int
    start = skipBom arr (base + len) base

-- | Locate an offset that is not before the index of the scan, and continue
-- the scan from there.
locateFrom :: T.Text -> Scan -> Offset -> (Location, Scan)
locateFrom (T.Text arr base len) s0 (Offset off0) = go s0
  where
    end, off :: Int
    end = base + len
    off = base + max 0 (min len off0)

    go :: Scan -> (Location, Scan)
    go s@(Scan i ln start ci col)
      | i >= off =
          if off <= ci
            -- An offset before the start of the columns is in a byte order
            -- mark.
            then (location ln (if off == ci then col else 1), s)
            else let col' = col + countChars ci off in (location ln col', Scan i ln start off col')
      | otherwise = case A.unsafeIndex arr i of
          LF -> newLine (i + 1)
          CR
            | i + 1 < end && A.unsafeIndex arr (i + 1) == LF -> go (Scan (i + 1) ln start ci col)
            | otherwise -> newLine (i + 1)
          _ -> go (Scan (i + 1) ln start ci col)
      where
        newLine :: Int -> (Location, Scan)
        newLine j = let start' = skipBom arr end j in go (Scan j (ln + 1) start' start' 1)

    location :: Int -> Int -> Location
    location ln col = Location {offset = Offset (off - base), line = ln, column = col}

    countChars :: Int -> Int -> Int
    countChars i0 i1 =
      length
        [() | i <- [i0 .. i1 - 1], isCharStart (A.unsafeIndex arr i)]

-- | The index after a byte order mark at the index, or the index.
skipBom :: A.Array -> Int -> Int -> Int
skipBom arr end i = if isBomIn arr end i then i + bomLength else i

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

-- $setup
-- >>> import Yamlet
