{-# OPTIONS_HADDOCK not-home #-}

-- | Building blocks of the YAML output.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Emit
  ( -- * Scalars
    plainSyntax
  , plainLines
  , singleQuoted
  , singleQuotedLines
  , quotedPlain
  , quotedPlainLines
  , doubleQuoted
  , doubleQuotedLines
  , literalBlock
  , foldedBlock
  , needsIndentIndicator

    -- * Other
  , indentStep
  , tagText
  , tagHandle
  , tagDirective
  , isPrintable
  , spaces
  ) where

import Data.ByteString qualified as BS
import Data.Char
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as B
import Data.Text.Encoding qualified as T
import Data.Word
import Numeric

import Yamlet.Internal.Parser.Chars
import Yamlet.Internal.Utils

-- | The number of spaces that the content of a block collection or a block
-- scalar is indented by, relative to its parent. The output style is fixed.
indentStep :: Int
indentStep = 2

-- | The text reads back as the same text if it is a plain scalar on one line,
-- in a flow collection if the flag is set. The check ignores the schema, so
-- e.g. @12@ passes.
plainSyntax :: Bool -> T.Text -> Bool
plainSyntax inFlow t = case T.uncons t of
  Nothing -> False
  Just (c, rest) ->
    firstOk c rest
      && isPlainChar c
      && valid c rest
      && not (textIsPrefixOf "---" t)
      && not (textIsPrefixOf "..." t)
  where
    -- The characters after the given one are valid in a plain scalar, and
    -- the text has no ": " or " #" and does not end with white space or a
    -- colon.
    valid :: Char -> T.Text -> Bool
    valid prev s = case T.uncons s of
      Nothing -> not (asciiChar isWhite prev) && prev /= ':'
      Just (c, s')
        | not (isPlainChar c) -> False
        | prev == ':' && c == ' ' -> False
        | prev == ' ' && c == '#' -> False
        | otherwise -> valid c s'

    isPlainChar :: Char -> Bool
    isPlainChar c =
      (c == ' ' || (isScalarChar c && c /= '\t'))
        && not (inFlow && asciiChar isFlowIndicator c)

    firstOk :: Char -> T.Text -> Bool
    firstOk c rest
      | c `elem` ("-?:" :: String) = case T.uncons rest of
          Just (c', _) -> not (asciiChar isWhite c')
          Nothing -> False
      | otherwise = not (asciiChar isWhite c) && not (asciiChar isIndicator c)

-- | The text reads back as the same text on a line of a plain scalar after
-- the first line. Such a line can start with an indicator, but not with a
-- comment.
plainNextLine :: Bool -> T.Text -> Bool
plainNextLine inFlow t = case (T.uncons t, T.unsnoc t) of
  (Just (first, _), Just (_, lastChar)) ->
    first /= '#'
      && not (asciiChar isWhite first)
      && not (asciiChar isWhite lastChar)
      && lastChar /= ':'
      && T.all isPlainChar t
      && not (T.isInfixOf ": " t)
      && not (T.isInfixOf " #" t)
  _ -> False
  where
    isPlainChar :: Char -> Bool
    isPlainChar c =
      (c == ' ' || (isScalarChar c && c /= '\t'))
        && not (inFlow && asciiChar isFlowIndicator c)

-- | A plain scalar on the lines that start at the positions, with the lines
-- after the first one at the given indentation, if the text can be plain. It
-- is in a flow collection if the flag is set.
plainLines :: Bool -> Int -> [Int] -> T.Text -> Maybe B.Builder
plainLines inFlow indent starts t
  | plainSyntax inFlow first && all (plainNextLine inFlow . snd) rest =
      Just (onLines indent B.fromText ls)
  | otherwise = Nothing
  where
    ls@(first, rest) = flowLines False False (asciiChar isWhite) starts t

-- | A single-quoted scalar on one line, if the text has no line breaks.
singleQuoted :: T.Text -> Maybe B.Builder
singleQuoted t
  | T.all (\c -> c == '\t' || isScalarChar c) t =
      Just $ "'" <> B.fromText (T.replace "'" "''" t) <> "'"
  | otherwise = Nothing

-- | A single-quoted scalar on the lines that start at the positions, as in
-- 'plainLines', if single quotes can hold the text.
singleQuotedLines :: Int -> [Int] -> T.Text -> Maybe B.Builder
singleQuotedLines indent starts t
  | null starts = singleQuoted t
  | all (T.all (\c -> c == '\t' || isScalarChar c)) (first : map snd rest) =
      Just $ "'" <> onLines indent (B.fromText . T.replace "'" "''") ls <> "'"
  | otherwise = Nothing
  where
    ls@(first, rest) = flowLines True False (asciiChar isWhite) starts t

-- | The quoted form of a plain scalar whose text cannot be plain: in single
-- quotes, or in double quotes if the text has a tab or a character that
-- single quotes cannot hold. A tab in single quotes is not visible.
quotedPlain :: T.Text -> B.Builder
quotedPlain = quotedPlainLines 0 []

-- | 'quotedPlain' on the lines that start at the positions, as in
-- 'plainLines'.
quotedPlainLines :: Int -> [Int] -> T.Text -> B.Builder
quotedPlainLines indent starts t
  | T.any (== '\t') t = doubleQuotedLines indent starts t
  | otherwise = fromMaybe (doubleQuotedLines indent starts t) (singleQuotedLines indent starts t)

-- | A double-quoted scalar with escapes for the characters that need them.
doubleQuoted :: T.Text -> B.Builder
doubleQuoted t = "\"" <> doubleQuotedText t <> "\""

-- | A double-quoted scalar on the lines that start at the positions, as in
-- 'plainLines'. The escapes of the tabs keep them at the ends of the lines.
doubleQuotedLines :: Int -> [Int] -> T.Text -> B.Builder
doubleQuotedLines indent starts t
  | null starts = doubleQuoted t
  | otherwise = "\"" <> onLines indent doubleQuotedText (flowLines True True (== ' ') starts t) <> "\""

-- | The text of a double-quoted scalar, with escapes for the characters that
-- need them.
doubleQuotedText :: T.Text -> B.Builder
doubleQuotedText = T.foldr (\c b -> escape c <> b) mempty
  where
    escape :: Char -> B.Builder
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\r' -> "\\r"
      '\0' -> "\\0"
      c
        | isScalarChar c -> B.fromChar c
        | ord c < 16 ^ xEscapeDigits -> "\\x" <> hex xEscapeDigits (ord c)
        | ord c < 16 ^ uEscapeDigits -> "\\u" <> hex uEscapeDigits (ord c)
        | otherwise -> "\\U" <> hex bigUEscapeDigits (ord c)

    hex :: Int -> Int -> B.Builder
    hex k i = let s = map toUpper (showHex i "") in B.fromText (T.pack (replicate (k - length s) '0' ++ s))
-- Inlining lets the builder write each character to the buffer. Without it,
-- the builder allocates a closure for each character, and the render
-- benchmark of the JSON input allocated 64 MB instead of 43 MB.
{-# INLINE doubleQuotedText #-}

-- | The lines of a flow scalar that start at the positions: the first line,
-- and each next line with the number of empty lines above it, or 'Nothing'
-- after an escaped line break. A line break replaces a space of the text,
-- and an empty line replaces a line break of the text. A position where the
-- style cannot start a line and keep the text joins its two lines.
--
-- The first flag allows an empty first and last line, e.g. for a quoted
-- scalar. The second flag allows escaped line breaks. The parser drops a
-- white character at the start or the end of a line.
flowLines :: Bool -> Bool -> (Char -> Bool) -> [Int] -> T.Text -> (T.Text, [(Maybe Int, T.Text)])
flowLines quoted escapes white starts t = case splitLines starts t of
  first : rest -> go True first rest
  [] -> (t, [])
  where
    go :: Bool -> T.Text -> [T.Text] -> (T.Text, [(Maybe Int, T.Text)])
    go isFirst a = \case
      [] -> (a, [])
      b : rest -> case lineEnd isFirst (null rest) a b of
        Just (a', end) -> let (l, ls) = go False b rest in (a', (end, l) : ls)
        Nothing -> go isFirst (a <> b) rest

    -- The first line without the text that the line break replaces, and the
    -- number of empty lines.
    lineEnd :: Bool -> Bool -> T.Text -> T.Text -> Maybe (T.Text, Maybe Int)
    lineEnd isFirst isLast a b
      | not startOk = Nothing
      | Just (a', ' ') <- T.unsnoc a, endOk a' = Just (a', Just 0)
      | k > 0, endOk a'' = Just (a'', Just k)
      | escapes = Just (a, Nothing)
      | otherwise = Nothing
      where
        startOk :: Bool
        startOk = case T.uncons b of
          Just (c, _) -> not (white c)
          Nothing -> quoted && isLast

        endOk :: T.Text -> Bool
        endOk x = case T.unsnoc x of
          Just (_, c) -> not (white c)
          Nothing -> quoted && isFirst

        k :: Int
        k = T.length (T.takeWhileEnd (== '\n') a)

        a'' :: T.Text
        a'' = T.dropEnd k a

-- | The flow scalar on its lines, with each line after the first one at the
-- given indentation.
onLines :: Int -> (T.Text -> B.Builder) -> (T.Text, [(Maybe Int, T.Text)]) -> B.Builder
onLines indent text (first, rest) = text first <> mconcat [lineBreak end <> spaces indent <> text l | (end, l) <- rest]
  where
    lineBreak :: Maybe Int -> B.Builder
    lineBreak = \case
      Just k -> B.fromText (T.replicate (k + 1) "\n")
      Nothing -> "\\\n"

-- | The text split at the positions. A position that does not come after the
-- one before it is ignored, and a position after the end of the text ends
-- the split.
splitLines :: [Int] -> T.Text -> [T.Text]
splitLines = go 0
  where
    go :: Int -> [Int] -> T.Text -> [T.Text]
    go at starts s = case starts of
      p : rest
        | p <= at -> go at rest s
        | T.compareLength s (p - at) == LT -> [s]
        | otherwise -> let (a, b) = T.splitAt (p - at) s in a : go p rest b
      [] -> [s]

-- | The header and the content lines of a literal block scalar, with the
-- content at the given indentation. The flag allows the keep indicator for
-- trailing empty lines.
literalBlock :: Bool -> Int -> T.Text -> Maybe (B.Builder, B.Builder)
literalBlock allowKeep indent t = do
  (header, body, trailing) <- blockParts allowKeep t
  let content =
        mconcat (map (line indent) (T.splitOn "\n" body))
          <> B.fromText (T.replicate (trailing - 1) "\n")
  Just ("|" <> header, content)

-- | The header and the content lines of a folded block scalar, with the
-- content at the given indentation. It has no keep indicator. Each line of
-- the text becomes one line of the output, or several lines if the lines
-- start at the positions.
foldedBlock :: Int -> [Int] -> T.Text -> Maybe (B.Builder, B.Builder)
foldedBlock indent starts t = do
  (header, body, _) <- blockParts False t
  let (leading, rest) = span T.null (T.splitOn "\n" body)
      content = mconcat (replicate (length leading) "\n") <> go Nothing (length leading) starts (groups rest)
  Just (">" <> header, content)
  where
    -- The lines with content, each with the number of empty lines before it.
    groups :: [T.Text] -> [(Int, T.Text)]
    groups ls = case span T.null ls of
      (_, []) -> []
      (empties, l : ls') -> (length empties, l) : groups ls'

    -- A line break between two lines that start with content folds into a
    -- space, so the output needs one empty line more there. The group starts
    -- at the offset.
    go :: Maybe T.Text -> Int -> [Int] -> [(Int, T.Text)] -> B.Builder
    go prev offset ss = \case
      [] -> mempty
      (empties, l) : ls ->
        let extra = case prev of
              Just p | not (isSpaced p) && not (isSpaced l) -> 1
              _ -> 0
            separator = case prev of
              Just _ -> mconcat (replicate (empties + extra) "\n")
              Nothing -> mempty
            lineStart = offset + empties
            lineEnd = lineStart + T.length l
            (inLine, ss') = span (< lineEnd) (dropWhile (<= lineStart) ss)
        in separator
             <> mconcat (map (line indent) (lineParts (map (subtract lineStart) inLine) l))
             <> go (Just l) (lineEnd + 1) ss' ls

    -- The parts of a line of the text that start at the positions. A line
    -- break replaces a space between two parts that start with content.
    lineParts :: [Int] -> T.Text -> [T.Text]
    lineParts ps l
      | null ps || isSpaced l = [l]
      | otherwise = join (splitLines ps l)
      where
        join :: [T.Text] -> [T.Text]
        join = \case
          a : b : rest
            | Just (a', ' ') <- T.unsnoc a
            , Just (c, _) <- T.uncons b
            , c /= ' ' && c /= '\t' ->
                a' : join (b : rest)
            | otherwise -> join (a <> b : rest)
          parts -> parts

    isSpaced :: T.Text -> Bool
    isSpaced l = case T.uncons l of
      Just (c, _) -> c == ' ' || c == '\t'
      Nothing -> False

-- | The header of a block scalar, its content without the trailing line breaks
-- and the number of these line breaks.
blockParts :: Bool -> T.Text -> Maybe (B.Builder, T.Text, Int)
blockParts allowKeep t
  | T.null body = Nothing
  | not (T.all (\c -> c == '\n' || c == '\t' || isScalarChar c) t) = Nothing
  | trailing > 1 && not allowKeep = Nothing
  | otherwise = Just (indicator <> chomping, body, trailing)
  where
    body :: T.Text
    body = T.dropWhileEnd (== '\n') t

    trailing :: Int
    trailing = T.length t - T.length body

    indicator :: B.Builder
    indicator = if needsIndentIndicator t then B.fromDec indentStep else mempty

    chomping :: B.Builder
    chomping = case trailing of
      0 -> "-"
      1 -> mempty
      _ -> "+"

-- | A block scalar with the text needs an indentation indicator, because its
-- first line with content starts with a space or a tab. YAML 1.2 does not
-- need the indicator for a tab, but libyaml rejects the block scalar without
-- it. Parsers do not agree on the meaning of the indicator at the top level,
-- so a caller there writes such a text with quotes.
needsIndentIndicator :: T.Text -> Bool
needsIndentIndicator t = case T.uncons (T.dropWhile (== '\n') t) of
  Just (c, _) -> c == ' ' || c == '\t'
  Nothing -> False

-- | A line of a block scalar. An empty line gets no indentation.
line :: Int -> T.Text -> B.Builder
line indent l
  | T.null l = "\n"
  | otherwise = "\n" <> spaces indent <> B.fromText l

-- | A tag in the shortest form that reads back as the same tag. A tag that no
-- text can hold, e.g. an empty tag, becomes the non-specific tag @!@.
--
-- A global tag that is not a valid URI needs the directive of
-- 'tagDirective' in its document.
tagText :: T.Text -> B.Builder
tagText tag
  | T.null tag = "!"
  | Just suffix <- textStripPrefix coreTagPrefix tag
  , not (T.null suffix) =
      "!!" <> shorthand suffix
  | Just suffix <- textStripPrefix "!" tag
  , not (T.null suffix) =
      "!" <> shorthand suffix
  | Just (c, suffix) <- T.uncons tag
  , not (isVerbatim tag) =
      if T.null suffix then "!" else handleText c <> shorthand suffix
  | otherwise = "!<" <> B.fromText tag <> ">"

-- | The character whose handle a tag needs, if the tag needs a directive.
tagHandle :: T.Text -> Maybe Char
tagHandle tag = case T.uncons tag of
  Just (c, suffix)
    | c /= '!'
    , not (T.null suffix)
    , not (textIsPrefixOf coreTagPrefix tag)
    , not (isVerbatim tag) ->
        Just c
  _ -> Nothing

-- | The @%TAG@ directive of the handle for the tags that start with the
-- character, with the line break. The prefix is always an escape, because
-- e.g. a @#@ after a space starts a comment.
tagDirective :: Char -> B.Builder
tagDirective c = "%TAG " <> handleText c <> " " <> percentEscape c <> "\n"

handleText :: Char -> B.Builder
handleText c = "!t" <> B.fromText (T.pack (showHex (ord c) "")) <> "!"

-- | A global tag that a verbatim tag holds as it is. The parser does not
-- decode the escapes of a verbatim tag.
isVerbatim :: T.Text -> Bool
isVerbatim tag = hasScheme && uriChars (T.unpack tag)
  where
    hasScheme :: Bool
    hasScheme = case T.break (== ':') tag of
      (scheme, rest) -> case T.uncons scheme of
        Just (c, cs) ->
          isAscii c && isAlpha c && T.all (\x -> isAscii x && (isAlphaNum x || x `elem` ("+-." :: String))) cs && not (T.null rest)
        Nothing -> False

    uriChars :: String -> Bool
    uriChars = \case
      '%' : a : b : rest -> isHexDigit a && isHexDigit b && uriChars rest
      c : rest -> asciiChar isUriChar c && uriChars rest
      [] -> True

-- | The text of a tag suffix or a tag prefix. A character that the form does
-- not allow gets a %XX escape, which the parser decodes.
shorthand :: T.Text -> B.Builder
shorthand = T.foldr (\c b -> (if asciiChar isTagChar c then B.fromChar c else percentEscape c) <> b) mempty

-- | The %XX escapes of the UTF-8 bytes of a character.
percentEscape :: Char -> B.Builder
percentEscape c = mconcat [B.fromText (T.pack ('%' : hex w)) | w <- BS.unpack (T.encodeUtf8 (T.singleton c))]
  where
    hex :: Word8 -> String
    hex w = let s = map toUpper (showHex w "") in replicate (percentDigits - length s) '0' ++ s

-- | c-printable without the line breaks and the byte order mark.
isPrintable :: Char -> Bool
isPrintable c
  | c < ' ' = False
  | c <= '~' = True
  | c < '\xA0' = False
  | c == '\xFEFF' = False
  | c >= '\xD800' && c <= '\xDFFF' = False
  | c == '\xFFFE' || c == '\xFFFF' = False
  | otherwise = True

-- | A printable character that needs no escape in a scalar. YAML 1.1 reads
-- U+2028 and U+2029 as line breaks, so they get escapes too.
isScalarChar :: Char -> Bool
-- The guards for ASCII come first. Without them, the encode benchmark of
-- the long texts takes about 15% longer.
isScalarChar c
  | c < ' ' = False
  | c <= '~' = True
  | otherwise = isPrintable c && c /= '\x2028' && c /= '\x2029'

spaces :: Int -> B.Builder
spaces k = B.fromText (T.replicate k " ")
