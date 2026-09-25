{-# OPTIONS_HADDOCK not-home #-}

-- | Building blocks of the YAML output.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Emit
  ( -- * Scalars
    plainSyntax
  , singleQuoted
  , doubleQuoted
  , literalBlock
  , foldedBlock
  , needsIndentIndicator

    -- * Other
  , tagText
  , tagHandle
  , tagDirective
  , isPrintable
  , spaces
  ) where

import Data.ByteString qualified as BS
import Data.Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Builder.Linear qualified as B
import Data.Word
import Numeric

-- | The text reads back as the same text if it is a plain scalar on one line,
-- in a flow collection if the flag is set. The check ignores the schema, so
-- e.g. @12@ passes.
plainSyntax :: Bool -> T.Text -> Bool
plainSyntax inFlow t = case T.uncons t of
  Nothing -> False
  Just (c, rest) ->
    firstOk c rest
      && T.all isPlainChar t
      && not (isWhite (T.last t))
      && T.last t /= ':'
      && not (": " `T.isInfixOf` t)
      && not (" #" `T.isInfixOf` t)
      && not ("---" `T.isPrefixOf` t)
      && not ("..." `T.isPrefixOf` t)
  where
    firstOk :: Char -> T.Text -> Bool
    firstOk c rest
      | c `elem` ("-?:" :: String) = case T.uncons rest of
          Just (c', _) -> not (isWhite c')
          Nothing -> False
      | otherwise = not (isWhite c) && c `notElem` ("-?:,[]{}#&*!|>'\"%@`" :: String)

    isPlainChar :: Char -> Bool
    isPlainChar c =
      (c == ' ' || (isPrintable c && c /= '\t'))
        && not (inFlow && c `elem` (",[]{}" :: String))

    isWhite :: Char -> Bool
    isWhite c = c == ' ' || c == '\t'

-- | A single-quoted scalar on one line, if the text has no line breaks.
singleQuoted :: T.Text -> Maybe B.Builder
singleQuoted t
  | T.all (\c -> c == '\t' || isPrintable c) t =
      Just $ "'" <> B.fromText (T.replace "'" "''" t) <> "'"
  | otherwise = Nothing

-- | A double-quoted scalar with escapes for the characters that need them.
doubleQuoted :: T.Text -> B.Builder
doubleQuoted t = "\"" <> T.foldr (\c b -> escape c <> b) mempty t <> "\""
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
        | isPrintable c -> B.fromChar c
        | ord c <= 0xFF -> "\\x" <> hex 2 (ord c)
        | ord c <= 0xFFFF -> "\\u" <> hex 4 (ord c)
        | otherwise -> "\\U" <> hex 8 (ord c)

    hex :: Int -> Int -> B.Builder
    hex k i = let s = map toUpper (showHex i "") in B.fromText (T.pack (replicate (k - length s) '0' ++ s))

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
-- content at the given indentation. It has no keep indicator, and each line of
-- the text becomes one line of the output.
foldedBlock :: Int -> T.Text -> Maybe (B.Builder, B.Builder)
foldedBlock indent t = do
  (header, body, _) <- blockParts False t
  let (leading, rest) = span T.null (T.splitOn "\n" body)
      content = mconcat (replicate (length leading) "\n") <> go Nothing (groups rest)
  Just (">" <> header, content)
  where
    -- The lines with content, each with the number of empty lines before it.
    groups :: [T.Text] -> [(Int, T.Text)]
    groups ls = case span T.null ls of
      (_, []) -> []
      (empties, l : ls') -> (length empties, l) : groups ls'

    -- A line break between two lines that start with content folds into a
    -- space, so the output needs one empty line more there.
    go :: Maybe T.Text -> [(Int, T.Text)] -> B.Builder
    go prev = \case
      [] -> mempty
      (empties, l) : ls ->
        let extra = case prev of
              Just p | not (isSpaced p) && not (isSpaced l) -> 1
              _ -> 0
            separator = case prev of
              Just _ -> mconcat (replicate (empties + extra) "\n")
              Nothing -> mempty
        in separator <> line indent l <> go (Just l) ls

    isSpaced :: T.Text -> Bool
    isSpaced l = case T.uncons l of
      Just (c, _) -> c == ' ' || c == '\t'
      Nothing -> False

-- | The header of a block scalar, its content without the trailing line breaks
-- and the number of these line breaks.
blockParts :: Bool -> T.Text -> Maybe (B.Builder, T.Text, Int)
blockParts allowKeep t
  | T.null body = Nothing
  | not (T.all (\c -> c == '\n' || c == '\t' || isPrintable c) t) = Nothing
  | trailing > 1 && not allowKeep = Nothing
  | otherwise = Just (indicator <> chomping, body, trailing)
  where
    body :: T.Text
    body = T.dropWhileEnd (== '\n') t

    trailing :: Int
    trailing = T.length t - T.length body

    indicator :: B.Builder
    indicator = if needsIndentIndicator t then "2" else mempty

    chomping :: B.Builder
    chomping = case trailing of
      0 -> "-"
      1 -> mempty
      _ -> "+"

-- | A block scalar with the text needs an indentation indicator, because its
-- first line with content starts with a space. Parsers do not agree on the
-- meaning of the indicator at the top level, so a caller there writes such a
-- text with quotes.
needsIndentIndicator :: T.Text -> Bool
needsIndentIndicator t = case T.uncons (T.dropWhile (== '\n') t) of
  Just (' ', _) -> True
  _ -> False

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
  | Just suffix <- T.stripPrefix "tag:yaml.org,2002:" tag
  , not (T.null suffix) =
      "!!" <> shorthand suffix
  | Just suffix <- T.stripPrefix "!" tag
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
    , not ("tag:yaml.org,2002:" `T.isPrefixOf` tag)
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
      c : rest -> isUriChar c && uriChars rest
      [] -> True

    isUriChar :: Char -> Bool
    isUriChar c = isTagChar c || c `elem` (",[]!" :: String)

-- | The text of a tag suffix or a tag prefix. A character that the form does
-- not allow gets a %XX escape, which the parser decodes.
shorthand :: T.Text -> B.Builder
shorthand = T.foldr (\c b -> (if isTagChar c then B.fromChar c else percentEscape c) <> b) mempty

-- | The %XX escapes of the UTF-8 bytes of a character.
percentEscape :: Char -> B.Builder
percentEscape c = mconcat [B.fromText (T.pack ('%' : hex w)) | w <- BS.unpack (T.encodeUtf8 (T.singleton c))]
  where
    hex :: Word8 -> String
    hex w = let s = map toUpper (showHex w "") in if length s < 2 then '0' : s else s

isTagChar :: Char -> Bool
isTagChar c = isAscii c && (isAlphaNum c || c `elem` ("-#;/?:@&=+$_.~*'()" :: String))

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

spaces :: Int -> B.Builder
spaces k = B.fromText (T.replicate k " ")
