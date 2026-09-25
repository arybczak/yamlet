{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | The parser of YAML 1.2.2 streams.
--
-- The functions follow the productions of the specification and keep their
-- names, e.g. 'nsFlowNode' implements @ns-flow-node(n,c)@. A few productions
-- are fused into loops over the bytes of the input for speed.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Parser
  ( parseStream
  ) where

import Control.Monad
import Data.Bits
import Data.ByteString qualified as BS
import Data.Char
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Encoding qualified as T
import Data.Text.Internal qualified as T
import Data.Word
import Numeric

import Yamlet.Error
import Yamlet.Internal.Comments
import Yamlet.Internal.Parser.Monad
import Yamlet.Internal.Syntax

-- | Parse all documents of a stream.
parseStream :: T.Text -> Either Error [Document]
parseStream input@(T.Text arr off len) = case prescan e start of
  Left i -> Left $ errorAt input (toOffset e i) ("invalid character " ++ codePoint (T.head (slice e i e.end)))
  Right (markers, boms) -> case runParser e start (lYamlStream markers) of
    Left (ParseError i msg) -> Left $ errorAt input (toOffset e i) msg
    Right (Just docs, _, _) -> case filter (not . allowedBom docs) boms of
      i : _ -> Left $ errorAt input (toOffset e i) "unexpected byte order mark"
      [] -> Right docs
    Right (Nothing, _, fu) -> let (i, msg) = unexpected e fu in Left $ errorAt input (toOffset e i) msg
  where
    -- A byte order mark can start a line between documents, or be a
    -- character of a quoted scalar.
    allowedBom :: [Document] -> Int -> Bool
    allowedBom docs i = case M.lookupLE (toOffset e i) (scalarRanges docs) of
      Just (_, (end, quoted)) | toOffset e i < end -> quoted
      _ -> isStartOfLine e i

    scalarRanges :: [Document] -> M.Map Offset (Offset, Bool)
    scalarRanges docs = M.fromList (foldr (\d -> ranges d.root) [] docs)
      where
        ranges :: Node -> [(Offset, (Offset, Bool))] -> [(Offset, (Offset, Bool))]
        ranges n acc = case n.content of
          Scalar style _ -> (n.offset, (n.endOffset, style == SingleQuoted || style == DoubleQuoted)) : acc
          Sequence _ xs -> foldr ranges acc xs
          Mapping _ kvs -> foldr (\(k, v) -> ranges k . ranges v) acc kvs
          Alias _ -> acc

    e :: Env
    e =
      Env
        { array = arr
        , base = off
        , end = off + len
        , handles = defaultHandles
        }

    start :: Int
    start = if isBom e off then off + 3 else off

    -- The characters that YAML forbids are not printable, so the error names
    -- the code point, e.g. U+0007.
    codePoint :: Char -> String
    codePoint c =
      let hex = map toUpper (showHex (ord c) "")
      in "U+" ++ replicate (4 - length hex) '0' ++ hex

-- | Check that the input has only characters that YAML allows, and find the
-- lines that start with a document marker, and the byte order marks. A
-- document cannot contain such a line. The index of a marker after a byte
-- order mark is the index of the mark. Return the index of an invalid
-- character on error.
prescan :: Env -> Int -> Either Int ([Int], [Int])
prescan e start = go start (if isMarker e start then [start] else []) []
  where
    go :: Int -> [Int] -> [Int] -> Either Int ([Int], [Int])
    go i acc boms
      | i >= e.end = Right (reverse acc, reverse boms)
      | otherwise =
          let w = A.unsafeIndex e.array i
          in if
               | w >= 0x20 && w < 0x7F -> go (i + 1) acc boms
               | w == LF || (w == CR && byteAt e (i + 1) /= LF) ->
                   let s = i + 1
                       marker = isMarker e s || (isBom e s && isMarker e (s + 3))
                   in go s (if marker then s : acc else acc) boms
               | w == CR || w == TAB -> go (i + 1) acc boms
               | w < 0x20 || w == 0x7F -> Left i
               -- C1 control characters except NEL.
               | w == 0xC2 && i + 1 < e.end
               , let w1 = A.unsafeIndex e.array (i + 1)
               , w1 >= 0x80 && w1 <= 0x9F && w1 /= 0x85 ->
                   Left i
               -- U+FFFE and U+FFFF.
               | w == 0xEF && i + 2 < e.end
               , A.unsafeIndex e.array (i + 1) == 0xBF
               , let w2 = A.unsafeIndex e.array (i + 2)
               , w2 == 0xBE || w2 == 0xBF ->
                   Left i
               | w == 0xEF && isBom e i -> go (i + 3) acc (i : boms)
               | otherwise -> go (i + 1) acc boms

-- | The location and the message of the error for the furthest position at
-- which the parser failed.
unexpected :: Env -> Int -> (Int, String)
unexpected e i = case indentationTab (i - 1) Nothing of
  Just tab -> (tab, "tabs cannot be used for indentation")
  Nothing | Just r <- blockMistake e i -> r
  Nothing
    | Just colon <- aliasColon ->
        (colon, "the name of the alias includes the ':', write a space before ':' if the alias is a key")
  Nothing -> (i,) $ case byteAt e i of
    w
      | byteBefore e i == STAR && not (isAnchorChar w) -> "expected an alias name after '*'"
      | byteBefore e i == AMP && not (isAnchorChar w) -> "expected an anchor name after '&'"
      | w == 0 -> "unexpected end of input"
      | indented -> maybe "unexpected indentation" id (indentationMistake e i)
      | isBreak w -> "unexpected end of line"
      | i > e.base && isBreak (byteBefore e i), Just msg <- indentationMistake e i -> msg
      | w == COLON && firstColon && not (fitsKey e entryStart i) ->
          "a key can be at most 1024 characters long, write a longer key after '? '"
      -- A colon on the first line of a key does not fail, so the scalar
      -- before this one started on a line above.
      | w == COLON && firstColon && valueColon ->
          "unexpected ':', this line continues the scalar from the line above, check the indentation and the line above"
      | w == COLON && valueColon ->
          "unexpected ':', quote the value if it contains \": \""
      | itemAfterKey -> "unexpected '-', a list cannot start on the line of its key"
      | Just msg <- mistake e i -> msg
      | otherwise -> unexpectedChar e i
  where
    -- The ':' that ends an alias name before the index, as in "*x: 1". An
    -- alias name can contain ':'.
    aliasColon :: Maybe Int
    aliasColon =
      let j = skipBackWhites e i
          start = wordStart e j
      in if j > start && byteBefore e j == COLON && byteAt e start == STAR
           then Just (j - 1)
           else Nothing

    -- The start of the entry on the line, after any "- ".
    entryStart :: Int
    entryStart = skipListItems e (skipSpaces e (lineStart e i))

    -- No colon that ends a key precedes the index on its line.
    firstColon :: Bool
    firstColon = not (any (isKeyColon e) [entryStart .. i - 1])

    -- A list item right after a key, as in "a: - b".
    itemAfterKey :: Bool
    itemAfterKey = isListItem e i && byteBefore e (skipBack i) == COLON
      where
        skipBack :: Int -> Int
        skipBack j = if isWhite (byteBefore e j) then skipBack (j - 1) else j

    -- A colon that ends a word and precedes white space, as in an unquoted
    -- value like "Error: file not found".
    valueColon :: Bool
    valueColon =
      isNsChar (byteBefore e i)
        && (let w = byteAt e (i + 1) in w == 0 || isWhite w || isBreak w)

    -- Only spaces precede the index on its line.
    indented :: Bool
    indented = i > e.base && byteBefore e i == SPACE && go (i - 1)
      where
        go :: Int -> Bool
        go j
          | j <= e.base = True
          | otherwise = case byteBefore e j of
              SPACE -> go (j - 1)
              w -> isBreak w

    -- The first tab in the indentation before the index, if only white space
    -- precedes the index on its line.
    indentationTab :: Int -> Maybe Int -> Maybe Int
    indentationTab j tab
      | j < e.base = tab'
      | otherwise = case A.unsafeIndex e.array j of
          SPACE -> indentationTab (j - 1) tab
          TAB -> indentationTab (j - 1) (Just j)
          w
            | isBreak w -> tab'
            | otherwise -> Nothing
      where
        tab' :: Maybe Int
        tab' = if byteAt e i == TAB then Just (maybe i id tab) else tab

-- | The error for a common mistake at the index, if the character there shows
-- one.
mistake :: Env -> Int -> Maybe String
mistake e i
  -- Inside a plain scalar, a '#' after other content does not stop the
  -- parser, so here it follows the end of another node, e.g. "x"#c.
  | w == HASH && isNsChar (byteBefore e i) =
      Just "unexpected '#', a comment needs a space before it"
  | w == COMMA && (let b = byteBefore e (skipBack i) in b == COMMA || b == LBRACKET || b == LBRACE) =
      Just "unexpected ',', a flow collection cannot have an empty entry"
  | w == STAR && not (isAnchorChar (byteAt e (i + 1))) = Just "expected an alias name after '*'"
  | w == STAR && (let b = byteAt e (wordStart e (skipBackWhites e i)) in b == AMP || b == EXCL) =
      Just "unexpected '*', an alias cannot have an anchor or a tag"
  | w == AMP && not (isAnchorChar (byteAt e (i + 1))) = Just "expected an anchor name after '&'"
  | afterQuote SQUOTE =
      Just $ unexpectedChar e i ++ " after a single-quoted scalar, write '' for a quote inside it"
  | afterQuote DQUOTE =
      Just $ unexpectedChar e i ++ " after a double-quoted scalar, write \\\" for a quote inside it"
  -- Other indicators start a node of another kind, e.g. '&' an anchor.
  | w == AT || w == GRAVE || w == PERCENT =
      Just $ unexpectedChar e i ++ ", a plain scalar cannot start with it, quote the value"
  | otherwise = Nothing
  where
    w :: Word8
    w = byteAt e i

    -- The index after the last content before the white space and the line
    -- breaks that end at the index.
    skipBack :: Int -> Int
    skipBack j
      | isWhite (byteBefore e j) || isBreak (byteBefore e j) = skipBack (j - 1)
      | otherwise = j

    -- Content right after a quote, as in 'it's'. A plain scalar can hold a
    -- quote, so the quote closes a quoted scalar. A colon there ends a key.
    afterQuote :: Word8 -> Bool
    afterQuote q =
      byteBefore e i == q && isNsChar w && not (isFlowIndicator w) && w /= COLON

-- | The error for content at the index that starts a line with a wrong
-- indentation, if the lines above show the likely mistake: a list item among
-- mapping entries or the other way round, or a line of a block scalar with
-- too little indentation.
indentationMistake :: Env -> Int -> Maybe String
indentationMistake e i = go (lineStart e i)
  where
    column :: Int
    column = i - lineStart e i

    -- Look at the lines above, up to the first line with less indentation.
    go :: Int -> Maybe String
    go start = do
      k <- lineAbove e start
      let indent = k - lineStart e k
      if
        | indent > column -> go (lineStart e k)
        | indent < column ->
            if endsWithHeader k
              then Just "unexpected indentation, the line has less indentation than the block scalar above it"
              else Nothing
        | isListItem e k && not (isListItem e i) && not (isFlowIndicator (byteAt e i)) ->
            Just "unexpected key among list items"
        | not (isListItem e k) && isListItem e i -> Just "unexpected list item among mapping entries"
        | otherwise -> Nothing

    -- The line from the index ends with a block scalar header, e.g. "key: |-".
    endsWithHeader :: Int -> Bool
    endsWithHeader j =
      let end = trimEnd j (contentEnd j)
          h = skipIndicators end
      in h > j
           && (let b = byteAt e (h - 1) in b == PIPE || b == GREATER)
           && (h - 1 == j || isWhite (byteAt e (h - 2)))

    -- The end of the line before a comment.
    contentEnd :: Int -> Int
    contentEnd j
      | b == 0 || isBreak b = j
      | b == HASH && isWhite (byteBefore e j) = j
      | otherwise = contentEnd (j + 1)
      where
        b :: Word8
        b = byteAt e j

    trimEnd :: Int -> Int -> Int
    trimEnd start j = if j > start && isWhite (byteBefore e j) then trimEnd start (j - 1) else j

    skipIndicators :: Int -> Int
    skipIndicators j =
      let b = byteBefore e j
      in if b == MINUS || b == 0x2B || isDecDigit b then skipIndicators (j - 1) else j

-- | The error for a line of a block collection that lacks the space after
-- "-" or the ":" after a key, if the entries above it at the same position
-- are list items or mapping entries.
blockMistake :: Env -> Int -> Maybe (Int, String)
blockMistake e i = do
  guard $ start < i
  k <- entryAbove (lineStart e i)
  if
    | isListItem e k && byteAt e start == MINUS && i == start + 1 ->
        Just (i, "expected a space after '-'")
    | not (isListItem e k) && (w == 0 || isBreak w) && not (any (isKeyColon e) [start .. i - 1]) ->
        Just $ case filter tightColon [start .. i - 1] of
          colon : _ -> (colon + 1, "expected a space after ':'")
          [] -> (i, "expected ':' after the key")
    | otherwise -> Nothing
  where
    w :: Word8
    w = byteAt e i

    start :: Int
    start = skipSpaces e (lineStart e i)

    column :: Int
    column = start - lineStart e i

    -- The closest entry above that starts at the column. An entry can follow
    -- "- " on its line, as in "- key: value".
    entryAbove :: Int -> Maybe Int
    entryAbove from = do
      k <- lineAbove e from
      let indent = k - lineStart e k
          entry = skipListItems e k
      if
        | indent == column -> Just k
        | entry - lineStart e k == column -> Just entry
        | indent < column -> Nothing
        | otherwise -> entryAbove (lineStart e k)

    -- A colon before a word, as in "key:value", but not in "http://".
    tightColon :: Int -> Bool
    tightColon j = byteAt e j == COLON && startsWord (byteAt e (j + 1))

    startsWord :: Word8 -> Bool
    startsWord b =
      (b < 0x80 && isAlphaNum (chr (fromIntegral b)))
        || b == SQUOTE
        || b == DQUOTE
        || b == LBRACKET
        || b == LBRACE

-- | The index after the last content before the white space that ends at the
-- index.
skipBackWhites :: Env -> Int -> Int
skipBackWhites e i = if isWhite (byteBefore e i) then skipBackWhites e (i - 1) else i

-- | The start of the word that ends at the index, e.g. of an anchor or an
-- alias with its indicator.
wordStart :: Env -> Int -> Int
wordStart e i = if isAnchorChar (byteBefore e i) then wordStart e (i - 1) else i

-- | The start of the line that contains the index.
lineStart :: Env -> Int -> Int
lineStart e i
  | i > e.base && not (isBreak (byteBefore e i)) = lineStart e (i - 1)
  | otherwise = i

-- | The first content of the closest line above the line that starts at the
-- index. Blank lines and comment lines do not count.
lineAbove :: Env -> Int -> Maybe Int
lineAbove e start
  | start <= e.base = Nothing
  | otherwise =
      let prev = lineStart e (breakStart (start - 1))
          k = skipSpaces e prev
          b = byteAt e k
      in if isBreak b || b == HASH then lineAbove e prev else Just k
  where
    -- The start of the line break that ends at the index, e.g. of CR LF.
    breakStart :: Int -> Int
    breakStart j
      | j > e.base && byteBefore e j == CR && byteAt e j == LF = j - 1
      | otherwise = j

-- | The index after the "- " indicators at the index, as in "- - key: value".
skipListItems :: Env -> Int -> Int
skipListItems e i = if isListItem e i then skipListItems e (skipWhites e (i + 1)) else i

-- | A colon that ends an implicit key is at the index.
isKeyColon :: Env -> Int -> Bool
isKeyColon e i = byteAt e i == COLON && (let b = byteAt e (i + 1) in b == 0 || isWhite b || isBreak b)

-- | A block sequence entry starts at the index.
isListItem :: Env -> Int -> Bool
isListItem e i = byteAt e i == MINUS && (let b = byteAt e (i + 1) in b == 0 || isWhite b || isBreak b)

unexpectedChar :: Env -> Int -> String
unexpectedChar e i
  | w < 0x80 = "unexpected " ++ show (chr (fromIntegral w))
  | otherwise = "unexpected " ++ show (T.head (slice e i e.end))
  where
    w :: Word8
    w = byteAt e i

----------------------------------------
-- Characters

pattern
  TAB
  , LF
  , CR
  , SPACE
  , EXCL
  , DQUOTE
  , HASH
  , PERCENT
  , AMP
  , SQUOTE
  , COMMA
  , MINUS
  , DOT
  , COLON
  , LESS
  , GREATER
  , QUESTION
  , AT
  , LBRACKET
  , BACKSLASH
  , RBRACKET
  , GRAVE
  , LBRACE
  , PIPE
  , RBRACE
  , STAR
    :: Word8
pattern TAB = 0x09
pattern LF = 0x0A
pattern CR = 0x0D
pattern SPACE = 0x20
pattern EXCL = 0x21
pattern DQUOTE = 0x22
pattern HASH = 0x23
pattern PERCENT = 0x25
pattern AMP = 0x26
pattern SQUOTE = 0x27
pattern STAR = 0x2A
pattern COMMA = 0x2C
pattern MINUS = 0x2D
pattern DOT = 0x2E
pattern COLON = 0x3A
pattern LESS = 0x3C
pattern GREATER = 0x3E
pattern QUESTION = 0x3F
pattern AT = 0x40
pattern LBRACKET = 0x5B
pattern BACKSLASH = 0x5C
pattern RBRACKET = 0x5D
pattern GRAVE = 0x60
pattern LBRACE = 0x7B
pattern PIPE = 0x7C
pattern RBRACE = 0x7D

isWhite :: Word8 -> Bool
isWhite w = w == SPACE || w == TAB

isBreak :: Word8 -> Bool
isBreak w = w == LF || w == CR

-- | ns-char. Every byte of a multibyte character counts, because the input
-- contains printable characters only.
isNsChar :: Word8 -> Bool
isNsChar w = w > SPACE && w /= 0x7F

isFlowIndicator :: Word8 -> Bool
isFlowIndicator w =
  w == COMMA
    || w == LBRACKET
    || w == RBRACKET
    || w == LBRACE
    || w == RBRACE

isIndicator :: Word8 -> Bool
isIndicator w = w < 0x80 && testBit indicators (fromIntegral w)
  where
    indicators :: Integer
    indicators = foldr (\c acc -> setBit acc (ord c)) 0 ("-?:,[]{}#&*!|>'\"%@`" :: String)

isDecDigit :: Word8 -> Bool
isDecDigit w = w >= 0x30 && w <= 0x39

isHexDigit' :: Word8 -> Bool
isHexDigit' w = isDecDigit w || (w >= 0x41 && w <= 0x46) || (w >= 0x61 && w <= 0x66)

hexValue :: Word8 -> Int
hexValue w
  | w <= 0x39 = fromIntegral w - 0x30
  | w <= 0x46 = fromIntegral w - 0x37
  | otherwise = fromIntegral w - 0x57

isWordChar :: Word8 -> Bool
isWordChar w =
  isDecDigit w
    || (w >= 0x41 && w <= 0x5A)
    || (w >= 0x61 && w <= 0x7A)
    || w == MINUS

-- | ns-uri-char without the escaped characters.
isUriChar :: Word8 -> Bool
isUriChar w = isWordChar w || w `elem` extra
  where
    extra :: [Word8]
    extra = map (fromIntegral . ord) "#;/?:@&=+$,_.!~*'()[]"

-- | ns-tag-char without the escaped characters.
isTagChar :: Word8 -> Bool
isTagChar w = isUriChar w && w /= EXCL && not (isFlowIndicator w)

isAnchorChar :: Word8 -> Bool
isAnchorChar w = isNsChar w && not (isFlowIndicator w)

isBom :: Env -> Int -> Bool
isBom e i = byteAt e i == 0xEF && byteAt e (i + 1) == 0xBB && byteAt e (i + 2) == 0xBF

----------------------------------------
-- Contexts

data Ctx = BlockOut | BlockIn | FlowOut | FlowIn | BlockKey | FlowKey
  deriving stock (Eq)

isKeyCtx :: Ctx -> Bool
isKeyCtx c = c == BlockKey || c == FlowKey

-- | ns-plain-safe(c), with 'isFlowCtx' of c as the flag. The function takes
-- the flag, not the context, so that a caller can compute it once for all the
-- bytes of a scalar.
isPlainSafe :: Bool -> Word8 -> Bool
isPlainSafe flow w = isNsChar w && not (flow && isFlowIndicator w)

-- | The flow indicators end a plain scalar in the context.
isFlowCtx :: Ctx -> Bool
isFlowCtx c = c == FlowIn || c == FlowKey

-- | in-flow(c)
inFlow :: Ctx -> Ctx
inFlow c = if isKeyCtx c then FlowKey else FlowIn

----------------------------------------
-- Scanning helpers

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
  | i - 3 >= e.base
      && A.unsafeIndex e.array (i - 3) == 0xEF
      && A.unsafeIndex e.array (i - 2) == 0xBB
      && A.unsafeIndex e.array (i - 1) == 0xBF =
      isStartOfLine e (i - 3)
  | otherwise = False

-- | Skip the empty lines of a flow scalar after a line break and the line
-- prefix of the next line (@l-empty(n,FLOW-IN)* s-flow-line-prefix(n)@).
-- Return the number of empty lines and the index of the content, or
-- 'Nothing' if the next line is indented less than the scalar.
flowFold :: Env -> Int -> Int -> Maybe (Int, Int)
flowFold e n = go 0
  where
    go :: Int -> Int -> Maybe (Int, Int)
    go !k i =
      let s = skipSpaces e i
          indented = s - i >= n
          w = skipWhites e s
      in if
           | indented && isBreak (byteAt e w) -> go (k + 1) (breakEnd e w)
           | not indented && isBreak (byteAt e s) -> go (k + 1) (breakEnd e s)
           | indented -> Just (k, w)
           | otherwise -> Nothing

-- | The text of a line folding with the given number of empty lines.
foldText :: Int -> T.Text
foldText = \case
  0 -> " "
  k -> T.replicate k "\n"

----------------------------------------
-- Basic structures

startOfLine :: P ()
startOfLine = do
  e <- env
  p <- pos
  guardP $ isStartOfLine e p

-- | s-indent(n)
sIndent :: Int -> P ()
sIndent n = do
  e <- env
  p <- pos
  let q = p + max 0 n
  if skipSpacesTo e p q == q then setPos q else failure
  where
    skipSpacesTo :: Env -> Int -> Int -> Int
    skipSpacesTo e i q
      | i < q && byteAt e i == SPACE = skipSpacesTo e (i + 1) q
      | otherwise = i

-- | Count the spaces at the current position.
countSpaces :: P Int
countSpaces = do
  e <- env
  p <- pos
  pure $ skipSpaces e p - p

-- | s-separate-in-line
sSeparateInLine :: P ()
sSeparateInLine = do
  e <- env
  p <- pos
  let q = skipWhites e p
  if q > p then setPos q else startOfLine

-- | c-nb-comment-text
cNbCommentText :: P ()
cNbCommentText = do
  char HASH
  skipWhile $ \w -> w /= 0 && not (isBreak w)

-- | b-break
bBreak :: P ()
bBreak = do
  e <- env
  p <- pos
  if isBreak (byteAt e p) then setPos (breakEnd e p) else failure

atEnd :: P ()
atEnd = do
  e <- env
  p <- pos
  guardP $ p >= e.end

-- | b-comment
bComment :: P ()
bComment = bBreak <|> atEnd

-- | s-b-comment
sBComment :: P ()
sBComment = do
  optional_ $ sSeparateInLine >> optional_ cNbCommentText
  bComment

-- | l-comment
lComment :: P ()
lComment = do
  sSeparateInLine
  optional_ cNbCommentText
  bComment

-- | s-l-comments
sLComments :: P ()
sLComments = do
  sBComment <|> startOfLine
  many_ lComment

-- | s-separate(n,c)
sSeparate :: Int -> Ctx -> P ()
sSeparate n c
  | isKeyCtx c = sSeparateInLine
  | otherwise = sSeparateLines n

-- | s-separate-lines(n)
sSeparateLines :: Int -> P ()
sSeparateLines n = (sLComments >> sFlowLinePrefix n) <|> sSeparateInLine

-- | s-flow-line-prefix(n)
sFlowLinePrefix :: Int -> P ()
sFlowLinePrefix n = do
  sIndent n
  optional_ sSeparateInLine

----------------------------------------
-- Stream

defaultHandles :: M.Map T.Text T.Text
defaultHandles = M.fromList [("!", "!"), ("!!", "tag:yaml.org,2002:")]

-- | A @---@ or @...@ marker at the start of a line.
isMarker :: Env -> Int -> Bool
isMarker e i =
  let w = byteAt e i
  in (w == MINUS || w == DOT)
       && byteAt e (i + 1) == w
       && byteAt e (i + 2) == w
       && (let w3 = byteAt e (i + 3) in w3 == 0 || isWhite w3 || isBreak w3)
       && isStartOfLine e i

-- | l-yaml-stream. The markers are the indices of the lines that start with
-- a document marker.
lYamlStream :: [Int] -> P [Document]
lYamlStream markers0 = do
  s <- pos
  lDocumentPrefix
  documents markers0 True s
  where
    -- The last argument is the index where the comments of the next
    -- document start.
    documents :: [Int] -> Bool -> Int -> P [Document]
    documents markers afterEnd prefix = do
      -- A byte order mark can come before a marker after a bare document.
      lDocumentPrefix
      e <- env
      p <- pos
      if
        | p >= e.end -> pure []
        | isMarker e p && byteAt e p == DOT -> do
            lDocumentSuffix
            lDocumentPrefix
            documents markers True prefix
        | isMarker e p -> document markers Nothing defaultHandles prefix
        | afterEnd && byteAt e p == PERCENT -> do
            (version, hs) <- directives
            q <- pos
            unless (isMarker e q && byteAt e q == MINUS) $
              throwAt q "expected a document start marker (---) after the directives"
            document markers version hs prefix
        | afterEnd -> bareDocument markers prefix
        | otherwise -> throwAt p "expected a document start marker (---)"

    document :: [Int] -> Maybe Version -> M.Map T.Text T.Text -> Int -> P [Document]
    document markers version hs prefix = do
      m <- pos
      advance 3
      p <- pos
      e <- env
      let (limit, markers') = nextMarker e markers p
      root <-
        withEnd limit . withHandles hs $
          lBareDocument <|> (eNode <* sLComments)
      finishDocument markers' version prefix (Just m) limit root

    bareDocument :: [Int] -> Int -> P [Document]
    bareDocument markers prefix = do
      p <- pos
      e <- env
      let (limit, markers') = nextMarker e markers p
      root <-
        withEnd limit lBareDocument <|> do
          fu <- furthest
          throwUnexpected fu
      finishDocument markers' Nothing prefix Nothing limit root

    -- The end of the document that starts at the index, and the markers
    -- after it.
    nextMarker :: Env -> [Int] -> Int -> (Int, [Int])
    nextMarker e markers p = case dropWhile (<= p) markers of
      m : ms -> (m, m : ms)
      [] -> (e.end, [])

    finishDocument
      :: [Int] -> Maybe Version -> Int -> Maybe Int -> Int -> Node -> P [Document]
    finishDocument markers version prefix marker limit root = do
      withEnd limit $ many_ lComment
      e <- env
      p <- pos
      when (p < limit) $ do
        fu <- furthest
        throwUnexpected (max fu p)
      let explicitEnd = isMarker e p && byteAt e p == DOT
      when explicitEnd lDocumentSuffix
      q <- pos
      when explicitEnd lDocumentPrefix
      r <- pos
      let doc =
            attachComments
              e
              prefix
              marker
              -- The lines after the last document belong to its end.
              (if r >= e.end then r else q)
              Document
                { version = version
                , explicitStart = isJust marker
                , explicitEnd = explicitEnd
                , docComments = noComments
                , root = root
                }
      (doc :) <$> documents markers explicitEnd q

-- | Stop with an error at the furthest failure.
throwUnexpected :: Int -> P a
throwUnexpected i = do
  e <- env
  let (j, msg) = unexpected e i
  throwAt j msg

-- | l-document-prefix, repeated.
lDocumentPrefix :: P ()
lDocumentPrefix = many_ $ do
  e <- env
  p <- pos
  if isBom e p then advance 3 else lComment

-- | l-document-suffix, without the comment lines after it. They belong to the
-- next document.
lDocumentSuffix :: P ()
lDocumentSuffix = do
  advance 3
  p <- pos
  sBComment <|> throwAt p "unexpected content after the document end marker (...)"

-- | l-directive, repeated, with the version and the tag handles they define.
directives :: P (Maybe Version, M.Map T.Text T.Text)
directives = go Nothing defaultHandles Set.empty
  where
    go
      :: Maybe Version
      -> M.Map T.Text T.Text
      -> Set.Set T.Text
      -> P (Maybe Version, M.Map T.Text T.Text)
    go version hs defined = do
      w <- peek
      if w /= PERCENT
        then pure (version, hs)
        else do
          p <- pos
          advance 1
          name <- directiveName
          case name of
            "YAML" -> do
              when (version /= Nothing) $
                throwAt p "duplicate %YAML directive"
              v <- yamlVersion p
              sLComments <|> throwAfter "unexpected content after the %YAML version"
              go (Just v) hs defined
            "TAG" -> do
              (handle, prefix) <- tagDirective
              when (handle `Set.member` defined)
                $ throwAt p
                $ "duplicate %TAG directive for " ++ T.unpack handle
              sLComments <|> throwAfter "unexpected content after the tag prefix"
              go version (M.insert handle prefix hs) (Set.insert handle defined)
            _ -> do
              many_ $ sSeparateInLine >> directiveParameter
              sLComments <|> throwAt p "invalid directive"
              go version hs defined

    -- Fail at the content after the white space at the position.
    throwAfter :: String -> P a
    throwAfter msg = do
      e <- env
      q <- pos
      throwAt (skipWhites e q) msg

    directiveName :: P T.Text
    directiveName = do
      e <- env
      p <- pos
      skipWhile isNsChar
      q <- pos
      when (q == p) $ throwAt p "expected a directive name"
      pure $ slice e p q

    directiveParameter :: P ()
    directiveParameter = do
      p <- pos
      skipWhile isNsChar
      q <- pos
      guardP (q > p)

    yamlVersion :: Int -> P Version
    yamlVersion p = do
      w <- peek
      unless (isWhite w) $ throwAfter badVersion
      sSeparateInLine
      v <- pos
      major <- number v
      char DOT <|> throwAt v badVersion
      minor <- number v
      w' <- peek
      when (isNsChar w') $ throwAt v badVersion
      when (major /= 1)
        $ throwAt p
        $ "unsupported YAML version " ++ show major ++ "." ++ show minor
      pure $ Version major minor
      where
        badVersion :: String
        badVersion = "expected a version such as 1.2 after %YAML"

        number :: Int -> P Int
        number v = do
          e <- env
          q <- pos
          skipWhile isDecDigit
          r <- pos
          when (r == q) $ throwAt v badVersion
          let digits = T.dropWhile (== '0') (slice e q r)
          -- A longer number could be beyond the range of Int.
          when (T.length digits > 9) $ throwAt p "unsupported YAML version"
          pure $ T.foldl' (\acc d -> acc * 10 + digitToInt d) 0 digits

    tagDirective :: P (T.Text, T.Text)
    tagDirective = do
      e <- env
      w <- peek
      unless (isWhite w) $
        throwAfter "expected a tag handle and a prefix after %TAG, e.g. %TAG !e! tag:example.com,2000:"
      sSeparateInLine
      h <- pos
      handle <- cTagHandle <|> throwAt h "invalid tag handle"
      w' <- peek
      unless (isWhite w') $ throwAfter noPrefix
      sSeparateInLine
      q <- pos
      first <- peek
      when (first == 0 || isBreak first || first == HASH) $ throwAt q noPrefix
      unless (first == EXCL || isTagChar first || first == PERCENT) $
        throwAt q "invalid tag prefix"
      when (first == EXCL) $ advance 1
      scan uriChars
      r <- pos
      pure (handle, percentDecode (slice e q r))
      where
        noPrefix :: String
        noPrefix = "expected a prefix after the tag handle, e.g. tag:example.com,2000:"

-- | c-tag-handle
cTagHandle :: P T.Text
cTagHandle = do
  e <- env
  p <- pos
  char EXCL
  named e p <|> secondary e p <|> pure "!"
  where
    named :: Env -> Int -> P T.Text
    named e p = do
      skipWhile isWordChar
      q <- pos
      guardP (q > p + 1)
      char EXCL
      pure $ slice e p (q + 1)

    secondary :: Env -> Int -> P T.Text
    secondary e p = do
      char EXCL
      pure $ slice e p (p + 2)

-- | Skip ns-uri-char*.
uriChars :: Env -> Int -> Int
uriChars e i
  | isUriChar w = uriChars e (i + 1)
  | w == PERCENT && isHexDigit' (byteAt e (i + 1)) && isHexDigit' (byteAt e (i + 2)) =
      uriChars e (i + 3)
  | otherwise = i
  where
    w :: Word8
    w = byteAt e i

-- | Skip ns-tag-char*.
tagChars :: Env -> Int -> Int
tagChars e i
  | isTagChar w = tagChars e (i + 1)
  | w == PERCENT && isHexDigit' (byteAt e (i + 1)) && isHexDigit' (byteAt e (i + 2)) =
      tagChars e (i + 3)
  | otherwise = i
  where
    w :: Word8
    w = byteAt e i

-- | Decode the %XX escapes of a tag.
percentDecode :: T.Text -> T.Text
percentDecode t
  | T.any (== '%') t = T.pack . decodeUtf8Chars $ go (T.unpack t)
  | otherwise = t
  where
    go :: String -> [Word8]
    go = \case
      '%' : a : b : rest ->
        fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
      c : rest -> encodeChar c ++ go rest
      [] -> []

    encodeChar :: Char -> [Word8]
    encodeChar c = A.toList arr 0 len
      where
        !(T.Text arr _ len) = T.singleton c

    decodeUtf8Chars :: [Word8] -> String
    decodeUtf8Chars = T.unpack . T.decodeUtf8Lenient . BS.pack

-- | l-bare-document
lBareDocument :: P Node
lBareDocument = sLBlockNode (-1) BlockIn

----------------------------------------
-- Nodes

-- | e-node
eNode :: P Node
eNode = eScalar noProps

-- | e-scalar with properties.
eScalar :: Props -> P Node
eScalar props = do
  e <- env
  p <- pos
  pure $ mkNode e p (toOffset e p) props (Scalar Plain T.empty)

-- | c-ns-properties(n,c)
cNsProperties :: Int -> Ctx -> P Props
cNsProperties n c = tagFirst <|> anchorFirst
  where
    tagFirst :: P Props
    tagFirst = do
      t <- cNsTagProperty
      a <- optional $ sSeparate n c >> cNsAnchorProperty
      pure $ Props a t

    anchorFirst :: P Props
    anchorFirst = do
      a <- cNsAnchorProperty
      t <- option NoTag $ sSeparate n c >> cNsTagProperty
      pure $ Props (Just a) t

-- | c-ns-anchor-property
cNsAnchorProperty :: P T.Text
cNsAnchorProperty = do
  char AMP
  nsAnchorName

-- | ns-anchor-name
nsAnchorName :: P T.Text
nsAnchorName = do
  e <- env
  p <- pos
  skipWhile isAnchorChar
  q <- pos
  guardP (q > p)
  pure $ slice e p q

-- | c-ns-tag-property
cNsTagProperty :: P Tag
cNsTagProperty = do
  e <- env
  p <- pos
  peek >>= guardP . (== EXCL)
  w <- peekAt 1
  if w == LESS
    then verbatim e p
    else shorthand e p <|> nonSpecific
  where
    verbatim :: Env -> Int -> P Tag
    verbatim e p = do
      advance 2
      q <- pos
      scan uriChars
      r <- pos
      w <- peek
      let t = slice e q r
      when (w /= GREATER || not (isLocal t || isGlobal t)) $ throwAt p "invalid verbatim tag"
      advance 1
      pure $ Tag t

    -- A local tag has a name after the "!".
    isLocal :: T.Text -> Bool
    isLocal t = case T.uncons t of
      Just ('!', rest) -> not (T.null rest)
      _ -> False

    -- A global tag is a URI, which starts with a scheme.
    isGlobal :: T.Text -> Bool
    isGlobal t = case T.uncons t of
      Just (c, rest) ->
        isAsciiLetter c && case T.uncons (T.dropWhile isSchemeChar rest) of
          Just (':', _) -> True
          _ -> False
      Nothing -> False

    isAsciiLetter :: Char -> Bool
    isAsciiLetter c = isAscii c && isAlpha c

    isSchemeChar :: Char -> Bool
    isSchemeChar c = isAscii c && (isAlphaNum c || c == '+' || c == '-' || c == '.')

    shorthand :: Env -> Int -> P Tag
    shorthand e p = do
      handle <- cTagHandle
      q <- pos
      scan tagChars
      r <- pos
      guardP (r > q)
      case M.lookup handle e.handles of
        Just prefix -> pure . Tag $ prefix <> percentDecode (slice e q r)
        Nothing -> throwAt p $ "undefined tag handle " ++ T.unpack handle

    nonSpecific :: P Tag
    nonSpecific = do
      char EXCL
      pure NonSpecificTag

-- | c-ns-alias-node
cNsAliasNode :: P Node
cNsAliasNode = do
  e <- env
  p <- pos
  char STAR
  name <- nsAnchorName
  q <- pos
  pure $ mkNode e p (toOffset e q) noProps (Alias name)

----------------------------------------
-- Flow scalars

-- | c-double-quoted(n,c)
cDoubleQuoted :: Int -> Ctx -> Props -> P Node
cDoubleQuoted n c props = withScan $ \e p ->
  let go :: Int -> Int -> [T.Text] -> Scanned T.Text
      go seg i acc = case byteAt e i of
        DQUOTE -> Done (i + 1) (finish (slice e seg i : acc))
        BACKSLASH
          | isBreak (byteAt e (i + 1)) ->
              if isKeyCtx c
                then NoMatch i
                else case flowFold e n (breakEnd e (i + 1)) of
                  Just (k, j) -> go j j (T.replicate k "\n" : slice e seg i : acc)
                  Nothing -> badIndent (i + 1)
          | i + 1 >= e.end -> unterminated i
          | otherwise -> case escape e (i + 1) of
              Just (t, j) -> go j j (t : slice e seg i : acc)
              Nothing -> Failed i (badEscape i)
        w
          | isWhite w ->
              let j = skipWhites e i
                  w' = byteAt e j
              in if
                   | isBreak w' -> fold i j acc
                   | otherwise -> go seg j acc
          | isBreak w -> fold i i acc
          | i >= e.end -> unterminated i
          | otherwise -> go seg (i + 1) acc
        where
          fold :: Int -> Int -> [T.Text] -> Scanned T.Text
          fold contentEnd brk acc'
            | isKeyCtx c = NoMatch brk
            | otherwise = case flowFold e n (breakEnd e brk) of
                Just (k, j) -> go j j (foldText k : slice e seg contentEnd : acc')
                Nothing -> badIndent brk

      unterminated :: Int -> Scanned T.Text
      unterminated i
        | isKeyCtx c = NoMatch i
        | otherwise = Failed p "unterminated double-quoted scalar"

      -- A hex escape with digits fails only for a bad code point. Any other
      -- invalid escape likely comes from a Windows path or a regular
      -- expression, e.g. "C:\Users" or "\d+".
      badEscape :: Int -> String
      badEscape i
        | chr (fromIntegral (byteAt e (i + 1))) `elem` ("xuU" :: String)
        , isHexDigit (chr (fromIntegral (byteAt e (i + 2)))) =
            "invalid escape sequence"
        | otherwise = "invalid escape sequence, write \\\\ for a backslash or use single quotes"

      badIndent :: Int -> Scanned T.Text
      badIndent i
        | nextContent i >= e.end || not (hasClosingQuote e DQUOTE (nextContent i)) = unterminated i
        | otherwise =
            Failed
              (nextContent i)
              "invalid indentation of a line in a double-quoted scalar"

      nextContent :: Int -> Int
      nextContent i = skipWhites e (skipBlankLines e i)
  in case go (p + 1) (p + 1) [] of
       Done q t -> Done q (mkNode e p (toOffset e q) props (Scalar DoubleQuoted t))
       NoMatch q -> NoMatch q
       Failed q msg -> Failed q msg

-- | c-single-quoted(n,c)
cSingleQuoted :: Int -> Ctx -> Props -> P Node
cSingleQuoted n c props = withScan $ \e p ->
  let go :: Int -> Int -> [T.Text] -> Scanned T.Text
      go seg i acc = case byteAt e i of
        SQUOTE
          | byteAt e (i + 1) == SQUOTE -> go (i + 2) (i + 2) ("'" : slice e seg i : acc)
          | otherwise -> Done (i + 1) (finish (slice e seg i : acc))
        w
          | isWhite w ->
              let j = skipWhites e i
              in if isBreak (byteAt e j) then fold i j acc else go seg j acc
          | isBreak w -> fold i i acc
          | i >= e.end -> unterminated i
          | otherwise -> go seg (i + 1) acc
        where
          fold :: Int -> Int -> [T.Text] -> Scanned T.Text
          fold contentEnd brk acc'
            | isKeyCtx c = NoMatch brk
            | otherwise = case flowFold e n (breakEnd e brk) of
                Just (k, j) -> go j j (foldText k : slice e seg contentEnd : acc')
                Nothing -> badIndent brk

      unterminated :: Int -> Scanned T.Text
      unterminated i
        | isKeyCtx c = NoMatch i
        | otherwise = Failed p "unterminated single-quoted scalar"

      badIndent :: Int -> Scanned T.Text
      badIndent i
        | nextContent i >= e.end || not (hasClosingQuote e SQUOTE (nextContent i)) = unterminated i
        | otherwise =
            Failed
              (nextContent i)
              "invalid indentation of a line in a single-quoted scalar"

      nextContent :: Int -> Int
      nextContent i = skipWhites e (skipBlankLines e i)
  in case go (p + 1) (p + 1) [] of
       Done q t -> Done q (mkNode e p (toOffset e q) props (Scalar SingleQuoted t))
       NoMatch q -> NoMatch q
       Failed q msg -> Failed q msg

-- | Skip the line break at the index and the blank lines after it.
skipBlankLines :: Env -> Int -> Int
skipBlankLines e i =
  let j = skipWhites e (breakEnd e i)
  in if isBreak (byteAt e j) then skipBlankLines e j else breakEnd e i

-- | The line from the index contains a closing quote. If it does not, a line
-- with a wrong indentation more likely follows a missing quote.
hasClosingQuote :: Env -> Word8 -> Int -> Bool
hasClosingQuote e quote i = case byteAt e i of
  w
    | w == quote -> True
    | w == BACKSLASH && quote == DQUOTE -> hasClosingQuote e quote (i + 2)
    | w == 0 || isBreak w -> False
    | otherwise -> hasClosingQuote e quote (i + 1)

finish :: [T.Text] -> T.Text
finish = \case
  [t] -> t
  ts -> T.concat (reverse ts)

-- | Decode the escape sequence after a backslash.
escape :: Env -> Int -> Maybe (T.Text, Int)
escape e i = case chr (fromIntegral (byteAt e i)) of
  '0' -> simple '\0'
  'a' -> simple '\a'
  'b' -> simple '\b'
  't' -> simple '\t'
  '\t' -> simple '\t'
  'n' -> simple '\n'
  'v' -> simple '\v'
  'f' -> simple '\f'
  'r' -> simple '\r'
  'e' -> simple '\ESC'
  ' ' -> simple ' '
  '"' -> simple '"'
  '/' -> simple '/'
  '\\' -> simple '\\'
  'N' -> simple '\x85'
  '_' -> simple '\xA0'
  'L' -> simple '\x2028'
  'P' -> simple '\x2029'
  'x' -> codePoint 2
  'u' -> case hexAt (i + 1) 4 of
    -- JSON escapes a character outside the Basic Multilingual Plane as a
    -- pair of surrogates.
    Just hi
      | hi >= 0xD800 && hi <= 0xDBFF
      , byteAt e (i + 5) == BACKSLASH
      , byteAt e (i + 6) == 0x75
      , Just lo <- hexAt (i + 7) 4
      , lo >= 0xDC00 && lo <= 0xDFFF ->
          fromCodePoint (0x10000 + (hi - 0xD800) * 0x400 + (lo - 0xDC00)) (i + 11)
    _ -> codePoint 4
  'U' -> codePoint 8
  _ -> Nothing
  where
    simple :: Char -> Maybe (T.Text, Int)
    simple ch = Just (T.singleton ch, i + 1)

    codePoint :: Int -> Maybe (T.Text, Int)
    codePoint k = do
      cp <- hexAt (i + 1) k
      fromCodePoint cp (i + 1 + k)

    fromCodePoint :: Int -> Int -> Maybe (T.Text, Int)
    fromCodePoint cp next
      | cp <= 0x10FFFF && (cp < 0xD800 || cp > 0xDFFF) = Just (T.singleton (chr cp), next)
      | otherwise = Nothing

    -- The value of k hex digits at the index.
    hexAt :: Int -> Int -> Maybe Int
    hexAt j k
      | all (isHexDigit' . byteAt e) [j .. j + k - 1] =
          Just $ foldl (\acc x -> acc * 16 + hexValue (byteAt e x)) 0 [j .. j + k - 1]
      | otherwise = Nothing

-- | ns-plain(n,c)
nsPlain :: Int -> Ctx -> Props -> P Node
nsPlain n c props = withScan $ \e p ->
  let w0 = byteAt e p
      firstOk =
        (isNsChar w0 && not (isIndicator w0))
          || ( (w0 == QUESTION || w0 == COLON || w0 == MINUS)
                 && isPlainSafe (isFlowCtx c) (byteAt e (p + 1))
             )
  in if not firstOk
       then NoMatch p
       else
         let q = plainLine e c (p + 1)
             node end t = mkNode e p (toOffset e end) props (Scalar Plain t)
         in if isKeyCtx c
              then Done q (node q (slice e p q))
              else case plainNextLines e n c q of
                ([], _) -> Done q (node q (slice e p q))
                (ts, r) -> Done r (node r (T.concat (slice e p q : ts)))

-- | The end of the plain scalar content on the current line.
plainLine :: Env -> Ctx -> Int -> Int
plainLine e c = go
  where
    go :: Int -> Int
    go i
      | isPlainSafe flow w && w /= COLON = go (i + 1)
      | w == COLON && isPlainSafe flow (byteAt e (i + 1)) = go (i + 1)
      | isWhite w =
          let j = skipWhites e i
          in if plainCharAfterWhite j then go (j + 1) else i
      | otherwise = i
      where
        w :: Word8
        w = byteAt e i

    plainCharAfterWhite :: Int -> Bool
    plainCharAfterWhite j =
      let w = byteAt e j
      in w /= HASH
           && isPlainSafe flow w
           && (w /= COLON || isPlainSafe flow (byteAt e (j + 1)))

    flow :: Bool
    flow = isFlowCtx c

-- | s-ns-plain-next-line(n,c)*. Return the text of the next lines and the
-- index after them.
plainNextLines :: Env -> Int -> Ctx -> Int -> ([T.Text], Int)
plainNextLines e n c = go
  where
    go :: Int -> ([T.Text], Int)
    go q =
      let j = skipWhites e q
      in if not (isBreak (byteAt e j))
           then ([], q)
           else case flowFold e n (breakEnd e j) of
             Just (k, t)
               | startsPlain t ->
                   let q' = plainLine e c (t + 1)
                       (ts, r) = go q'
                   in (foldText k : slice e t q' : ts, r)
             _ -> ([], q)

    startsPlain :: Int -> Bool
    startsPlain t =
      let w = byteAt e t
      in w /= HASH
           && isPlainSafe flow w
           && (w /= COLON || isPlainSafe flow (byteAt e (t + 1)))

    flow :: Bool
    flow = isFlowCtx c

----------------------------------------
-- Flow collections

-- | c-flow-sequence(n,c)
cFlowSequence :: Int -> Ctx -> Props -> P Node
cFlowSequence n c props = do
  e <- env
  p <- pos
  char LBRACKET
  optional_ $ sSeparate n c
  entries <- flowEntries n c' (nsFlowSeqEntry n c')
  closing c' p RBRACKET "flow sequence" "expected ',' or ']'"
  q <- pos
  pure $ mkNode e p (toOffset e q) props (Sequence Flow entries)
  where
    c' :: Ctx
    c' = inFlow c

-- | c-flow-mapping(n,c)
cFlowMapping :: Int -> Ctx -> Props -> P Node
cFlowMapping n c props = do
  e <- env
  p <- pos
  char LBRACE
  optional_ $ sSeparate n c
  entries <- flowEntries n c' (nsFlowMapEntry n c')
  closing c' p RBRACE "flow mapping" (expected entries)
  q <- pos
  pure $ mkNode e p (toOffset e q) props (Mapping Flow entries)
  where
    c' :: Ctx
    c' = inFlow c

    -- After a key with no value, the most likely mistake is a missing colon,
    -- e.g. in {"a" 1}.
    expected :: [(Node, Node)] -> String
    expected entries = case reverse entries of
      (k, v) : _
        | v.content == Scalar Plain T.empty
        , v.props == noProps
        , v.offset == k.endOffset ->
            "expected ':', ',' or '}'"
      _ -> "expected ',' or '}'"

-- | ns-s-flow-seq-entries(n,c) and ns-s-flow-map-entries(n,c).
flowEntries :: forall a. Int -> Ctx -> P a -> P [a]
flowEntries n c entry = go []
  where
    go :: [a] -> P [a]
    go acc = next <|> pure (reverse acc)
      where
        next :: P [a]
        next = do
          x <- entry
          optional_ $ sSeparate n c
          (char COMMA >> optional_ (sSeparate n c) >> go (x : acc))
            <|> pure (reverse (x : acc))

-- | The closing bracket of a flow collection that starts at the index. Its
-- absence is an error unless the collection is an implicit key, which the
-- parser can try again as a value. If the collection stops at the end of a
-- line, the error points to its start, which can be far away.
closing :: Ctx -> Int -> Word8 -> String -> String -> P ()
closing c start w kind msg = do
  e <- env
  p <- pos
  char w <|> if
    | c == FlowKey -> failure
    | atLineEnd e p -> throwAt start ("unterminated " ++ kind)
    | otherwise -> throwAt p (maybe msg id (mistake e p))
  where
    -- The separation after an entry goes on to the next line if the
    -- collection can continue there. So a stop at the end of a line means
    -- that the document ends or that the next line is indented too little.
    atLineEnd :: Env -> Int -> Bool
    atLineEnd e i
      | i >= e.end = True
      | otherwise = case byteAt e i of
          HASH -> let b = byteAt e (i - 1) in isWhite b || isBreak b
          b | isWhite b -> atLineEnd e (i + 1)
          b -> isBreak b

-- | ns-flow-seq-entry(n,c)
--
-- The grammar tries a JSON-like node as the key of a pair and then again as
-- a node, which takes exponential time for nested flow sequences. So the
-- parser reads the node once, and a JSON-like node becomes a key if it fits
-- one and a colon follows. The node is read once even if it fails, as it can
-- in a key.
nsFlowSeqEntry :: Int -> Ctx -> P Node
nsFlowSeqEntry n c = do
  e <- env
  p <- pos
  (pair e p <$> nsFlowPair n c) <|> nodeEntry e p
  where
    pair :: Env -> Int -> (Node, Node) -> Node
    pair e p (k, v) = mkNode e p v.endOffset noProps (Mapping Flow [(k, v)])

    nodeEntry :: Env -> Int -> P Node
    nodeEntry e p = do
      k <- nsFlowNode n c
      q <- pos
      let value = optional_ sSeparateInLine >> cNsFlowMapAdjacentValue n c
      if isJsonNode k && fitsKey e p q && all (not . isBreak . byteAt e) [p .. q - 1]
        then (pair e p . (k,) <$> value) <|> pure k
        else pure k

    -- The content of c-flow-json-node(n,c).
    isJsonNode :: Node -> Bool
    isJsonNode k = case k.content of
      Sequence Flow _ -> True
      Mapping Flow _ -> True
      Scalar SingleQuoted _ -> True
      Scalar DoubleQuoted _ -> True
      _ -> False

-- | ns-flow-map-entry(n,c)
nsFlowMapEntry :: Int -> Ctx -> P (Node, Node)
nsFlowMapEntry n c = explicit <|> nsFlowMapImplicitEntry n c
  where
    explicit :: P (Node, Node)
    explicit = do
      char QUESTION
      sSeparate n c
      nsFlowMapExplicitEntry n c

-- | ns-flow-map-explicit-entry(n,c)
nsFlowMapExplicitEntry :: Int -> Ctx -> P (Node, Node)
nsFlowMapExplicitEntry n c =
  nsFlowMapImplicitEntry n c <|> ((,) <$> eNode <*> eNode)

-- | ns-flow-map-implicit-entry(n,c)
nsFlowMapImplicitEntry :: Int -> Ctx -> P (Node, Node)
nsFlowMapImplicitEntry n c = yamlKeyEntry <|> cNsFlowMapEmptyKeyEntry n c <|> jsonKeyEntry
  where
    yamlKeyEntry :: P (Node, Node)
    yamlKeyEntry = do
      k <- nsFlowYamlNode n c
      v <- (optional_ (sSeparate n c) >> cNsFlowMapSeparateValue n c) <|> eNode
      pure (k, v)

    jsonKeyEntry :: P (Node, Node)
    jsonKeyEntry = do
      k <- cFlowJsonNode n c
      v <- (optional_ (sSeparate n c) >> cNsFlowMapAdjacentValue n c) <|> eNode
      pure (k, v)

-- | c-ns-flow-map-empty-key-entry(n,c)
cNsFlowMapEmptyKeyEntry :: Int -> Ctx -> P (Node, Node)
cNsFlowMapEmptyKeyEntry n c = do
  k <- eNode
  v <- cNsFlowMapSeparateValue n c
  pure (k, v)

-- | c-ns-flow-map-separate-value(n,c)
cNsFlowMapSeparateValue :: Int -> Ctx -> P Node
cNsFlowMapSeparateValue n c = do
  char COLON
  w <- peek
  guardP . not $ isPlainSafe (isFlowCtx c) w
  (sSeparate n c >> nsFlowNode n c) <|> eNode

-- | c-ns-flow-map-adjacent-value(n,c)
cNsFlowMapAdjacentValue :: Int -> Ctx -> P Node
cNsFlowMapAdjacentValue n c = do
  char COLON
  (optional_ (sSeparate n c) >> nsFlowNode n c) <|> eNode

-- | ns-flow-pair(n,c) without c-ns-flow-pair-json-key-entry(n,c), which
-- 'nsFlowSeqEntry' parses.
nsFlowPair :: Int -> Ctx -> P (Node, Node)
nsFlowPair n c = explicit <|> yamlKeyEntry <|> cNsFlowMapEmptyKeyEntry n c
  where
    explicit :: P (Node, Node)
    explicit = do
      char QUESTION
      sSeparate n c
      nsFlowMapExplicitEntry n c

    yamlKeyEntry :: P (Node, Node)
    yamlKeyEntry = do
      k <- nsSImplicitYamlKey FlowKey
      v <- cNsFlowMapSeparateValue n c
      pure (k, v)

-- | ns-s-implicit-yaml-key(c)
nsSImplicitYamlKey :: Ctx -> P Node
nsSImplicitYamlKey c = implicitKey $ nsFlowYamlNode 0 c

-- | c-s-implicit-json-key(c)
cSImplicitJsonKey :: Ctx -> P Node
cSImplicitJsonKey c = implicitKey $ cFlowJsonNode 0 c

-- | An implicit key with the separation after it. It is at most 1024
-- characters long.
implicitKey :: P Node -> P Node
implicitKey key = do
  e <- env
  p <- pos
  k <- key
  q <- pos
  guardP $ fitsKey e p q
  optional_ sSeparateInLine
  pure k

-- | The input between the indices has at most 1024 characters, the limit of
-- an implicit key.
fitsKey :: Env -> Int -> Int -> Bool
fitsKey e p q = q - p <= 1024 || (q - p <= 4096 && countChars <= 1024)
  where
    countChars :: Int
    countChars =
      length
        [() | x <- [p .. q - 1], let w = byteAt e x, w < 0x80 || w >= 0xC0]

----------------------------------------
-- Flow nodes

-- | ns-flow-yaml-node(n,c)
nsFlowYamlNode :: Int -> Ctx -> P Node
nsFlowYamlNode n c =
  peek >>= \case
    STAR -> cNsAliasNode
    w
      | w == EXCL || w == AMP -> do
          props <- cNsProperties n c
          (sSeparate n c >> nsPlain n c props) <|> empty props
      | otherwise -> nsPlain n c noProps
  where
    -- Properties before JSON-like content belong to c-flow-json-node, which
    -- ordered choice would not try after an empty node.
    empty :: Props -> P Node
    empty props = do
      notFollowedBy $ do
        optional_ $ sSeparate n c
        w <- peek
        guardP $ w == LBRACKET || w == LBRACE || w == SQUOTE || w == DQUOTE
      eScalar props

-- | c-flow-json-node(n,c)
cFlowJsonNode :: Int -> Ctx -> P Node
cFlowJsonNode n c = do
  props <- option noProps $ cNsProperties n c <* sSeparate n c
  cFlowJsonContent n c props

-- | ns-flow-node(n,c)
nsFlowNode :: Int -> Ctx -> P Node
nsFlowNode n c =
  peek >>= \case
    STAR -> cNsAliasNode
    w
      | w == EXCL || w == AMP -> do
          props <- cNsProperties n c
          (sSeparate n c >> nsFlowContent n c props) <|> eScalar props
      | otherwise -> nsFlowContent n c noProps

-- | ns-flow-content(n,c)
nsFlowContent :: Int -> Ctx -> Props -> P Node
nsFlowContent n c props =
  peek >>= \case
    LBRACKET -> cFlowSequence n c props
    LBRACE -> cFlowMapping n c props
    SQUOTE -> cSingleQuoted n c props
    DQUOTE -> cDoubleQuoted n c props
    _ -> nsPlain n c props

-- | c-flow-json-content(n,c)
cFlowJsonContent :: Int -> Ctx -> Props -> P Node
cFlowJsonContent n c props =
  peek >>= \case
    LBRACKET -> cFlowSequence n c props
    LBRACE -> cFlowMapping n c props
    SQUOTE -> cSingleQuoted n c props
    DQUOTE -> cDoubleQuoted n c props
    _ -> failure

----------------------------------------
-- Block scalars

data Chomping = Strip | Clip | Keep
  deriving stock (Eq)

-- | c-l+literal(n) and c-l+folded(n).
cLBlockScalar :: Int -> Props -> P Node
cLBlockScalar n props = do
  e <- env
  p <- pos
  indicator <- peek
  guardP $ indicator == PIPE || indicator == GREATER
  advance 1
  (chomping, explicitIndent) <- cBBlockHeader p
  q <- pos
  indent <- case explicitIndent of
    Just m -> pure $ max 0 n + m
    Nothing -> case detectIndent e n q of
      Right m -> pure m
      Left i ->
        throwAt
          i
          "a leading empty line of a block scalar has more spaces than the first non-empty line"
  let (lines_, trailing, r) = blockLines e indent q
      text = case indicator of
        PIPE -> literalText lines_
        _ -> foldedText lines_
      value = chomp chomping (not (null lines_)) trailing text
      style = if indicator == PIPE then Literal else Folded
      -- The empty lines after the content are not part of the scalar,
      -- unless it keeps them.
      contentEnd = case (chomping, reverse lines_) of
        (Keep, _) -> r
        (_, BlockLine _ (T.Text _ o l) : _) -> o + l
        (_, []) -> q
  setPos r
  lTrailComments indent
  pure $ mkNode e p (toOffset e contentEnd) props (Scalar style value)

-- | c-b-block-header(t). Return the chomping and the indentation indicator.
cBBlockHeader :: Int -> P (Chomping, Maybe Int)
cBBlockHeader p = do
  a <- peek
  b <- peekAt 1
  let (chomping, indent, k)
        | Just t <- chompingOf a, Just m <- indentOf b = (t, Just m, 2)
        | Just m <- indentOf a, Just t <- chompingOf b = (t, Just m, 2)
        | Just t <- chompingOf a = (t, Nothing, 1)
        | Just m <- indentOf a = (Clip, Just m, 1)
        | otherwise = (Clip, Nothing, 0)
  advance k
  e <- env
  q <- pos
  let content = skipWhites e q
  sBComment <|> if
    | isDecDigit (byteAt e q) ->
        throwAt q "the indentation indicator of a block scalar must be from 1 to 9"
    | content > q && isNsChar (byteAt e content) ->
        throwAt content "the content of a block scalar starts on the next line"
    | otherwise -> throwAt p "invalid block scalar header"
  pure (chomping, indent)
  where
    chompingOf :: Word8 -> Maybe Chomping
    chompingOf = \case
      MINUS -> Just Strip
      0x2B -> Just Keep
      _ -> Nothing

    indentOf :: Word8 -> Maybe Int
    indentOf w
      | w >= 0x31 && w <= 0x39 = Just (fromIntegral w - 0x30)
      | otherwise = Nothing

-- | Detect the content indentation of a block scalar from its first non-empty
-- line. Return the index of a leading empty line with too many spaces on
-- error.
detectIndent :: Env -> Int -> Int -> Either Int Int
detectIndent e n = go 0 Nothing
  where
    go :: Int -> Maybe Int -> Int -> Either Int Int
    go maxEmpty maxAt i =
      let s = skipSpaces e i
          k = s - i
          w = byteAt e s
      in if
           | isBreak w || (s >= e.end && k > 0) ->
               go (max maxEmpty k) (if k > maxEmpty then Just s else maxAt) (breakEnd e s)
           | s >= e.end || k <= n -> Right (max (n + 1) (max maxEmpty 1))
           | maxEmpty > k, Just j <- maxAt -> Left j
           | otherwise -> Right k

-- | A content line of a block scalar: the number of empty lines before it and
-- its text after the indentation.
data BlockLine = BlockLine !Int !T.Text

-- | Split the content of a block scalar into lines. Return the lines, the
-- number of empty lines after the last one and the index after them.
blockLines :: Env -> Int -> Int -> ([BlockLine], Int, Int)
blockLines e indent = go 0 []
  where
    go :: Int -> [BlockLine] -> Int -> ([BlockLine], Int, Int)
    go !empties acc i
      | i >= e.end = (reverse acc, empties, i)
      | otherwise =
          let s = skipSpacesMax i
              w = byteAt e s
          in if
               | isBreak w -> go (empties + 1) acc (breakEnd e s)
               | s >= e.end -> (reverse acc, empties + 1, s)
               | s - i == indent ->
                   let t = lineEnd s
                       acc' = BlockLine empties (slice e s t) : acc
                   in if t >= e.end
                        then (reverse acc', 0, t)
                        else go 0 acc' (breakEnd e t)
               | otherwise -> (reverse acc, empties, i)

    -- At most indent spaces.
    skipSpacesMax :: Int -> Int
    skipSpacesMax i = loop i
      where
        loop :: Int -> Int
        loop j
          | j - i < indent && byteAt e j == SPACE = loop (j + 1)
          | otherwise = j

    lineEnd :: Int -> Int
    lineEnd j
      | j < e.end && not (isBreak (byteAt e j)) = lineEnd (j + 1)
      | otherwise = j

literalText :: [BlockLine] -> T.Text
literalText = \case
  [] -> T.empty
  BlockLine k t : rest -> T.concat $ T.replicate k "\n" : t : concatMap line rest
  where
    line :: BlockLine -> [T.Text]
    line (BlockLine k t) = ["\n", T.replicate k "\n", t]

foldedText :: [BlockLine] -> T.Text
foldedText = \case
  [] -> T.empty
  BlockLine k t : rest -> T.concat $ T.replicate k "\n" : t : go (isSpaced t) rest
  where
    go :: Bool -> [BlockLine] -> [T.Text]
    go prevSpaced = \case
      [] -> []
      BlockLine k t : rest ->
        let spaced = isSpaced t
            sep
              | not prevSpaced && not spaced = foldText k
              | otherwise = T.replicate (k + 1) "\n"
        in sep : t : go spaced rest

    isSpaced :: T.Text -> Bool
    isSpaced t = case T.uncons t of
      Just (ch, _) -> ch == ' ' || ch == '\t'
      Nothing -> False

-- | Apply the chomping to the content of a block scalar. The end of the input
-- counts as a line break, as in the YAML test suite.
chomp :: Chomping -> Bool -> Int -> T.Text -> T.Text
chomp chomping hasContent trailing text = case chomping of
  Strip -> text
  Clip
    | hasContent -> text <> "\n"
    | otherwise -> text
  Keep
    | hasContent -> text <> T.replicate (trailing + 1) "\n"
    | otherwise -> T.replicate trailing "\n"

-- | l-trail-comments(n)
lTrailComments :: Int -> P ()
lTrailComments n = optional_ $ do
  k <- countSpaces
  guardP (k < n)
  advance k
  cNbCommentText
  bComment
  many_ lComment

----------------------------------------
-- Block collections

-- | l+block-sequence(n)
lBlockSequence :: Int -> Props -> P Node
lBlockSequence n props = do
  e <- env
  k <- countSpaces
  guardP (k > n)
  advance k
  p <- pos
  x <- cLBlockSeqEntry k
  xs <- many $ sIndent k >> cLBlockSeqEntry k
  pure $ mkNode e p (last (x : xs)).endOffset props (Sequence Block (x : xs))

-- | c-l-block-seq-entry(n)
cLBlockSeqEntry :: Int -> P Node
cLBlockSeqEntry n = do
  char MINUS
  w <- peek
  guardP . not $ isNsChar w
  sLBlockIndented n BlockIn

-- | s-l+block-indented(n,c)
sLBlockIndented :: Int -> Ctx -> P Node
sLBlockIndented n c = compact <|> sLBlockNode n c <|> (eNode <* sLComments)
  where
    compact :: P Node
    compact = do
      e <- env
      m <- countSpaces
      advance m
      p <- pos
      if mayStartEntry e p
        then nsLCompactSequence (n + 1 + m) <|> nsLCompactMapping (n + 1 + m)
        else nsLCompactSequence (n + 1 + m)

    -- An entry of a mapping has an explicit key or a colon on its first line.
    -- A key cannot start with the indicator of a sequence entry. Without this
    -- check, each level of a nested sequence would scan the rest of the line.
    mayStartEntry :: Env -> Int -> Bool
    mayStartEntry e p
      | byteAt e p == QUESTION = True
      | byteAt e p == MINUS && not (isNsChar (byteAt e (p + 1))) = False
      | otherwise = go p
      where
        go :: Int -> Bool
        go i = case byteAt e i of
          COLON -> True
          w
            | w == 0 || isBreak w -> False
            | otherwise -> go (i + 1)

-- | ns-l-compact-sequence(n)
nsLCompactSequence :: Int -> P Node
nsLCompactSequence n = do
  e <- env
  p <- pos
  x <- cLBlockSeqEntry n
  xs <- many $ sIndent n >> cLBlockSeqEntry n
  pure $ mkNode e p (last (x : xs)).endOffset noProps (Sequence Block (x : xs))

-- | l+block-mapping(n)
lBlockMapping :: Int -> Props -> P Node
lBlockMapping n props = do
  e <- env
  k <- countSpaces
  guardP (k > n)
  advance k
  p <- pos
  x <- nsLBlockMapEntry k
  xs <- many $ sIndent k >> nsLBlockMapEntry k
  pure $ mkNode e p (snd (last (x : xs))).endOffset props (Mapping Block (x : xs))

-- | ns-l-block-map-entry(n)
nsLBlockMapEntry :: Int -> P (Node, Node)
nsLBlockMapEntry n = cLBlockMapExplicitEntry n <|> nsLBlockMapImplicitEntry n

-- | c-l-block-map-explicit-entry(n)
cLBlockMapExplicitEntry :: Int -> P (Node, Node)
cLBlockMapExplicitEntry n = do
  char QUESTION
  w <- peek
  guardP . not $ isNsChar w
  k <- sLBlockIndented n BlockOut
  v <- lBlockMapExplicitValue <|> eNode
  pure (k, v)
  where
    lBlockMapExplicitValue :: P Node
    lBlockMapExplicitValue = do
      sIndent n
      char COLON
      w <- peek
      guardP . not $ isNsChar w
      sLBlockIndented n BlockOut

-- | ns-l-block-map-implicit-entry(n)
nsLBlockMapImplicitEntry :: Int -> P (Node, Node)
nsLBlockMapImplicitEntry n = do
  k <- nsSBlockMapImplicitKey <|> eNode
  v <- cLBlockMapImplicitValue n
  pure (k, v)
  where
    nsSBlockMapImplicitKey :: P Node
    nsSBlockMapImplicitKey = cSImplicitJsonKey BlockKey <|> nsSImplicitYamlKey BlockKey

-- | c-l-block-map-implicit-value(n)
cLBlockMapImplicitValue :: Int -> P Node
cLBlockMapImplicitValue n = do
  char COLON
  w <- peek
  guardP . not $ isNsChar w
  sLBlockNode n BlockOut <|> (eNode <* sLComments)

-- | ns-l-compact-mapping(n)
nsLCompactMapping :: Int -> P Node
nsLCompactMapping n = do
  e <- env
  p <- pos
  x <- nsLBlockMapEntry n
  xs <- many $ sIndent n >> nsLBlockMapEntry n
  pure $ mkNode e p (snd (last (x : xs))).endOffset noProps (Mapping Block (x : xs))

----------------------------------------
-- Block nodes

-- | s-l+block-node(n,c)
sLBlockNode :: Int -> Ctx -> P Node
sLBlockNode n c = do
  e <- env
  p <- pos
  if flowOnly e p
    then sLFlowInBlock n
    else sLBlockInBlock n c <|> sLFlowInBlock n
  where
    -- Block content starts with a property, an indicator of a block scalar or
    -- the end of the line. Other content on the same line is a flow node.
    flowOnly :: Env -> Int -> Bool
    flowOnly e p =
      not (isStartOfLine e p)
        && let w = byteAt e (skipWhites e p)
           in not
                ( w == 0
                    || isBreak w
                    || w == HASH
                    || w == PIPE
                    || w == GREATER
                    || w == EXCL
                    || w == AMP
                )

-- | s-l+flow-in-block(n)
sLFlowInBlock :: Int -> P Node
sLFlowInBlock n = do
  sSeparate (n + 1) FlowOut
  node <- nsFlowNode (n + 1) FlowOut
  sLComments
  pure node

-- | s-l+block-in-block(n,c)
sLBlockInBlock :: Int -> Ctx -> P Node
sLBlockInBlock n c = sLBlockScalar n c <|> sLBlockCollection n c

-- | s-l+block-scalar(n,c)
sLBlockScalar :: Int -> Ctx -> P Node
sLBlockScalar n c = do
  sSeparate (n + 1) c
  props <- option noProps $ cNsProperties (n + 1) c <* sSeparate (n + 1) c
  cLBlockScalar n props

-- | s-l+block-collection(n,c)
sLBlockCollection :: Int -> Ctx -> P Node
sLBlockCollection n c = do
  props <- withProps <|> (sLComments >> pure noProps)
  lBlockSequence (if c == BlockOut then n - 1 else n) props <|> lBlockMapping n props
  where
    -- If both properties do not end the line, the second one can belong to
    -- the first key of the mapping.
    withProps :: P Props
    withProps = do
      sSeparate (n + 1) c
      (cNsProperties (n + 1) c <* sLComments) <|> (oneProperty <* sLComments)

    oneProperty :: P Props
    oneProperty =
      (Props Nothing <$> cNsTagProperty)
        <|> ((\a -> Props (Just a) NoTag) <$> cNsAnchorProperty)

-- | A node without comments from the given index to the given offset.
mkNode :: Env -> Int -> Offset -> Props -> Content -> Node
mkNode e p end props c =
  Node
    { offset = toOffset e p
    , endOffset = end
    , props = props
    , comments = noComments
    , content = c
    }
