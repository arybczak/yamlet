{-# OPTIONS_HADDOCK not-home #-}

-- | The messages of parse errors. The parser only knows the position at
-- which it failed, so these functions look at the input around it to name
-- the likely mistake, e.g. a missing colon after a key.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Parser.Hints
  ( unexpected
  , mistake
  ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Word

import Yamlet.Internal.Parser.Chars
import Yamlet.Internal.Parser.Monad

-- | The location and the message of the error for the furthest position at
-- which the parser failed.
unexpected :: Env -> Int -> (Int, String)
unexpected e i = case indentationTab (i - 1) Nothing of
  Just tab -> (tab, "tabs cannot be used for indentation")
  Nothing
    | Just start <- propertiesLine ->
        (start, "an anchor or a tag cannot be on a line of its own here, write it after the key or the '-'")
  Nothing | Just r <- blockMistake e i -> r
  Nothing | afterComment -> (i, "a comment ends a plain scalar, so this line cannot continue it")
  Nothing
    | Just colon <- aliasColon ->
        (colon, "the name of the alias includes the ':', write a space before ':' if the alias is a key")
  Nothing -> (i,) $ case byteAt e i of
    w
      | byteBefore e i == STAR && not (isAnchorChar w) -> "expected an alias name after '*'"
      | byteBefore e i == AMP && not (isAnchorChar w) -> "expected an anchor name after '&'"
      | w == 0 -> "unexpected end of input"
      | indented -> fromMaybe "unexpected indentation" (indentationMistake e i)
      | isBreak w -> "unexpected end of line"
      | i > e.base && isBreak (byteBefore e i), Just msg <- indentationMistake e i -> msg
      | w == COLON && firstColon && not (fitsKey e entryStart i) ->
          "a key can be at most 1024 characters long, write a longer key after '? '"
      | w == COLON && multiLineKey -> "unexpected ':', a key must be on a single line"
      | w == COLON && firstColon && valueColon && onStartMarkerLine ->
          "unexpected ':', a mapping cannot start on the line of '---'"
      -- A colon on the first line of a key does not fail, so the scalar
      -- before this one started on a line above.
      | w == COLON && firstColon && valueColon && isJust (lineAbove e (lineStart e i)) ->
          "unexpected ':', this line continues the scalar from the line above, check the indentation and the line above"
      | w == COLON && valueColon ->
          "unexpected ':', quote the value if it contains \": \""
      | itemAfterKey -> "unexpected '-', a list cannot start on the line of its key"
      | itemAfterProperty -> "unexpected '-', a list cannot start on the line of its anchor or tag"
      | Just msg <- mistake e i -> msg
      | Just node <- endBefore -> unexpectedChar e i ++ " after the end of " ++ node
      | otherwise -> unexpectedChar e i
  where
    -- The node that ends before the index on its line, as in
    -- "key: "value" more".
    endBefore :: Maybe String
    endBefore
      | not (isNsChar (byteAt e i)) = Nothing
      | b == RBRACKET || b == RBRACE = Just "a flow collection"
      | (b == DQUOTE || b == SQUOTE) && j < i = Just "a quoted scalar"
      | otherwise = Nothing
      where
        j :: Int
        j = skipBackWhites e i

        b :: Word8
        b = byteBefore e j

    -- The index starts a line that looks like the continuation of a plain
    -- scalar, and the closest line above that is not blank has a comment.
    afterComment :: Bool
    afterComment =
      i == skipSpaces e (lineStart e i)
        && isNsChar (byteAt e i)
        && not (isListItem e i)
        && not (any (isKeyColon e) [i .. lineEnd i - 1])
        && isNothing (mistake e i)
        && commentAbove (lineStart e i)
      where
        commentAbove :: Int -> Bool
        commentAbove start
          | start <= e.base = False
          | otherwise =
              let prev = lineStart e (start - 1)
                  k = skipWhites e prev
              in if isBreak (byteAt e k) then commentAbove prev else hasComment k

        hasComment :: Int -> Bool
        hasComment j = byteAt e j == HASH || any comment [j + 1 .. lineEnd j - 1]

        comment :: Int -> Bool
        comment j = byteAt e j == HASH && isWhite (byteBefore e j)

        lineEnd :: Int -> Int
        lineEnd j = if byteAt e j == 0 || isBreak (byteAt e j) then j else lineEnd (j + 1)

    -- The start of the line of the index if the line has only anchors and
    -- tags, as in "&anchor".
    propertiesLine :: Maybe Int
    propertiesLine =
      let start = skipSpaces e (lineStart e i)
      in if onlyProperties start then Just start else Nothing
      where
        onlyProperties :: Int -> Bool
        onlyProperties j =
          let b = byteAt e j
              next = skipWhites e (wordEnd j)
              b' = byteAt e next
          in (b == AMP || b == EXCL)
               && (b' == 0 || isBreak b' || b' == HASH || onlyProperties next)

        wordEnd :: Int -> Int
        wordEnd j = if isNsChar (byteAt e j) then wordEnd (j + 1) else j

    -- The ':' that ends an alias name before the index, as in "*x: 1". An
    -- alias name can contain ':'.
    aliasColon :: Maybe Int
    aliasColon =
      let j = skipBackWhites e i
          start = wordStart e j
      in if j > start && byteBefore e j == COLON && byteAt e start == STAR
           then Just (j - 1)
           else Nothing

    -- A key before the index that started on a line above. A key that ends
    -- here on one line does not fail.
    multiLineKey :: Bool
    multiLineKey =
      multiLineCollection || firstColon && (byteBefore e i == DQUOTE || byteBefore e i == SQUOTE)

    -- A flow collection ends before the index and starts on a line above.
    multiLineCollection :: Bool
    multiLineCollection =
      let b = byteBefore e i
      in (b == RBRACKET || b == RBRACE) && go (i - 2) (1 :: Int) False
      where
        go :: Int -> Int -> Bool -> Bool
        go j depth crossed
          | j < e.base = False
          | c == RBRACKET || c == RBRACE = go (j - 1) (depth + 1) crossed
          | c == LBRACKET || c == LBRACE = if depth == 1 then crossed else go (j - 1) (depth - 1) crossed
          | otherwise = go (j - 1) depth (crossed || isBreak c)
          where
            c :: Word8
            c = byteAt e j

    onStartMarkerLine :: Bool
    onStartMarkerLine = isMarker e (lineStart e i) && byteAt e (lineStart e i) == MINUS

    -- The start of the entry on the line, after any "- ".
    entryStart :: Int
    entryStart = skipListItems e (skipSpaces e (lineStart e i))

    -- No colon that ends a key precedes the index on its line.
    firstColon :: Bool
    firstColon = not (any (isKeyColon e) [entryStart .. i - 1])

    -- A list item right after a key, as in "a: - b".
    itemAfterKey :: Bool
    itemAfterKey = isListItem e i && byteBefore e (skipBackWhites e i) == COLON

    -- A list item right after an anchor or a tag, as in "&a - b".
    itemAfterProperty :: Bool
    itemAfterProperty =
      let j = skipBackWhites e i
          b = byteAt e (wordStart e j)
      in isListItem e i && j < i && (b == AMP || b == EXCL)

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
        tab' = if byteAt e i == TAB then Just (fromMaybe i tab) else tab

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
  -- In the block style, these characters start a block scalar and do not fail.
  | w == PIPE || w == GREATER =
      Just $ unexpectedChar e i ++ ", a block scalar cannot be inside a flow collection"
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
          _ | openQuote -> (i, "a key must be on a single line")
          colon : _ -> (colon + 1, "expected a space after ':'")
          [] -> (i, "expected ':' after the key")
    | otherwise -> Nothing
  where
    -- The line starts a quoted scalar that does not end on it, as in "a
    -- quoted key on two lines".
    openQuote :: Bool
    openQuote =
      let q = byteAt e start
      in (q == DQUOTE || q == SQUOTE) && q `notElem` [byteAt e j | j <- [start + 1 .. i - 1]]

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
