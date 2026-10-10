{-# OPTIONS_HADDOCK not-home #-}

-- | The messages of parse errors. The parser only knows the position at
-- which it failed, so these functions look at the input around it to name
-- the likely mistake, e.g. a missing colon after a key.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Parser.Hints
  ( unexpected
  , flowError
  , codePointName
  , firstTab
  , tabMessage
  , keyLengthMessage
  ) where

import Control.Monad
import Data.Char
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Word
import Numeric

import Yamlet.Internal.Chars
import Yamlet.Internal.Parser.Monad
import Yamlet.Internal.Parser.Scan
import Yamlet.Internal.Utils

-- | The location and the message of the error for the furthest position at
-- which the parser failed, and a tab before the position on its line that
-- can be the cause instead, with 'tabMessage'.
unexpected :: Env -> Int -> (Maybe Int, (Int, String))
unexpected input i = (tabCause, other)
  where
    tabCause :: Maybe Int
    tabCause = case indentationTab (i - 1) Nothing of
      Nothing
        | byteAt e i == COLON && firstColonFrom contentStart ->
            firstTab e lineStart contentStart
      t -> t

    other :: (Int, String)
    other
      | Just start <- propertiesLine =
          ( start
          , "an anchor or a tag cannot be on a line of its own here, write it after the key or the '-'"
          )
      | Just r <- blockMistake = r
      | afterComment =
          (i, "a comment ends a plain scalar, so this line cannot continue it")
      | Just colon <- aliasColon e i = (colon, aliasColonMessage)
      | otherwise = (i,) $ case byteAt e i of
          w
            | byteBefore e i == STAR && not (isAnchorChar w) ->
                "expected an alias name after '*'"
            | byteBefore e i == AMP && not (isAnchorChar w) ->
                "expected an anchor name after '&'"
            | w == 0 -> "unexpected end of input"
            | indented, Just msg <- indentationMistake -> msg
            | indented, Just msg <- mistakeIn e False i -> msg
            | indented, not alignedWithEntry -> "unexpected indentation"
            | isBreak w -> "unexpected end of line"
            | i > e.base && isBreak (byteBefore e i)
            , Just msg <- indentationMistake ->
                msg
            | w == COLON && firstColon && not (fitsKey e entryStart i) -> keyLengthMessage
            | w == COLON && multiLineKey ->
                "unexpected ':', a key must be on a single line"
            | w == COLON && firstColon && valueColon && onStartMarkerLine ->
                "unexpected ':', a mapping cannot start on the line of '---'"
            -- A colon on the first line of a key does not fail, so the scalar
            -- before this one started on a line above.
            | w == COLON
                && firstColon
                && valueColon
                && isJust (lineAbove (lineStartAt e i)) ->
                "unexpected ':', this line continues the scalar from the line above, check the indentation and the line above"
            | w == COLON && valueColon ->
                "unexpected ':', quote the value if it contains \": \""
            | itemAfterKey -> "unexpected '-', a list cannot start on the line of its key"
            | itemAfterProperty ->
                "unexpected '-', a list cannot start on the line of its anchor or tag"
            | itemAfterStartMarker ->
                "unexpected '-', a list cannot start on the line of '---'"
            | Just msg <- mistakeIn e False i -> msg
            | Just node <- endBefore -> unexpectedChar e i ++ " after the end of " ++ node
            | otherwise -> unexpectedChar e i

    e :: Env
    e = afterBoms input

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
    -- scalar, the closest line above that is not blank has a comment, and
    -- the content above ends with a plain scalar.
    afterComment :: Bool
    afterComment =
      i == skipSpaces e (lineStartAt e i)
        && isNsChar (byteAt e i)
        && not (isListItem i)
        && not (any isKeyColon [i .. lineEndAt e i - 1])
        && isNothing (mistakeIn e False i)
        && commentAbove (lineStartAt e i)
        && maybe False endsPlain (lineAbove (lineStartAt e i))
      where
        -- The line with the content at the index ends with a plain scalar,
        -- not with a quoted scalar, a flow collection, an alias, an anchor,
        -- a tag, an indicator or the header of a block scalar, and it is not
        -- a line of a block scalar.
        endsPlain :: Int -> Bool
        endsPlain k =
          let end = lineContentEnd e k
              start = wordStart e end
              b = byteBefore e end
              w = byteAt e start
          in end > k
               && b /= SQUOTE
               && b /= DQUOTE
               && b /= RBRACKET
               && b /= RBRACE
               && b /= COLON
               && w /= STAR
               && w /= AMP
               && w /= EXCL
               && not (end - start == 1 && (w == MINUS || w == QUESTION))
               && not (endsWithBlockHeader k)
               && not (inBlockScalar k)

        -- The closest line above that is indented less starts a block
        -- scalar.
        inBlockScalar :: Int -> Bool
        inBlockScalar k = go k
          where
            go :: Int -> Bool
            go j = case lineAbove (lineStartAt e j) of
              Nothing -> False
              Just above
                | columnOf above < columnOf k -> endsWithBlockHeader above
                | otherwise -> go above

        commentAbove :: Int -> Bool
        commentAbove start
          | start <= e.base = False
          | otherwise =
              let prev = lineStartAt e (start - 1)
                  k = skipWhites e (skipBoms e prev)
              in if isBreak (byteAt e k) then commentAbove prev else hasComment k

        -- A quoted scalar can contain " #", as in "\"x #y\"", so a quote
        -- that can close a scalar after the '#' shows that the '#' may not
        -- start a comment.
        hasComment :: Int -> Bool
        hasComment j = case L.find startsComment [j .. end - 1] of
          Just h -> not (any closingQuote [h + 1 .. end - 1])
          Nothing -> False
          where
            end :: Int
            end = lineEndAt e j

            startsComment :: Int -> Bool
            startsComment k = byteAt e k == HASH && (k == j || isWhite (byteBefore e k))

        closingQuote :: Int -> Bool
        closingQuote k =
          let b = byteAt e k in (b == SQUOTE || b == DQUOTE) && canEndQuoted e (k + 1)

    -- The start of the line of the index if the line has only anchors and
    -- tags, as in "&anchor".
    propertiesLine :: Maybe Int
    propertiesLine =
      let start = skipSpaces e (lineStartAt e i)
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

    -- A key before the index that started on a line above. A key that ends
    -- here on one line does not fail.
    multiLineKey :: Bool
    multiLineKey =
      multiLineCollection
        || firstColon && (byteBefore e i == DQUOTE || byteBefore e i == SQUOTE)

    -- A flow collection ends before the index and starts on a line above.
    multiLineCollection :: Bool
    multiLineCollection =
      let b = byteBefore e i
      in (b == RBRACKET || b == RBRACE) && go (i - 2) 1 False
      where
        go :: Int -> Int -> Bool -> Bool
        go j depth crossed
          | j < e.base = False
          | c == RBRACKET || c == RBRACE = go (j - 1) (depth + 1) crossed
          | c == LBRACKET || c == LBRACE =
              if depth == 1 then crossed else go (j - 1) (depth - 1) crossed
          | otherwise = go (j - 1) depth (crossed || isBreak c)
          where
            c :: Word8
            c = byteAt e j

    onStartMarkerLine :: Bool
    onStartMarkerLine = isStartMarker e markerStart

    -- A byte order mark can come before a marker.
    markerStart :: Int
    markerStart = skipBoms e (lineStartAt e i)

    -- The start of the entry on the line, after any "- ".
    entryStart :: Int
    entryStart = skipListItems (skipSpaces e (lineStartAt e i))

    firstColon :: Bool
    firstColon = firstColonFrom entryStart

    -- No colon that ends a key is from the given index to the index of the
    -- error, other than in a flow collection.
    firstColonFrom :: Int -> Bool
    firstColonFrom start = go start 0
      where
        go :: Int -> Int -> Bool
        go j depth
          | j >= i = True
          | b == LBRACKET || b == LBRACE = go (j + 1) (depth + 1)
          | b == RBRACKET || b == RBRACE = go (j + 1) (max 0 (depth - 1))
          | depth == 0 && isKeyColon j = False
          | otherwise = go (j + 1) depth
          where
            b :: Word8
            b = byteAt e j

    -- A list item right after a key, as in "a: - b".
    itemAfterKey :: Bool
    itemAfterKey = isListItem i && byteBefore e (skipBackWhites e i) == COLON

    -- A list item right after an anchor or a tag, as in "&a - b".
    itemAfterProperty :: Bool
    itemAfterProperty =
      let j = skipBackWhites e i
          b = byteAt e (wordStart e j)
      in isListItem i && j < i && (b == AMP || b == EXCL)

    itemAfterStartMarker :: Bool
    itemAfterStartMarker =
      isListItem i
        && onStartMarkerLine
        && skipBackWhites e i == markerStart + markerLength

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

    -- The index is at the column of a list item or a key on a line above, so
    -- the content is the mistake, not the indentation. A line of a block
    -- scalar above is neither.
    alignedWithEntry :: Bool
    alignedWithEntry = case entryAbove (columnOf i) (lineStartAt e i) of
      Just k -> isListItem k || any isKeyColon [k .. lineContentEnd e k - 1]
      Nothing -> False

    lineStart :: Int
    lineStart = lineStartAt e i

    -- The content of the line of the index after the white space and the
    -- indicators of block entries, as in "- ? key". A plain scalar can follow
    -- a tab there, but a key cannot.
    contentStart :: Int
    contentStart = go lineStart
      where
        go :: Int -> Int
        go j =
          let k = skipWhites e j
          in if isBlockIndicator k then go (k + 1) else k

    -- The first tab in the indentation before the index, if only white space
    -- and the indicators of block entries precede the index on its line.
    indentationTab :: Int -> Maybe Int -> Maybe Int
    indentationTab j tab
      | j < e.base = tab'
      | isBlockIndicator j = indentationTab (j - 1) tab
      | otherwise = case A.unsafeIndex e.array j of
          SPACE -> indentationTab (j - 1) tab
          TAB -> indentationTab (j - 1) (Just j)
          w
            | isBreak w -> tab'
            | otherwise -> Nothing
      where
        tab' :: Maybe Int
        tab' = if byteAt e i == TAB then Just (fromMaybe i tab) else tab

    isBlockIndicator :: Int -> Bool
    isBlockIndicator j =
      let w = byteAt e j
      in (w == MINUS || w == QUESTION || w == COLON) && isWhite (byteAt e (j + 1))

    -- The error for a line of a block collection that lacks the space after
    -- "-" or the ":" after a key, if the entries above it at the same
    -- position are list items or mapping entries.
    blockMistake :: Maybe (Int, String)
    blockMistake = do
      guard $ start < stop
      k <- entryAbove column (lineStartAt e stop)
      if
        | isListItem k && byteAt e start == MINUS && stop == start + 1 ->
            Just (stop, "expected a space after '-'")
        | not (isListItem k)
            && (w == 0 || isBreak w || stop < i)
            && not (any isKeyColon [afterKey .. stop - 1]) ->
            Just $ case (keyEnd, filter tightColon [afterKey .. stop - 1]) of
              (Nothing, _) -> (start, "unterminated " ++ quotedName ++ " scalar")
              (Just end, _) | end > stop -> (stop, "a key must be on a single line")
              (_, colon : _) -> (colon + 1, "expected a space after ':'")
              (_, []) -> (stop, "expected ':' after the key")
        | otherwise -> Nothing
      where
        -- The parser fails at a comment after the content, as in "key # note".
        stop :: Int
        stop
          | byteAt e i == HASH && isWhite (byteBefore e i) = skipBackWhites e i
          | otherwise = i

        -- The index after the quoted scalar that starts the line, or the start
        -- of the line without a quote, or 'Nothing' if the scalar does not
        -- end. A colon inside the scalar does not end a key.
        keyEnd :: Maybe Int
        keyEnd
          | quote == DQUOTE || quote == SQUOTE = closing (start + 1)
          | otherwise = Just start
          where
            closing :: Int -> Maybe Int
            closing j
              | j >= e.end = Nothing
              | quote == SQUOTE && b == SQUOTE && byteAt e (j + 1) == SQUOTE =
                  closing (j + 2)
              | quote == DQUOTE && b == BACKSLASH = closing (j + 2)
              | b == quote = Just (j + 1)
              | otherwise = closing (j + 1)
              where
                b :: Word8
                b = byteAt e j

        -- The index after the key on the line.
        afterKey :: Int
        afterKey = fromMaybe stop keyEnd

        quote :: Word8
        quote = byteAt e start

        quotedName :: String
        quotedName = if quote == DQUOTE then "double-quoted" else "single-quoted"

        w :: Word8
        w = byteAt e stop

        start :: Int
        start = skipSpaces e (lineStartAt e stop)

        column :: Int
        column = start - lineStartAt e stop

        -- A colon before a word, as in "key:value", but not in "http://".
        tightColon :: Int -> Bool
        tightColon j = byteAt e j == COLON && startsWord (byteAt e (j + 1))

        startsWord :: Word8 -> Bool
        startsWord b =
          (isAsciiByte b && isAlphaNum (chr (fromIntegral b)))
            || b == SQUOTE
            || b == DQUOTE
            || b == LBRACKET
            || b == LBRACE

    -- The closest entry above the line that starts at the index, at the
    -- column. An entry can follow "- " on its line, as in "- key: value".
    entryAbove :: Int -> Int -> Maybe Int
    entryAbove column from = do
      k <- lineAbove from
      let indent = columnOf k
          entry = skipListItems k
      if
        | indent == column -> Just k
        | columnOf entry == column -> Just entry
        | indent < column -> Nothing
        | otherwise -> entryAbove column (lineStartAt e k)

    -- The error for content at the index that starts a line with a wrong
    -- indentation, if the lines above show the likely mistake: a list item
    -- among mapping entries or the other way round, or a line of a block
    -- scalar with too little indentation.
    indentationMistake :: Maybe String
    indentationMistake = go (lineStartAt e i)
      where
        column :: Int
        column = columnOf i

        -- Look at the lines above, up to the first line with less
        -- indentation.
        go :: Int -> Maybe String
        go start = do
          k <- lineAbove start
          let indent = columnOf k
          if
            | indent > column -> go (lineStartAt e k)
            | indent < column ->
                if endsWithBlockHeader k
                  then
                    Just
                      "unexpected indentation, the line has less indentation than the block scalar above it"
                  else Nothing
            | isListItem k && not (isListItem i) && not (isFlowIndicator (byteAt e i)) ->
                Just "unexpected key among list items"
            | not (isListItem k) && isListItem i ->
                Just "unexpected list item among mapping entries"
            | otherwise -> Nothing

    -- The line from the content at the index ends with the header of a block
    -- scalar, e.g. "key: |-".
    endsWithBlockHeader :: Int -> Bool
    endsWithBlockHeader k =
      let h = skipIndicators (lineContentEnd e k)
      in h > k
           && (let b = byteAt e (h - 1) in b == PIPE || b == GREATER)
           && (h - 1 == k || isWhite (byteAt e (h - 2)))
      where
        skipIndicators :: Int -> Int
        skipIndicators j =
          let b = byteBefore e j
          in if b == MINUS || b == PLUS || isDecDigit b then skipIndicators (j - 1) else j

    -- The first content of the closest line above the line that starts at the
    -- index. Blank lines and comment lines do not count.
    lineAbove :: Int -> Maybe Int
    lineAbove start = skipSpaces e . skipBoms e <$> contentLineAbove e start

    -- A byte order mark can start the first line of a document, before its
    -- indentation.
    columnOf :: Int -> Int
    columnOf j = j - skipBoms e (lineStartAt e j)

    -- The index after the "- " indicators at the index, as in "- - key: value".
    skipListItems :: Int -> Int
    skipListItems j = if isListItem j then skipListItems (skipWhites e (j + 1)) else j

    -- A colon that ends an implicit key is at the index.
    isKeyColon :: Int -> Bool
    isKeyColon j =
      byteAt e j == COLON
        && (let b = byteAt e (j + 1) in b == 0 || isWhite b || isBreak b)

    -- A block sequence entry starts at the index.
    isListItem :: Int -> Bool
    isListItem j =
      byteAt e j == MINUS
        && (let b = byteAt e (j + 1) in b == 0 || isWhite b || isBreak b)

-- | The ':' that ends an alias name before the index, as in "*x: 1". An
-- alias name can contain ':'.
aliasColon :: Env -> Int -> Maybe Int
aliasColon e i =
  let j = skipBackWhites e i
      start = wordStart e j
  in if j > start && byteBefore e j == COLON && byteAt e start == STAR
       then Just (j - 1)
       else Nothing

aliasColonMessage :: String
aliasColonMessage =
  "the name of the alias includes the ':', write a space before ':' if the alias is a key"

-- | The location and the message of the error at the index inside a flow
-- collection: a common mistake if the input there shows one, or else the
-- index and the given message.
flowError :: Env -> Int -> String -> (Int, String)
flowError e i msg = case aliasColon e i of
  Just colon -> (colon, aliasColonMessage)
  Nothing -> (i, fromMaybe msg (mistakeIn (afterBoms e) True i))

-- | The input without the byte order marks at its start. The hints look at
-- the content of the lines around an error, and the marks are not content
-- of the first line. The indices stay those of the input.
afterBoms :: Env -> Env
afterBoms e = e {base = skipBoms e e.base}

-- | The flag tells if the index is inside a flow collection.
mistakeIn :: Env -> Bool -> Int -> Maybe String
mistakeIn e flow i
  | isBom e i = Just "unexpected byte order mark"
  -- Inside a plain scalar, a '#' after other content does not stop the
  -- parser, so here it follows the end of another node, e.g. "x"#c.
  | w == HASH && isNsChar (byteBefore e i) =
      Just "unexpected '#', a comment needs a space before it"
  | w == COMMA
      && ( let b = byteBefore e (skipBack i)
           in b == COMMA || b == LBRACKET || b == LBRACE
         ) =
      Just "unexpected ',', a flow collection cannot have an empty entry"
  -- In the block style, these characters start a block scalar.
  | flow && (w == PIPE || w == GREATER) =
      Just $ unexpectedChar e i ++ ", a block scalar cannot be inside a flow collection"
  | w == STAR && not (isAnchorChar (byteAt e (i + 1))) =
      Just "expected an alias name after '*'"
  | w == STAR
      && (let b = byteAt e (wordStart e (skipBackWhites e i)) in b == AMP || b == EXCL) =
      Just "unexpected '*', an alias cannot have an anchor or a tag"
  | w == AMP && not (isAnchorChar (byteAt e (i + 1))) =
      Just "expected an anchor name after '&'"
  | afterQuote SQUOTE =
      Just $
        unexpectedChar e i
          ++ " after a single-quoted scalar, write '' for a quote inside it"
  | afterQuote DQUOTE =
      Just $
        unexpectedChar e i
          ++ " after a double-quoted scalar, write \\\" for a quote inside it"
  -- A '%' at the start of a line in the block style starts a directive.
  | not flow && w == PERCENT && isStartOfLine e i && isNsChar (byteAt e (i + 1)) =
      Just
        "unexpected '%', a directive needs '...' on a line above it to end the document"
  -- Other indicators start a node of another kind, e.g. '&' an anchor.
  | w == AT || w == GRAVE || w == PERCENT =
      Just $
        unexpectedChar e i ++ ", a plain scalar cannot start with it, quote the value"
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
      byteBefore e i == q
        && isNsChar w
        && not (isFlowIndicator w)
        && w /= COLON
        && not quoteInTag

    -- A quote can be a character of a tag, as in "!'".
    quoteInTag :: Bool
    quoteInTag = byteBefore e (tagStart i) == EXCL
      where
        tagStart :: Int -> Int
        tagStart j = if isTagChar (byteBefore e j) then tagStart (j - 1) else j

-- | The index after the content of the line from the content at the index,
-- before its comment.
lineContentEnd :: Env -> Int -> Int
lineContentEnd e = skipBackWhites e . go
  where
    go :: Int -> Int
    go j
      | b == 0 || isBreak b = j
      | b == HASH && isWhite (byteBefore e j) = j
      | otherwise = go (j + 1)
      where
        b :: Word8
        b = byteAt e j

-- | The index after the last content before the white space that ends at the
-- index.
skipBackWhites :: Env -> Int -> Int
skipBackWhites e i = if isWhite (byteBefore e i) then skipBackWhites e (i - 1) else i

-- | The start of the word that ends at the index, e.g. of an anchor or an
-- alias with its indicator.
wordStart :: Env -> Int -> Int
wordStart e i = if isAnchorChar (byteBefore e i) then wordStart e (i - 1) else i

unexpectedChar :: Env -> Int -> String
unexpectedChar e i
  | isAsciiByte w = "unexpected " ++ show (chr (fromIntegral w))
  | isPrint c = "unexpected '" ++ [c] ++ "'"
  | otherwise = "unexpected " ++ codePointName c
  where
    w :: Word8
    w = byteAt e i

    c :: Char
    c = T.head (slice e i e.end)

-- | The first tab from the first index to before the second.
firstTab :: Env -> Int -> Int -> Maybe Int
firstTab e i j = L.find (\k -> byteAt e k == TAB) [i .. j - 1]

tabMessage :: String
tabMessage = "tabs cannot be used for indentation"

keyLengthMessage :: String
keyLengthMessage =
  "a key can be at most "
    ++ show maxImplicitKeyLength
    ++ " characters long, write a longer key after '? '"

-- | The code point of a character, e.g. U+0007, for a character that an error
-- cannot show.
codePointName :: Char -> String
codePointName c =
  let hex = map toUpper (showHex (ord c) "")
  in "U+" ++ replicate (4 - length hex) '0' ++ hex
