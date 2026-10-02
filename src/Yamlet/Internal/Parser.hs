{-# OPTIONS_HADDOCK not-home #-}

-- | The parser of YAML 1.2.2 streams.
--
-- The functions follow the productions of the specification and keep their
-- names, e.g. @nsFlowNode@ implements @ns-flow-node(n,c)@. A few productions
-- are fused into loops over the bytes of the input for speed.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Parser
  ( parseStream

    -- * Block scalars
  , BlockLine (..)
  , foldedText
  ) where

import Control.Monad
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

import Yamlet.Error
import Yamlet.Internal.Comments
import Yamlet.Internal.Parser.Chars
import Yamlet.Internal.Parser.Hints
import Yamlet.Internal.Parser.Monad
import Yamlet.Internal.Syntax
import Yamlet.Internal.Utils

-- | Parse all documents of a stream.
parseStream :: T.Text -> Either Error [Document]
parseStream input@(T.Text arr off len) = case prescan e start of
  Left i -> Left $ errorAt input (toOffset e i) ("invalid character " ++ codePointName (T.head (slice e i e.end)))
  Right (markers, boms) -> case runParser e start (lYamlStream markers) of
    Left (ParseError i msg) -> Left $ parseError i msg
    Right (Just docs, _, _) -> case filter (not . allowedBom docs) boms of
      (i, _) : _ -> Left $ errorAt input (toOffset e i) "unexpected byte order mark"
      [] -> Right docs
    Right (Nothing, _, fu) -> Left $ uncurry parseError (unexpected e fu)
  where
    -- A byte order mark at the start of the line of an error is the likely
    -- cause, unless a document without a marker can start on the line.
    parseError :: Int -> String -> Error
    parseError i msg
      | let s = lineStartAt e i
      , bomBeforeContent e s
      , not (inPrefix s) =
          errorAt input (toOffset e s) "unexpected byte order mark"
      | otherwise = errorAt input (toOffset e i) msg

    -- Only empty lines and comment lines are between the start of the line
    -- and the start of the stream or a @...@ marker, so the line is in the
    -- prefix of a document, which can start with a byte order mark.
    inPrefix :: Int -> Bool
    inPrefix s
      | s <= off = True
      | otherwise =
          let prev = previousLineStart e s
              j = skipBoms e prev
              b = byteAt e (skipWhites e j)
          in if isBreak b || b == HASH then inPrefix prev else isEndMarker e j

    -- A byte order mark can start a line between documents, or be a
    -- character of a quoted scalar.
    allowedBom :: [Document] -> (Int, Bool) -> Bool
    allowedBom docs (i, lineStart) = case M.lookupLE (toOffset e i) (scalarRanges docs) of
      Just (_, (end, quoted)) | toOffset e i < end -> quoted
      _ -> lineStart

    scalarRanges :: [Document] -> M.Map Offset (Offset, Bool)
    scalarRanges docs = M.fromList (foldr (\d -> ranges d.root) [] docs)
      where
        ranges :: Node -> [(Offset, (Offset, Bool))] -> [(Offset, (Offset, Bool))]
        ranges n acc = case n.content of
          ScalarContent style _ -> (n.offset, (n.endOffset, style == SingleQuoted || style == DoubleQuoted)) : acc
          SequenceContent _ xs -> foldr ranges acc xs
          MappingContent _ kvs -> foldr (\(k, v) -> ranges k . ranges v) acc kvs
          AliasContent _ -> acc

    e :: Env
    e =
      Env
        { array = arr
        , base = off
        , end = off + len
        , streamEnd = off + len
        , handles = defaultHandles
        }

    start :: Int
    start = streamStart e

-- | The index after the byte order mark at the start of the input.
streamStart :: Env -> Int
streamStart e = if isBom e e.base then e.base + bomLength else e.base

-- | Check that the input has only characters that YAML allows, and find the
-- lines that start with a document marker, and the byte order marks. A
-- document cannot contain such a line. A marker after a byte order mark does
-- not count: a quoted scalar can contain the line, and other nodes end at the
-- mark anyway. Each byte order mark comes with a flag that is true if the
-- mark is at the start of a line, as 'isStartOfLine' tells. Return the index
-- of an invalid character on error.
prescan :: Env -> Int -> Either Int ([Int], [(Int, Bool)])
prescan e start = go start start [start | isMarker e start] []
  where
    -- A byte order mark at index ls is at the start of a line.
    go :: Int -> Int -> [Int] -> [(Int, Bool)] -> Either Int ([Int], [(Int, Bool)])
    go i ls acc boms
      | i >= e.end = Right (reverse acc, reverse boms)
      | otherwise =
          let w = A.unsafeIndex e.array i
          in if
               | w >= SPACE && w < DEL -> go (i + 1) ls acc boms
               | w == LF || (w == CR && byteAt e (i + 1) /= LF) ->
                   let s = i + 1
                   in go s s (if isMarker e s then s : acc else acc) boms
               | w == CR || w == TAB -> go (i + 1) ls acc boms
               | w < SPACE || w == DEL -> Left i
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
               | w == 0xEF && isBom e i ->
                   let next = i + bomLength
                   in go next (if i == ls then next else ls) acc ((i, i == ls) : boms)
               | otherwise -> go (i + 1) ls acc boms

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

-- | The indicator of a block collection entry, which a character of a plain
-- scalar cannot follow, as in "- a" but not "-a".
blockIndicator :: Word8 -> P ()
blockIndicator w = do
  char w
  next <- peek
  guardP . not $ isNsChar next

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
defaultHandles = M.fromList [("!", "!"), ("!!", coreTagPrefix)]

-- | l-yaml-stream. The markers are the indices of the lines that start with
-- a document marker.
lYamlStream :: [Int] -> P [Document]
lYamlStream markers0 = do
  s <- pos
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
        | isEndMarker e p -> do
            lDocumentSuffix
            documents markers True prefix
        | isMarker e p -> document markers Nothing defaultHandles prefix
        | afterEnd && byteAt e p == PERCENT -> do
            (version, hs) <- directives
            q <- pos
            unless (isStartMarker e q) $
              throwAt q "expected a document start marker (---) after the directives"
            document markers version hs prefix
        | afterEnd -> bareDocument markers prefix
        | otherwise -> throwAt p "expected a document start marker (---)"

    document :: [Int] -> Maybe YamlVersion -> M.Map T.Text T.Text -> Int -> P [Document]
    document markers version hs prefix = do
      m <- pos
      advance markerLength
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
      :: [Int] -> Maybe YamlVersion -> Int -> Maybe Int -> Int -> Node -> P [Document]
    finishDocument markers version prefix marker limit root = do
      withEnd limit $ many_ lComment
      e <- env
      p <- pos
      when (p < limit && not (startsPrefix e p)) $ do
        fu <- furthest
        throwUnexpected (max fu p)
      let explicitEnd = isEndMarker e p
      when explicitEnd lDocumentSuffix
      q <- pos
      -- The first empty line after the end marker ends the lines of the
      -- document.
      let gap = gapEnd e q
      rest <- documents markers explicitEnd gap
      let !(!doc, next) =
            attachComments
              e
              (prefix == streamStart e)
              (not (null rest))
              prefix
              marker
              p
              -- The lines after the last document belong to its end, also
              -- after more end markers.
              (if null rest then e.end else gap)
              Document
                { version = version
                , explicitStart = isJust marker
                , explicitEnd = explicitEnd
                , docComments = noComments
                , root = root
                }
          !rest' = linesAbove next rest
      pure (doc : rest')

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
  if isBom e p then advance bomLength else lComment

-- | l-document-suffix, without the comment lines after it. They belong to the
-- next document.
lDocumentSuffix :: P ()
lDocumentSuffix = do
  advance markerLength
  p <- pos
  sBComment <|> throwAt p "unexpected content after the document end marker (...)"

-- | l-directive, repeated, with the version and the tag handles they define.
directives :: P (Maybe YamlVersion, M.Map T.Text T.Text)
directives = go Nothing defaultHandles Set.empty
  where
    go
      :: Maybe YamlVersion
      -> M.Map T.Text T.Text
      -> Set.Set T.Text
      -> P (Maybe YamlVersion, M.Map T.Text T.Text)
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
              when (isJust version) $
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

    yamlVersion :: Int -> P YamlVersion
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
      pure $ YamlVersion major minor
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
          maybe (throwAt p "unsupported YAML version") pure $ readVersion (slice e q r)
          where
            -- The value of the digits, or 'Nothing' beyond 'maxVersion'.
            readVersion :: T.Text -> Maybe Int
            readVersion = T.foldl' step (Just 0)

            step :: Maybe Int -> Char -> Maybe Int
            step acc c = do
              n <- acc
              let n' = n * 10 + digitToInt c
              guard (n' <= maxVersion)
              pure n'

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
      invalidEscape r
      -- The escapes of the prefix and of the suffix of a tag can form one
      -- character, so the prefix stays encoded.
      pure (handle, slice e q r)
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
      slice e p <$> pos

    secondary :: Env -> Int -> P T.Text
    secondary e p = do
      char EXCL
      slice e p <$> pos

-- | Skip ns-uri-char*.
uriChars :: Env -> Int -> Int
uriChars e i
  | isUriChar (byteAt e i) = uriChars e (i + 1)
  | isPercentEscape e i = uriChars e (i + percentEscapeLength)
  | otherwise = i

-- | Skip ns-tag-char*.
tagChars :: Env -> Int -> Int
tagChars e i
  | isTagChar (byteAt e i) = tagChars e (i + 1)
  | isPercentEscape e i = tagChars e (i + percentEscapeLength)
  | otherwise = i

percentEscapeLength :: Int
percentEscapeLength = 1 + percentDigits

isPercentEscape :: Env -> Int -> Bool
isPercentEscape e i = byteAt e i == PERCENT && all (isHexDigit' . byteAt e) [i + 1 .. i + percentDigits]

-- | Stop with an error if a @%@ without two hexadecimal digits after it is at
-- the index, after the valid characters of a tag.
invalidEscape :: Int -> P ()
invalidEscape i = do
  e <- env
  when (byteAt e i == PERCENT) $
    throwAt i "invalid escape in the tag, write '%' and two hexadecimal digits"

-- | Decode the %XX escapes of a tag, or 'Nothing' if the bytes are not valid
-- UTF-8.
percentDecode :: T.Text -> Maybe T.Text
percentDecode t
  | T.any (== '%') t = either (const Nothing) Just . T.decodeUtf8' . BS.pack $ go (T.unpack t)
  | otherwise = Just t
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
  pure $! mkNode e p (toOffset e p) props (emptyContent e)

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
      char EXCL
      char LESS
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
      invalidEscape r
      -- Only the primary handle "!" can stand alone, as the non-specific tag.
      when (r == q && handle /= "!") $
        throwAt r ("expected the rest of the tag after " ++ T.unpack handle)
      guardP (r > q)
      case M.lookup handle e.handles of
        Just prefix -> case percentDecode (prefix <> slice e q r) of
          Just t -> pure (Tag t)
          Nothing -> throwAt p "the escapes of the tag are not valid UTF-8"
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
  pure $! mkNode e p (toOffset e q) noProps (AliasContent name)

----------------------------------------
-- Flow scalars

-- | c-double-quoted(n,c)
cDoubleQuoted :: Int -> Ctx -> Props -> P Node
cDoubleQuoted = cQuoted DoubleQuoted

-- | c-single-quoted(n,c)
cSingleQuoted :: Int -> Ctx -> Props -> P Node
cSingleQuoted = cQuoted SingleQuoted

-- | A double-quoted or a single-quoted scalar.
cQuoted :: ScalarStyle -> Int -> Ctx -> Props -> P Node
cQuoted style n c props = withScan $ \e p ->
  let double :: Bool
      double = style == DoubleQuoted

      quote :: Word8
      quote = if double then DQUOTE else SQUOTE

      name :: String
      name = if double then "double-quoted" else "single-quoted"

      go :: Int -> Int -> [T.Text] -> Lines -> Scanned Content
      go seg i acc ls = case byteAt e i of
        w
          | w == quote ->
              if not double && byteAt e (i + 1) == SQUOTE
                then go (i + 2) (i + 2) ("'" : slice e seg i : acc) ls
                else Done (i + 1) $ case ls of
                  FirstLine -> ScalarLinesContent style (finish (slice e seg i : acc)) []
                  Lines ps starts _ -> severalLines style (slice e seg i : acc) ps starts
          | w == BACKSLASH && double -> backslash seg i acc ls
          | isWhite w ->
              let j = skipWhites e i
              in if isBreak (byteAt e j) then fold i j acc else go seg j acc ls
          | isBreak w -> fold i i acc
          | i >= e.end -> endOfDocument i
          | otherwise -> go seg (i + 1) acc ls
        where
          fold :: Int -> Int -> [T.Text] -> Scanned Content
          fold contentEnd brk acc'
            | isKeyCtx c = NoMatch brk
            | otherwise = case flowFold e n (breakEnd e brk) of
                Just (k, j) -> go j j [] (newLine (foldText k : slice e seg contentEnd : acc') ls)
                Nothing -> badIndent brk

      backslash :: Int -> Int -> [T.Text] -> Lines -> Scanned Content
      backslash seg i acc ls
        | isBreak (byteAt e (i + 1)) =
            if isKeyCtx c
              then NoMatch i
              else case flowFold e n (breakEnd e (i + 1)) of
                Just (k, j) -> go j j [] (newLine (T.replicate k "\n" : slice e seg i : acc) ls)
                Nothing -> badIndent (i + 1)
        | i + 1 >= e.end = endOfDocument i
        | otherwise = case escape e (i + 1) of
            Just (t, j) -> go j j (t : slice e seg i : acc) ls
            Nothing -> Failed i (badEscape i)

      -- The scalar reaches the end of the document at the index.
      endOfDocument :: Int -> Scanned Content
      endOfDocument i
        | isKeyCtx c = NoMatch i
        | Just m <- cutByMarker e quote = Failed m (markerInside e (name ++ " scalar"))
        | otherwise = unterminated

      unterminated :: Scanned Content
      unterminated = Failed p ("unterminated " ++ name ++ " scalar")

      -- A hex escape with digits fails only for a bad code point. Any other
      -- invalid escape likely comes from a Windows path or a regular
      -- expression, e.g. "C:\Users" or "\d+".
      badEscape :: Int -> String
      badEscape i
        | chr (fromIntegral (byteAt e (i + 1))) `elem` ("xuU" :: String)
        , isHexDigit (chr (fromIntegral (byteAt e (i + 2)))) =
            "invalid escape sequence"
        | otherwise = "invalid escape sequence, write \\\\ for a backslash or use single quotes"

      badIndent :: Int -> Scanned Content
      badIndent i
        | nextContent i >= e.end = endOfDocument i
        | not (hasClosingQuote e quote (nextContent i)) = unterminated
        | Just tab <- firstTab e (skipBlankLines e i) (nextContent i) = Failed tab tabMessage
        | otherwise =
            Failed
              (nextContent i)
              ("invalid indentation of a line in a " ++ name ++ " scalar")

      nextContent :: Int -> Int
      nextContent i = skipWhites e (skipBlankLines e i)
  in case go (p + 1) (p + 1) [] FirstLine of
       Done q content -> Done q (mkNode e p (toOffset e q) props content)
       NoMatch q -> NoMatch q
       Failed q msg -> Failed q msg
-- Inlining gives a loop for each style. Without it, the parse benchmark of
-- the JSON input allocates more.
{-# INLINE cQuoted #-}

-- | Skip the line break at the index and the blank lines after it.
skipBlankLines :: Env -> Int -> Int
skipBlankLines e i =
  let j = skipWhites e (breakEnd e i)
  in if isBreak (byteAt e j) then skipBlankLines e j else breakEnd e i

-- | A document marker ends the document, and the byte is in the input after
-- it, e.g. the closing quote of a scalar that the marker cuts. Return the
-- index of the marker.
cutByMarker :: Env -> Word8 -> Maybe Int
cutByMarker e w
  | e.end < e.streamEnd && any (\j -> A.unsafeIndex e.array j == w) [e.end .. e.streamEnd - 1] = Just e.end
  | otherwise = Nothing

-- | The error for the document marker that ends the document inside the
-- node.
markerInside :: Env -> String -> String
markerInside e node =
  "unexpected '"
    ++ replicate markerLength (chr (fromIntegral (A.unsafeIndex e.array e.end)))
    ++ "' in a "
    ++ node
    ++ ", indent the line"

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

-- | The lines of a scalar before its current line: the pieces of their text,
-- in reverse order, the positions where they start, in reverse order, and
-- the length of the pieces.
data Lines
  = FirstLine
  | Lines [T.Text] [Int] !Int

-- | Add the pieces of the current line, in reverse order, and start a new
-- line after them.
newLine :: [T.Text] -> Lines -> Lines
newLine acc = \case
  FirstLine -> next [] [] 0
  Lines ps ls len -> next ps ls len
  where
    next :: [T.Text] -> [Int] -> Int -> Lines
    next ps ls len =
      let len' = len + sum (map T.length acc)
      in Lines (acc ++ ps) (len' : ls) len'

-- | The scalar from the pieces of its last line, and the pieces and the
-- starts of the lines before it, all in reverse order.
severalLines :: ScalarStyle -> [T.Text] -> [T.Text] -> [Int] -> Content
severalLines style acc ps starts = ScalarLinesContent style (finish (acc ++ ps)) (reverse starts)

-- | The positions where the lines start, from the length of the first line
-- and the separators and the texts of the next lines.
lineStarts :: Int -> [T.Text] -> [Int]
lineStarts = go []
  where
    go :: [Int] -> Int -> [T.Text] -> [Int]
    go acc !len = \case
      sep : t : rest ->
        let !start = len + T.length sep
        in go (start : acc) (start + T.length t) rest
      _ -> reverse acc

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
  'x' -> codePoint xEscapeDigits
  'u' -> case hexAt (i + 1) uEscapeDigits of
    -- JSON escapes a character outside the Basic Multilingual Plane as a
    -- pair of surrogates.
    Just hi
      | isHighSurrogate hi
      , let second = i + 1 + uEscapeDigits
      , byteAt e second == BACKSLASH
      , byteAt e (second + 1) == LOWER_U
      , Just lo <- hexAt (second + 2) uEscapeDigits
      , isLowSurrogate lo ->
          fromCodePoint (fromSurrogates hi lo) (second + 2 + uEscapeDigits)
    _ -> codePoint uEscapeDigits
  'U' -> codePoint bigUEscapeDigits
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
      | isScalarValue cp = Just (T.singleton (chr cp), next)
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
        not (startsPrefix e p)
          && ( (isNsChar w0 && not (isIndicator w0))
                 || ( (w0 == QUESTION || w0 == COLON || w0 == MINUS)
                        && isPlainSafe (isFlowCtx c) (byteAt e (p + 1))
                    )
             )
  in if not firstOk
       then NoMatch p
       else
         let q = plainLine e c (p + 1)
             first = slice e p q
             node end t ls = mkNode e p (toOffset e end) props (ScalarLinesContent Plain t ls)
         in if isKeyCtx c
              then Done q (node q first [])
              else case plainNextLines e n c q of
                ([], _) -> Done q (node q first [])
                (ts, r) -> Done r (node r (T.concat (first : ts)) (lineStarts (T.length first) ts))

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
           && not (startsPrefix e t)
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
  pure $! mkNode e p (toOffset e q) props (SequenceContent Flow entries)
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
  pure $! mkNode e p (toOffset e q) props (MappingContent Flow entries)
  where
    c' :: Ctx
    c' = inFlow c

    -- After a key with no value, the most likely mistake is a missing colon,
    -- e.g. in {"a" 1}.
    expected :: [(Node, Node)] -> String
    expected entries = case reverse entries of
      (k, v) : _
        | v.content == ScalarContent Plain T.empty
        , v.props == noProps
        , v.offset == k.endOffset ->
            "expected ':', ',' or '}'"
      _ -> "expected ',' or '}'"

-- | ns-s-flow-seq-entries(n,c) and ns-s-flow-map-entries(n,c).
flowEntries :: forall a. Int -> Ctx -> P a -> P [a]
flowEntries n c entry = go []
  where
    -- Each choice ends before the next entry, so that the stack does not
    -- grow with the number of entries.
    go :: [a] -> P [a]
    go acc =
      optional entry >>= \case
        Nothing -> pure $! reverse acc
        Just x -> do
          optional_ $ sSeparate n c
          more <- (True <$ (char COMMA >> optional_ (sSeparate n c))) <|> pure False
          if more then go (x : acc) else pure $! reverse (x : acc)

-- | The closing bracket of a flow collection that starts at the index. Its
-- absence is an error unless the collection is an implicit key, which the
-- parser can try again as a value. If the collection stops at the end of a
-- line, the error points to its start, which can be far away.
closing :: Ctx -> Int -> Word8 -> String -> String -> P ()
closing c start w kind msg = do
  e <- env
  p <- pos
  char w
    <|> if
      | c == FlowKey -> failure
      | atLineEnd e p -> case nextContent e p of
          Just (lineStart, q)
            | bomBeforeContent e lineStart -> throwAt lineStart "unexpected byte order mark"
            | Just tab <- firstTab e lineStart q -> throwAt tab tabMessage
            | byteAt e q == w ->
                throwAt q ("'" ++ [chr (fromIntegral w)] ++ "' is indented too little to end the " ++ kind)
            | closedLater e q ->
                throwAt q ("the line is indented too little to continue the " ++ kind)
          Nothing | Just m <- cutByMarker e w -> throwAt m (markerInside e kind)
          _ -> throwAt start ("unterminated " ++ kind)
      | dash e p ->
          throwAt p "unexpected '-', a list item cannot be inside a flow collection, quote '-' if it is a string"
      | otherwise -> throwAt p (fromMaybe msg (mistake e True p))
  where
    -- The separation after an entry goes on to the next line if the
    -- collection can continue there. If it stops at the end of a line, the
    -- document ends or the next line is indented too little.
    atLineEnd :: Env -> Int -> Bool
    atLineEnd e i
      | i >= e.end = True
      | otherwise = case byteAt e i of
          HASH -> let b = byteAt e (i - 1) in isWhite b || isBreak b
          b | isWhite b -> atLineEnd e (i + 1)
          b -> isBreak b

    -- The start of the next line with content after the line of the index,
    -- and the index of the content, unless a document marker or the end of
    -- the input comes first. A closing bracket or a tab there shows that the
    -- line is indented too little. Other content can be the next key after a
    -- missing bracket.
    nextContent :: Env -> Int -> Maybe (Int, Int)
    nextContent e i
      | i >= e.end = Nothing
      | not (isBreak (byteAt e i)) = nextContent e (i + 1)
      | otherwise =
          let s = breakEnd e i
              q = skipWhites e s
              b = byteAt e q
          in if
               | q >= e.end || isMarker e s -> Nothing
               | isBreak b || b == HASH -> nextContent e q
               | otherwise -> Just (s, q)

    -- A closing bracket without an opening bracket of its own follows in the
    -- document, so the collection likely continues there.
    closedLater :: Env -> Int -> Bool
    closedLater e = go (0 :: Int)
      where
        go :: Int -> Int -> Bool
        go depth i
          | i >= e.end = False
          | b == w = depth == 0 || go (depth - 1) (i + 1)
          | b == opening = go (depth + 1) (i + 1)
          | otherwise = go depth (i + 1)
          where
            b :: Word8
            b = byteAt e i

        opening :: Word8
        opening = if w == RBRACKET then LBRACKET else LBRACE

    -- A '-' that cannot start a plain scalar, e.g. "- " as in a block
    -- sequence.
    dash :: Env -> Int -> Bool
    dash e i = byteAt e i == MINUS && not (isAnchorChar (byteAt e (i + 1)))

-- | ns-flow-seq-entry(n,c)
--
-- The grammar reads a JSON-like node first as the key of a pair and then
-- again as a node. For nested flow sequences, this takes exponential time.
-- The parser reads the node only once, as a node. The node becomes a key if
-- it is on one line, it is not too long for an implicit key, and a colon
-- follows. Otherwise it stays a node, and the parser does not read it again.
nsFlowSeqEntry :: Int -> Ctx -> P Node
nsFlowSeqEntry n c = do
  e <- env
  p <- pos
  (pair e p <$!> nsFlowPair n c) <|> nodeEntry e p
  where
    pair :: Env -> Int -> (Node, Node) -> Node
    pair e p (k, v) = mkNode e p v.endOffset noProps (MappingContent Flow [(k, v)])

    nodeEntry :: Env -> Int -> P Node
    nodeEntry e p = do
      k <- nsFlowNode n c
      q <- pos
      let value = do
            optional_ sSeparateInLine
            r <- pos
            guardP $ fitsKey e p r
            cNsFlowMapAdjacentValue n c
      if isJsonNode k && fitsKey e p q && not (any (isBreak . byteAt e) [p .. q - 1])
        then (pair e p . (k,) <$!> value) <|> pure k
        else pure k

    -- The content of c-flow-json-node(n,c).
    isJsonNode :: Node -> Bool
    isJsonNode k = case k.content of
      SequenceContent Flow _ -> True
      MappingContent Flow _ -> True
      ScalarContent SingleQuoted _ -> True
      ScalarContent DoubleQuoted _ -> True
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
  nsFlowMapImplicitEntry n c <|> do
    k <- eNode
    v <- eNode
    pure (k, v)

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

-- | An implicit key with the separation after it. Both together are at most
-- 'maxImplicitKeyLength' characters long.
implicitKey :: P Node -> P Node
implicitKey key = do
  e <- env
  p <- pos
  k <- key
  optional_ sSeparateInLine
  q <- pos
  guardP $ fitsKey e p q
  pure k

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
    -- At the top level, n is -1. A literal reading of the specification then
    -- gives |1 no indentation, but libyaml and other parsers count from 0.
    Just m -> pure $ max 0 n + m
    Nothing -> case detectIndent e n q of
      Right m -> pure m
      Left i ->
        throwAt
          i
          "a leading empty line of a block scalar has more spaces than the first non-empty line"
  let (lines_, trailing, r) = blockLines e indent q
      (text, starts) = case indicator of
        PIPE -> (literalText lines_, [])
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
  pure $! mkNode e p (toOffset e contentEnd) props (ScalarLinesContent style value starts)

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
  sBComment
    <|> if
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
      PLUS -> Just Keep
      _ -> Nothing

    indentOf :: Word8 -> Maybe Int
    indentOf w
      | w >= DIGIT_1 && w <= DIGIT_9 = Just (fromIntegral (w - DIGIT_0))
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
               -- Spaces at the end of the input are an empty line, as in the
               -- test JEF9/02 of the YAML test suite.
               | s >= e.end -> (reverse acc, empties + 1, s)
               -- Only a block scalar at the top level has content at the
               -- start of a line.
               | s - i == indent
               , not (startsPrefix e s) ->
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

-- | The text of a folded block scalar and the positions where its lines
-- start.
foldedText :: [BlockLine] -> (T.Text, [Int])
foldedText = \case
  [] -> (T.empty, [])
  BlockLine k t : rest ->
    let next = go (isSpaced t) rest
    in (T.concat (T.replicate k "\n" : t : next), lineStarts (k + T.length t) next)
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
  k <- countSpaces
  guardP (k > n)
  advance k
  nsLCompactSequence k props

-- | c-l-block-seq-entry(n)
cLBlockSeqEntry :: Int -> P Node
cLBlockSeqEntry n = do
  blockIndicator MINUS
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
        then nsLCompactSequence (n + 1 + m) noProps <|> nsLCompactMapping (n + 1 + m) noProps
        else nsLCompactSequence (n + 1 + m) noProps

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

-- | ns-l-compact-sequence(n), with the properties of the node.
nsLCompactSequence :: Int -> Props -> P Node
nsLCompactSequence n props = do
  e <- env
  p <- pos
  x <- cLBlockSeqEntry n
  xs <- many $ sIndent n >> cLBlockSeqEntry n
  pure $! mkNode e p (lastOf x xs).endOffset props (SequenceContent Block (x : xs))

-- | l+block-mapping(n)
lBlockMapping :: Int -> Props -> P Node
lBlockMapping n props = do
  k <- countSpaces
  guardP (k > n)
  advance k
  nsLCompactMapping k props

-- | ns-l-block-map-entry(n)
nsLBlockMapEntry :: Int -> P (Node, Node)
nsLBlockMapEntry n = cLBlockMapExplicitEntry n <|> nsLBlockMapImplicitEntry n

-- | c-l-block-map-explicit-entry(n)
cLBlockMapExplicitEntry :: Int -> P (Node, Node)
cLBlockMapExplicitEntry n = do
  blockIndicator QUESTION
  k <- sLBlockIndented n BlockOut
  e <- env
  v <- lBlockMapExplicitValue <|> (pure $! missingValue e k)
  pure (k, v)
  where
    -- The key took the comments and the empty lines below it, so the position
    -- of the parser is after them. A value there would take them.
    missingValue :: Env -> Node -> Node
    missingValue e k = Node k.endOffset k.endOffset noProps noComments (emptyContent e)

    lBlockMapExplicitValue :: P Node
    lBlockMapExplicitValue = do
      sIndent n
      blockIndicator COLON
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
  blockIndicator COLON
  sLBlockNode n BlockOut <|> (eNode <* sLComments)

-- | ns-l-compact-mapping(n), with the properties of the node.
nsLCompactMapping :: Int -> Props -> P Node
nsLCompactMapping n props = do
  e <- env
  p <- pos
  x <- nsLBlockMapEntry n
  xs <- many $ sIndent n >> nsLBlockMapEntry n
  pure $! mkNode e p (snd (lastOf x xs)).endOffset props (MappingContent Block (x : xs))

-- | The last element of a non-empty list.
lastOf :: a -> [a] -> a
lastOf x = \case
  [] -> x
  y : ys -> lastOf y ys

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

-- | The content of an empty node.
emptyContent :: Env -> Content
-- With a constant that contains 'T.empty', the parser returns a node with
-- this content as a thunk, e.g. the value of "a:" above another key. GHC
-- sees a constructor, takes the node for a value and drops the '$!' that
-- builds it, but the node must wait for the evaluation of 'T.empty'. A
-- NOINLINE pragma on the constant prevents this on GHC 9.10, but not on GHC
-- 9.14. The heap check of the render tests finds this thunk.
emptyContent e = ScalarContent Plain (slice e e.base e.base)

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
