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
import Data.ByteString qualified as BS
import Data.Char
import Data.List qualified as L
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
      i : _ -> Left $ errorAt input (toOffset e i) "unexpected byte order mark"
      [] -> Right docs
    Right (Nothing, _, fu) -> Left $ uncurry parseError (unexpected e fu)
  where
    -- A byte order mark at the start of the line of an error is the likely
    -- cause, unless a document marker or a directive follows it.
    parseError :: Int -> String -> Error
    parseError i msg = case bomAt (lineOf i) of
      Just b
        | let j = skipBoms e b
        , not (isMarker e j || byteAt e j == PERCENT) ->
            errorAt input (toOffset e b) "unexpected byte order mark"
      _ -> errorAt input (toOffset e i) msg

    -- The byte order mark at the start of a line, other than the one at the
    -- start of the input.
    bomAt :: Int -> Maybe Int
    bomAt s
      | s == off && isBom e s = if isBom e (s + 3) then Just (s + 3) else Nothing
      | isBom e s = Just s
      | otherwise = Nothing

    lineOf :: Int -> Int
    lineOf i = if i > off && not (isBreak (byteBefore e i)) then lineOf (i - 1) else i

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

-- | Check that the input has only characters that YAML allows, and find the
-- lines that start with a document marker, and the byte order marks. A
-- document cannot contain such a line. The index of a marker after a byte
-- order mark is the index of the mark. Return the index of an invalid
-- character on error.
prescan :: Env -> Int -> Either Int ([Int], [Int])
prescan e start = go start [start | isMarker e (skipBoms e start)] []
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
                       marker = isMarker e (skipBoms e s)
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
          maybe (throwAt p "unsupported YAML version") pure $ readBoundedInt (slice e q r)

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
              in if isBreak w' then fold i j acc else go seg j acc
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
  char w
    <|> if
      | c == FlowKey -> failure
      | atLineEnd e p -> case nextContent e p of
          Just (lineStart, q)
            | Just tab <- L.find (\j -> byteAt e j == TAB) [lineStart .. q - 1] ->
                throwAt tab "tabs cannot be used for indentation"
            | byteAt e q == w ->
                throwAt q ("'" ++ [chr (fromIntegral w)] ++ "' is indented too little to end the " ++ kind)
          _ -> throwAt start ("unterminated " ++ kind)
      | dash e p ->
          throwAt p "unexpected '-', a list item cannot be inside a flow collection, quote '-' if it is a string"
      | otherwise -> throwAt p (fromMaybe msg (mistake e True p))
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

    -- A '-' that cannot start a plain scalar, e.g. "- " as in a block
    -- sequence.
    dash :: Env -> Int -> Bool
    dash e i = byteAt e i == MINUS && not (isAnchorChar (byteAt e (i + 1)))

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
      if isJsonNode k && fitsKey e p q && not (any (isBreak . byteAt e) [p .. q - 1])
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
               -- Spaces at the end of the input are an empty line, as in the
               -- test JEF9/02 of the YAML test suite.
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
