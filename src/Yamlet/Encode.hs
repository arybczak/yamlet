-- | Conversion of Haskell values to nodes and rendering of nodes as YAML.
module Yamlet.Encode
  ( -- * Class
    ToYAML(..)
  , (.=)
  , mapping

    -- * Rendering
  , renderDocuments
  , isPlainSafe
  ) where

import Data.Char
import Data.Int
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Word
import Numeric
import Numeric.Natural

import Yamlet.Schema
import Yamlet.Node

----------------------------------------
-- Class

-- | Types that can be converted to a node.
class ToYAML a where
  toYAML :: a -> Node

  -- | Convert a list. The instance for 'Char' creates a string instead.
  toYAMLList :: [a] -> Node
  toYAMLList = node . Sequence . map toYAML

-- | An entry of a mapping with a string key.
(.=) :: ToYAML a => T.Text -> a -> (Node, Node)
key .= v = (node (String key), toYAML v)

infixr 8 .=

-- | A mapping with the entries in the given order.
mapping :: [(Node, Node)] -> Node
mapping = node . Mapping

instance ToYAML Node where toYAML = id
instance ToYAML () where toYAML _ = node Null
instance ToYAML Bool where toYAML = node . Bool
instance ToYAML Integer where toYAML = node . Int
instance ToYAML Natural where toYAML = node . Int . toInteger
instance ToYAML Int where toYAML = node . Int . toInteger
instance ToYAML Int8 where toYAML = node . Int . toInteger
instance ToYAML Int16 where toYAML = node . Int . toInteger
instance ToYAML Int32 where toYAML = node . Int . toInteger
instance ToYAML Int64 where toYAML = node . Int . toInteger
instance ToYAML Word where toYAML = node . Int . toInteger
instance ToYAML Word8 where toYAML = node . Int . toInteger
instance ToYAML Word16 where toYAML = node . Int . toInteger
instance ToYAML Word32 where toYAML = node . Int . toInteger
instance ToYAML Word64 where toYAML = node . Int . toInteger
instance ToYAML Double where toYAML = node . Float
instance ToYAML Float where toYAML = node . Float . realToFrac
instance ToYAML T.Text where toYAML = node . String
instance ToYAML TL.Text where toYAML = node . String . TL.toStrict

instance ToYAML Char where
  toYAML = node . String . T.singleton
  toYAMLList = node . String . T.pack

instance ToYAML a => ToYAML [a] where
  toYAML = toYAMLList

instance ToYAML a => ToYAML (NE.NonEmpty a) where
  toYAML = toYAML . NE.toList

-- | 'Nothing' is null.
instance ToYAML a => ToYAML (Maybe a) where
  toYAML = maybe (node Null) toYAML

instance (ToYAML k, ToYAML v) => ToYAML (M.Map k v) where
  toYAML m = mapping [ (toYAML k, toYAML v) | (k, v) <- M.toList m ]

instance (ToYAML a, ToYAML b) => ToYAML (a, b) where
  toYAML (a, b) = node $ Sequence [toYAML a, toYAML b]

instance (ToYAML a, ToYAML b, ToYAML c) => ToYAML (a, b, c) where
  toYAML (a, b, c) = node $ Sequence [toYAML a, toYAML b, toYAML c]

----------------------------------------
-- Rendering

-- | Render documents in the block style. Documents after the first one start
-- with a @---@ marker.
renderDocuments :: [Node] -> T.Text
renderDocuments docs = TL.toStrict . B.toLazyText . mconcat $ zipWith document [0 :: Int ..] docs
  where
    document :: Int -> Node -> B.Builder
    document i n = (if i > 0 then "---\n" else mempty) <> topLevel n

    topLevel :: Node -> B.Builder
    topLevel n = case n.value of
      Sequence (_ : _) -> tagLine n <> blockSequence 0 True n
      Mapping (_ : _) -> tagLine n <> blockMapping 0 True n
      _ -> scalarValue 2 n <> "\n"

    -- A tag of a block collection takes a line of its own.
    tagLine :: Node -> B.Builder
    tagLine n = case tagPrefix n of
      Just t -> t <> "\n"
      Nothing -> mempty

-- | A block sequence. The first entry does not start with indentation if the
-- sequence continues a line.
blockSequence :: Int -> Bool -> Node -> B.Builder
blockSequence indent atLineStart n = case n.value of
  Sequence xs -> mconcat $ zipWith entry [0 :: Int ..] xs
  _ -> mempty
  where
    entry :: Int -> Node -> B.Builder
    entry i x = (if i > 0 || atLineStart then spaces indent else mempty) <> "-" <> item x

    item :: Node -> B.Builder
    item x = case x.value of
      Sequence (_ : _) -> collection x $ blockSequence (indent + 2) False x
      Mapping (_ : _) -> collection x $ blockMapping (indent + 2) False x
      _ -> " " <> scalarValue (indent + 2) x <> "\n"

    collection :: Node -> B.Builder -> B.Builder
    collection x body = case tagPrefix x of
      Just t -> " " <> t <> "\n" <> reindent body
      Nothing -> " " <> body
      where
        reindent :: B.Builder -> B.Builder
        reindent b = spaces (indent + 2) <> b

-- | A block mapping. The first entry does not start with indentation if the
-- mapping continues a line.
blockMapping :: Int -> Bool -> Node -> B.Builder
blockMapping indent atLineStart n = case n.value of
  Mapping kvs -> mconcat $ zipWith entry [0 :: Int ..] kvs
  _ -> mempty
  where
    entry :: Int -> (Node, Node) -> B.Builder
    entry i (k, v) = (if i > 0 || atLineStart then spaces indent else mempty) <> case implicitKey k of
      Just key -> key <> ":" <> value v
      Nothing -> "?" <> explicit k <> spaces indent <> ":" <> explicit v

    value :: Node -> B.Builder
    value v = case v.value of
      Sequence (_ : _) -> tagged v <> "\n" <> blockSequence indent True v
      Mapping (_ : _) -> tagged v <> "\n" <> blockMapping (indent + 2) True v
      _ -> " " <> scalarValue (indent + 2) v <> "\n"

    -- The key or the value of an explicit entry.
    explicit :: Node -> B.Builder
    explicit x = case x.value of
      Sequence (_ : _) -> tagged x <> "\n" <> blockSequence (indent + 2) True x
      Mapping (_ : _) -> tagged x <> "\n" <> blockMapping (indent + 2) True x
      _ -> " " <> scalarValue (indent + 2) x <> "\n"

    tagged :: Node -> B.Builder
    tagged x = maybe mempty (" " <>) (tagPrefix x)

-- | A key that fits on one line, or 'Nothing' if it needs an explicit entry.
implicitKey :: Node -> Maybe B.Builder
implicitKey k = case k.value of
  Sequence _ -> Nothing
  Mapping _ -> Nothing
  _ | TL.length (B.toLazyText key) > 1024 -> Nothing
    | otherwise -> Just key
  where
    key :: B.Builder
    key = withTag k (scalarText k)

-- | A scalar, or an empty collection in the flow style.
scalarValue :: Int -> Node -> B.Builder
scalarValue indent n = withTag n $ case n.value of
  Sequence _ -> "[]"
  Mapping _ -> "{}"
  String t | Just b <- literal indent t -> b
  _ -> scalarText n

-- | Prefix the tag if it is not the default for the value.
withTag :: Node -> B.Builder -> B.Builder
withTag n b = case tagPrefix n of
  Just t -> t <> " " <> b
  Nothing -> b

tagPrefix :: Node -> Maybe B.Builder
tagPrefix n
  | n.tag == defaultTag n.value = Nothing
  | Just suffix <- T.stripPrefix "tag:yaml.org,2002:" n.tag
  , T.all isTagChar suffix
  = Just $ "!!" <> B.fromText suffix
  | Just suffix <- T.stripPrefix "!" n.tag
  , not (T.null suffix)
  , T.all isTagChar suffix
  = Just $ "!" <> B.fromText suffix
  | otherwise = Just $ "!<" <> B.fromText n.tag <> ">"
  where
    isTagChar :: Char -> Bool
    isTagChar c = isAscii c && (isAlphaNum c || c `elem` ("-#;/?:@&=+$_.~*'()" :: String))

-- | A scalar on one line.
scalarText :: Node -> B.Builder
scalarText n = case n.value of
  Null -> "null"
  Bool b -> if b then "true" else "false"
  Int i -> B.fromString (show i)
  Float d
    | isNaN d -> ".nan"
    | isInfinite d -> if d > 0 then ".inf" else "-.inf"
    | otherwise -> B.fromString (show d)
  String t
    | isPlainSafe t -> B.fromText t
    | otherwise -> doubleQuoted t
  Sequence _ -> "[]"
  Mapping _ -> "{}"

-- | The string reads back as the same string if it is a plain scalar in the
-- block style, as a value or as a key. In a flow collection the characters
-- @,[]{}@ need quotes too, so the check does not apply there.
isPlainSafe :: T.Text -> Bool
isPlainSafe t = case T.uncons t of
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
    && resolvePlain t == String t
  where
    firstOk :: Char -> T.Text -> Bool
    firstOk c rest
      | c `elem` ("-?:" :: String) = case T.uncons rest of
          Just (c', _) -> not (isWhite c')
          Nothing -> False
      | otherwise = not (isWhite c) && c `notElem` ("-?:,[]{}#&*!|>'\"%@`" :: String)

    isPlainChar :: Char -> Bool
    isPlainChar c = c == ' ' || (isPrintable c && c /= '\t')

    isWhite :: Char -> Bool
    isWhite c = c == ' ' || c == '\t'

-- | A literal block scalar for a string with line breaks.
literal :: Int -> T.Text -> Maybe B.Builder
literal indent t
  | not (T.any (== '\n') t) = Nothing
  | T.null body = Nothing
  | not (T.all (\c -> c == '\n' || c == '\t' || isPrintable c) t) = Nothing
  | otherwise = Just $ mconcat
    [ "|"
    , if leadingSpace then B.fromString (show indentStep) else mempty
    , case trailing of
        0 -> "-"
        1 -> mempty
        _ -> "+"
    , mconcat (map line (T.splitOn "\n" body))
    , B.fromText (T.replicate (trailing - 1) "\n")
    ]
  where
    indentStep :: Int
    indentStep = 2

    body :: T.Text
    body = T.dropWhileEnd (== '\n') t

    trailing :: Int
    trailing = T.length t - T.length body

    leadingSpace :: Bool
    leadingSpace = case T.uncons (T.dropWhile (== '\n') body) of
      Just (c, _) -> c == ' '
      Nothing -> False

    line :: T.Text -> B.Builder
    line l
      | T.null l = "\n"
      | otherwise = "\n" <> spaces indent <> B.fromText l

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
      c | isPrintable c -> B.singleton c
        | ord c <= 0xFF -> "\\x" <> hex 2 (ord c)
        | ord c <= 0xFFFF -> "\\u" <> hex 4 (ord c)
        | otherwise -> "\\U" <> hex 8 (ord c)

    hex :: Int -> Int -> B.Builder
    hex k i = let s = map toUpper (showHex i "") in B.fromString (replicate (k - length s) '0' ++ s)

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
