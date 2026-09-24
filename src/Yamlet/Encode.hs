-- | Conversion of Haskell values to nodes and rendering of nodes as YAML.
module Yamlet.Encode
  ( -- * Class
    ToYAML(..)
  , (.=)
  , mapping

    -- * Rendering
  , renderDocuments
  ) where

import Data.Int
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Word
import Numeric.Natural

import Yamlet.Internal.Emit
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
instance ToYAML Double where toYAML = node . Float . doubleToFloat
instance ToYAML Float where toYAML = node . Float . doubleToFloat . realToFrac
instance ToYAML Sci.Scientific where toYAML = node . Float . Finite
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
  | otherwise = Just (tagText n.tag)

-- | A scalar on one line.
scalarText :: Node -> B.Builder
scalarText n = case n.value of
  Null -> "null"
  Bool b -> if b then "true" else "false"
  Int i -> B.fromString (show i)
  -- The generic format always has a dot or an exponent, so the number reads
  -- back as a float, not as an integer.
  Float (Finite s) -> B.fromString (Sci.formatScientific Sci.Generic Nothing s)
  Float Infinity -> ".inf"
  Float NegativeInfinity -> "-.inf"
  Float NaN -> ".nan"
  String t
    | isPlainSafe t -> B.fromText t
    | otherwise -> doubleQuoted t
  Sequence _ -> "[]"
  Mapping _ -> "{}"

-- | A literal block scalar for a string with line breaks.
literal :: Int -> T.Text -> Maybe B.Builder
literal indent t
  | T.any (== '\n') t = uncurry (<>) <$> literalBlock True indent t
  | otherwise = Nothing
