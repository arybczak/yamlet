-- | Conversion of Haskell values to nodes and rendering of nodes as YAML.
module Yamlet.Encode
  ( -- * Class
    ToYaml (..)
  , (.=)
  , mapping

    -- * Rendering
  , renderDocuments
  , toSyntax
  ) where

import Data.Containers.ListUtils
import Data.Fixed
import Data.Foldable
import Data.Int
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Scientific qualified as Sci
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Time
import Data.Word
import Numeric.Natural

import Yamlet.Internal.Emit
import Yamlet.Internal.Time
import Yamlet.Node
import Yamlet.Schema
import Yamlet.Syntax qualified as S

----------------------------------------
-- Class

-- | Types that can be converted to a node.
class ToYaml a where
  toYaml :: a -> Node

  -- | Convert a list. The instance for 'Char' creates a string instead.
  toYamlList :: [a] -> Node
  toYamlList = node . Sequence . map toYaml

-- | An entry of a mapping with a string key.
(.=) :: ToYaml a => T.Text -> a -> (Node, Node)
key .= v = (node (String key), toYaml v)

infixr 8 .=

-- | A mapping with the entries in the given order. The keys must be
-- different, as for 'Mapping'.
mapping :: [(Node, Node)] -> Node
mapping = node . Mapping

instance ToYaml Node where toYaml = id
instance ToYaml () where toYaml _ = node Null
instance ToYaml Bool where toYaml = node . Bool
instance ToYaml Integer where toYaml = node . Int
instance ToYaml Natural where toYaml = node . Int . toInteger
instance ToYaml Int where toYaml = node . Int . toInteger
instance ToYaml Int8 where toYaml = node . Int . toInteger
instance ToYaml Int16 where toYaml = node . Int . toInteger
instance ToYaml Int32 where toYaml = node . Int . toInteger
instance ToYaml Int64 where toYaml = node . Int . toInteger
instance ToYaml Word where toYaml = node . Int . toInteger
instance ToYaml Word8 where toYaml = node . Int . toInteger
instance ToYaml Word16 where toYaml = node . Int . toInteger
instance ToYaml Word32 where toYaml = node . Int . toInteger
instance ToYaml Word64 where toYaml = node . Int . toInteger
instance ToYaml Double where toYaml = node . Float . doubleToFloatValue
instance ToYaml Float where toYaml = node . Float . floatToFloatValue
instance ToYaml Sci.Scientific where toYaml = node . Float . Finite
instance ToYaml Day where toYaml = node . String . formatDay
instance ToYaml TimeOfDay where toYaml = node . String . formatTimeOfDay
instance ToYaml LocalTime where toYaml = node . String . formatLocalTime
instance ToYaml ZonedTime where toYaml = node . String . formatZonedTime
instance ToYaml UTCTime where toYaml = node . String . formatUTCTime

-- | A number of seconds.
instance ToYaml NominalDiffTime where
  toYaml d = let MkFixed ps = nominalDiffTimeToSeconds d in node (Float (Finite (Sci.scientific ps (-12))))

-- | A number of seconds.
instance ToYaml DiffTime where
  toYaml d = node (Float (Finite (Sci.scientific (diffTimeToPicoseconds d) (-12))))

instance ToYaml T.Text where toYaml = node . String
instance ToYaml TL.Text where toYaml = node . String . TL.toStrict

instance ToYaml Char where
  toYaml = node . String . T.singleton
  toYamlList = node . String . T.pack

instance ToYaml a => ToYaml [a] where
  toYaml = toYamlList

instance ToYaml a => ToYaml (NE.NonEmpty a) where
  toYaml = toYaml . NE.toList

-- | 'Nothing' is null.
instance ToYaml a => ToYaml (Maybe a) where
  toYaml = maybe (node Null) toYaml

-- | Two keys that give the same node, e.g. 'Nothing' and @'Just' ()@, or two
-- NaN values, give a mapping that does not read back.
instance (ToYaml k, ToYaml v) => ToYaml (M.Map k v) where
  toYaml m = mapping [(toYaml k, toYaml v) | (k, v) <- M.toList m]

instance ToYaml v => ToYaml (IM.IntMap v) where
  toYaml m = mapping [(toYaml k, toYaml v) | (k, v) <- IM.toList m]

-- | A list in ascending order.
instance ToYaml a => ToYaml (Set.Set a) where
  toYaml = toYaml . Set.toAscList

-- | A list in ascending order.
instance ToYaml IS.IntSet where
  toYaml = toYaml . IS.toAscList

instance ToYaml a => ToYaml (Seq.Seq a) where
  toYaml = toYaml . toList

-- | A mapping with one key, @Left@ or @Right@, e.g. @{Left: 1}@.
instance (ToYaml a, ToYaml b) => ToYaml (Either a b) where
  toYaml = \case
    Left a -> mapping ["Left" .= a]
    Right b -> mapping ["Right" .= b]

instance (ToYaml a1, ToYaml a2) => ToYaml (a1, a2) where
  toYaml (a1, a2) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        ]

instance (ToYaml a1, ToYaml a2, ToYaml a3) => ToYaml (a1, a2, a3) where
  toYaml (a1, a2, a3) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        , toYaml a3
        ]

instance (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4) => ToYaml (a1, a2, a3, a4) where
  toYaml (a1, a2, a3, a4) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        , toYaml a3
        , toYaml a4
        ]

instance (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4, ToYaml a5) => ToYaml (a1, a2, a3, a4, a5) where
  toYaml (a1, a2, a3, a4, a5) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        , toYaml a3
        , toYaml a4
        , toYaml a5
        ]

instance
  (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4, ToYaml a5, ToYaml a6)
  => ToYaml (a1, a2, a3, a4, a5, a6)
  where
  toYaml (a1, a2, a3, a4, a5, a6) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        , toYaml a3
        , toYaml a4
        , toYaml a5
        , toYaml a6
        ]

instance
  (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4, ToYaml a5, ToYaml a6, ToYaml a7)
  => ToYaml (a1, a2, a3, a4, a5, a6, a7)
  where
  toYaml (a1, a2, a3, a4, a5, a6, a7) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        , toYaml a3
        , toYaml a4
        , toYaml a5
        , toYaml a6
        , toYaml a7
        ]

instance
  (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4, ToYaml a5, ToYaml a6, ToYaml a7, ToYaml a8)
  => ToYaml (a1, a2, a3, a4, a5, a6, a7, a8)
  where
  toYaml (a1, a2, a3, a4, a5, a6, a7, a8) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        , toYaml a3
        , toYaml a4
        , toYaml a5
        , toYaml a6
        , toYaml a7
        , toYaml a8
        ]

instance
  (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4, ToYaml a5, ToYaml a6, ToYaml a7, ToYaml a8, ToYaml a9)
  => ToYaml (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
  toYaml (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        , toYaml a3
        , toYaml a4
        , toYaml a5
        , toYaml a6
        , toYaml a7
        , toYaml a8
        , toYaml a9
        ]

instance
  ( ToYaml a1
  , ToYaml a2
  , ToYaml a3
  , ToYaml a4
  , ToYaml a5
  , ToYaml a6
  , ToYaml a7
  , ToYaml a8
  , ToYaml a9
  , ToYaml a10
  )
  => ToYaml (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  where
  toYaml (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
    node $
      Sequence
        [ toYaml a1
        , toYaml a2
        , toYaml a3
        , toYaml a4
        , toYaml a5
        , toYaml a6
        , toYaml a7
        , toYaml a8
        , toYaml a9
        , toYaml a10
        ]

----------------------------------------
-- Rendering

-- | Render documents in the block style. Documents after the first one start
-- with a @---@ marker.
renderDocuments :: [Node] -> T.Text
renderDocuments docs = TL.toStrict . B.toLazyText . mconcat $ zipWith document [0 :: Int ..] docs
  where
    document :: Int -> Node -> B.Builder
    document i n
      | null handles = (if i > 0 then "---\n" else mempty) <> topLevel n
      | otherwise = (if i > 0 then "...\n" else mempty) <> foldMap tagDirective handles <> "---\n" <> topLevel n
      where
        -- The handles for the tags that are not valid URIs.
        handles :: [Char]
        handles = nubOrd . mapMaybe tagHandle $ tags n []

    tags :: Node -> [T.Text] -> [T.Text]
    tags n acc =
      n.tag : case n.value of
        Sequence xs -> foldr tags acc xs
        Mapping kvs -> foldr (\(k, v) -> tags k . tags v) acc kvs
        _ -> acc

    topLevel :: Node -> B.Builder
    topLevel n = case n.value of
      Sequence (_ : _) -> tagLine n <> blockSequence 0 True n
      Mapping (_ : _) -> tagLine n <> blockMapping 0 True n
      String t | needsIndentIndicator t -> withTag n (scalarText n) <> "\n"
      _ -> scalarValue 2 n <> "\n"

    -- A tag of a block collection takes a line of its own.
    tagLine :: Node -> B.Builder
    tagLine n = case tagPrefix n of
      Just t -> t <> "\n"
      Nothing -> mempty

-- | Convert a node to a node of a syntax tree, e.g. to set the styles of its
-- scalars or to add comments before 'S.renderSyntax' writes it. The styles
-- are the ones that 'renderDocuments' uses.
toSyntax :: Node -> S.Node
toSyntax n = sn {S.props = S.Props Nothing tag}
  where
    tag :: S.Tag
    tag
      | n.tag == defaultTag n.value = S.NoTag
      | otherwise = S.Tag n.tag

    sn :: S.Node
    sn = case n.value of
      Sequence xs -> S.sequenceNode (map toSyntax xs)
      Mapping kvs -> S.mappingNode [(toSyntax k, toSyntax v) | (k, v) <- kvs]
      String t
        | isPlainSafe t -> S.plainNode t
        | T.any (== '\n') t -> S.scalarNode S.Literal t
        | otherwise -> S.scalarNode S.DoubleQuoted t
      v -> S.plainNode (plainText v)

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
    entry i (k, v) =
      (if i > 0 || atLineStart then spaces indent else mempty) <> case implicitKey k of
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
  _
    | TL.length (B.toLazyText key) > 1024 -> Nothing
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
  String t | not (isPlainSafe t) -> doubleQuoted t
  v -> B.fromText (plainText v)

-- | The text of a value without quotes, or an empty collection in the flow
-- style.
plainText :: Value -> T.Text
plainText = \case
  Null -> "null"
  Bool b -> if b then "true" else "false"
  Int i -> T.pack (show i)
  -- The generic format always has a dot or an exponent, so the number reads
  -- back as a float, not as an integer.
  Float (Finite s) -> T.pack (finite s)
  Float Infinity -> ".inf"
  Float NegativeZero -> "-0.0"
  Float NegativeInfinity -> "-.inf"
  Float NaN -> ".nan"
  String t -> t
  Sequence _ -> "[]"
  Mapping _ -> "{}"
  where
    -- For an exponent close to the upper limit of Int, the exponent that
    -- formatScientific writes overflows. Such a number is beyond the limit of
    -- the decoder, so the check can use that lower limit.
    finite :: Sci.Scientific -> String
    finite s
      | Sci.base10Exponent s > 10000 =
          let ds = show (abs c)
              ex = toInteger (Sci.base10Exponent s) + toInteger (length ds) - 1
          in case dropWhileEnd (== '0') ds of
               d : rest -> concat [if c < 0 then "-" else "", [d], ".", if null rest then "0" else rest, "e", show ex]
               [] -> "0.0"
      | otherwise = Sci.formatScientific Sci.Generic Nothing s
      where
        c :: Integer
        c = Sci.coefficient s

-- | A literal block scalar for a string with line breaks.
literal :: Int -> T.Text -> Maybe B.Builder
literal indent t
  | T.any (== '\n') t = uncurry (<>) <$> literalBlock True indent t
  | otherwise = Nothing
