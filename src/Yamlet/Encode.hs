-- | Conversion of Haskell values to nodes.
module Yamlet.Encode
  ( -- * Class
    ToYaml (..)
  , (.=)
  , mapping
  ) where

import Control.Applicative
import Data.Fixed
import Data.Foldable
import Data.Functor.Identity
import Data.Int
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid qualified as Mon
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Scientific qualified as Sci
import Data.Semigroup qualified as Sem
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as B
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Data.Time.ToText
import Data.Tree qualified as Tree
import Data.UUID.Types qualified as UUID
import Data.Void
import Data.Word
import GHC.Generics
import GHC.TypeLits hiding (Natural)
import Math.NumberTheory.Logarithms
import Numeric.Natural

import Yamlet.Internal.Generic
import Yamlet.Internal.Schema
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Internal.Utils
import Yamlet.Internal.View
import Yamlet.Syntax qualified as S
import Yamlet.Value

----------------------------------------
-- Class

-- | Types that can be converted to a node. A type with a 'Generic' instance
-- can derive the instance, see "Yamlet.Generic".
class ToYaml a where
  toYaml :: a -> S.Node
  default toYaml
    :: ( Generic a
       , GenericYaml a
       , Rep a ~ D1 d f
       , GConstructors f
       , GEncoding (SumEncoding a) f
       , GToConstructor f
       )
    => a -> S.Node
  toYaml = genericToYaml

  -- | Convert a list. The instance for 'Char' creates a string instead.
  toYamlList :: [a] -> S.Node
  toYamlList = S.sequenceNode . map toYaml

  -- | Convert the value of a mapping entry, with the node of its key, e.g. to
  -- put comments on the key as 'Yamlet.Commented' does. '.=', the derived
  -- encoders and the instances for maps use it. The default returns the key
  -- unchanged.
  toYamlField :: S.Node -> a -> (S.Node, S.Node)
  toYamlField k v = (k, toYaml v)

-- | An entry of a mapping with a string key.
(.=) :: ToYaml a => T.Text -> a -> (S.Node, S.Node)
key .= v = toYamlField (string key) v

infixr 8 .=

-- | A mapping with the entries in the given order. A mapping with two equal
-- keys does not read back.
mapping :: [(S.Node, S.Node)] -> S.Node
mapping = S.mappingNode

instance ToYaml S.Node where toYaml = id
instance ToYaml Value where toYaml = toSyntax

-- | The value with the comments of its entry. The lines above and the comment
-- of the first line go on the key, where the renderer writes them at the same
-- places as on a value. Without a key, they go on the value. The lines after
-- the value replace its own.
instance ToYaml a => ToYaml (S.Commented a) where
  toYaml c =
    let v = withLinesAfter c.comments.after (toYaml c.value)
        vc = v.comments
    in S.Node
         v.offset
         v.endOffset
         v.props
         vc {S.before = c.comments.before ++ vc.before, S.inline = c.comments.inline <|> vc.inline}
         v.content
  toYamlField k c = (key, withLinesAfter c.comments.after (toYaml c.value))
    where
      key :: S.Node
      key = S.Node k.offset k.endOffset k.props (S.Comments c.comments.before c.comments.inline k.comments.after) k.content

-- | The value alone. The key of an entry goes to the value inside, e.g. for a
-- 'Yamlet.Commented' value.
instance ToYaml a => ToYaml (S.Located a) where
  toYaml l = toYaml l.value
  toYamlField k l = toYamlField k l.value

-- | The node with the given lines after it in place of its own, if the lines
-- are not empty.
withLinesAfter :: [S.Line] -> S.Node -> S.Node
withLinesAfter ls v
  | null ls = v
  | otherwise = S.Node v.offset v.endOffset v.props (v.comments {S.after = ls}) v.content

-- | An empty list, as a tuple without elements.
instance ToYaml () where toYaml _ = S.sequenceNode []

instance ToYaml Bool where toYaml = scalar . Bool
instance ToYaml Integer where toYaml = scalar . Int
instance ToYaml Natural where toYaml = scalar . Int . toInteger
instance ToYaml Int where toYaml = scalar . Int . toInteger
instance ToYaml Int8 where toYaml = scalar . Int . toInteger
instance ToYaml Int16 where toYaml = scalar . Int . toInteger
instance ToYaml Int32 where toYaml = scalar . Int . toInteger
instance ToYaml Int64 where toYaml = scalar . Int . toInteger
instance ToYaml Word where toYaml = scalar . Int . toInteger
instance ToYaml Word8 where toYaml = scalar . Int . toInteger
instance ToYaml Word16 where toYaml = scalar . Int . toInteger
instance ToYaml Word32 where toYaml = scalar . Int . toInteger
instance ToYaml Word64 where toYaml = scalar . Int . toInteger
instance ToYaml Double where toYaml = scalar . Float . doubleToFloatValue
instance ToYaml Float where toYaml = scalar . Float . floatToFloatValue

-- | A value whose exponent in scientific notation is beyond the range from
-- -1000 to 1000, e.g. @1e1001@, does not read back, see 'Finite'.
instance ToYaml Sci.Scientific where toYaml = scalar . Float . Finite

instance ToYaml Day where toYaml = timestamp buildDay
instance ToYaml TimeOfDay where toYaml = iso8601 buildTimeOfDay
instance ToYaml LocalTime where toYaml = timestamp buildLocalTime
instance ToYaml ZonedTime where toYaml = timestamp buildZonedTime
instance ToYaml UTCTime where toYaml = timestamp buildUTCTime

-- | A number of seconds.
instance ToYaml NominalDiffTime where
  toYaml d = let MkFixed ps = nominalDiffTimeToSeconds d in scalar (Float (Finite (Sci.scientific ps (negate picoDecimals))))

-- | A number of seconds.
instance ToYaml DiffTime where
  toYaml d = scalar (Float (Finite (Sci.scientific (diffTimeToPicoseconds d) (negate picoDecimals))))

-- | The text form with hyphens, e.g. @123e4567-e89b-12d3-a456-426614174000@.
instance ToYaml UUID.UUID where toYaml = scalar . String . UUID.toText

instance ToYaml Month where toYaml = iso8601 buildMonth
instance ToYaml Quarter where toYaml = iso8601 buildQuarter
instance ToYaml QuarterOfYear where toYaml = iso8601 buildQuarterOfYear

-- | A string in an ISO 8601 format, the same as in aeson.
iso8601 :: (a -> TLB.Builder) -> a -> S.Node
iso8601 build = scalar . String . TL.toStrict . TLB.toLazyText . build

-- | Like 'iso8601', but plain even though YAML 1.1 reads the text as a
-- timestamp, because the value is one.
timestamp :: (a -> TLB.Builder) -> a -> S.Node
timestamp build x
  | isPlainString t = S.plainNode t
  | otherwise = string t
  where
    t :: T.Text
    t = TL.toStrict (TLB.toLazyText (build x))

-- | The English name in lowercase, e.g. @monday@.
instance ToYaml DayOfWeek where
  toYaml = scalar . String . T.toLower . T.pack . show

-- | A mapping with the keys @months@ and @days@, e.g. @{months: 1, days: 2}@.
instance ToYaml CalendarDiffDays where
  toYaml d = mapping ["months" .= cdMonths d, "days" .= cdDays d]

-- | A mapping with the keys @months@ and @time@, a number of seconds, e.g.
-- @{months: 1, time: 1.5}@.
instance ToYaml CalendarDiffTime where
  toYaml d = mapping ["months" .= ctMonths d, "time" .= ctTime d]

instance ToYaml T.Text where toYaml = scalar . String
instance ToYaml TL.Text where toYaml = scalar . String . TL.toStrict

instance ToYaml Char where
  toYaml = scalar . String . T.singleton
  toYamlList = scalar . String . T.pack

instance ToYaml a => ToYaml [a] where
  toYaml = toYamlList

instance ToYaml a => ToYaml (NE.NonEmpty a) where
  toYaml = toYaml . NE.toList

-- | 'Nothing' is null. The key of an entry goes to the value inside, e.g. for
-- a 'Yamlet.Commented' value.
instance ToYaml a => ToYaml (Maybe a) where
  toYaml = maybe (scalar Null) toYaml
  toYamlField k = maybe (k, scalar Null) (toYamlField k)

-- | Two keys that give the same node, e.g. 'Nothing' and @'Just' ()@, or two
-- NaN values, give a mapping that does not read back.
instance (ToYaml k, ToYaml v) => ToYaml (M.Map k v) where
  toYaml m = mapping [toYamlField (toYaml k) v | (k, v) <- M.toList m]

instance ToYaml v => ToYaml (IM.IntMap v) where
  toYaml m = mapping [toYamlField (toYaml k) v | (k, v) <- IM.toList m]

-- | A list in ascending order.
instance ToYaml a => ToYaml (Set.Set a) where
  toYaml = toYaml . Set.toAscList

-- | A list in ascending order.
instance ToYaml IS.IntSet where
  toYaml = toYaml . IS.toAscList

instance ToYaml a => ToYaml (Seq.Seq a) where
  toYaml = toYaml . toList

-- | A list of the label and the subtrees, e.g. @[a, [[b, []]]]@.
instance ToYaml a => ToYaml (Tree.Tree a) where
  toYaml t = toYaml (Tree.rootLabel t, Tree.subForest t)

-- | @LT@, @EQ@ or @GT@.
instance ToYaml Ordering where
  toYaml = scalar . String . T.pack . show

-- | Null.
instance ToYaml (Proxy a) where
  toYaml _ = scalar Null

instance ToYaml Void where
  toYaml = absurd

-- | A mapping with the keys @numerator@ and @denominator@, e.g.
-- @{numerator: 1, denominator: 3}@.
instance (Integral a, ToYaml a) => ToYaml (Ratio a) where
  toYaml r = mapping ["numerator" .= numerator r, "denominator" .= denominator r]

-- | A number. If the resolution is not a product of 2s and 5s, e.g. 3, a value
-- can have no exact decimal form. It then becomes the nearest number with as
-- many digits after the point as the resolution has, which does not read back.
-- For such a resolution, use 'Rational' instead.
instance HasResolution a => ToYaml (Fixed a) where
  toYaml (MkFixed n) = scalar . Float . Finite $ case decimalPlaces res of
    Just places -> Sci.scientific (n * (10 ^ places `div` res)) (negate places)
    Nothing -> Sci.scientific (round (n * 10 ^ digits % res)) (negate digits)
    where
      res :: Integer
      res = resolution (Proxy @a)

      digits :: Int
      digits = integerLog10 res + 1

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Identity a)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Const a b)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Down a)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Sem.Min a)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Sem.Max a)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Sem.First a)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Sem.Last a)

-- | The value inside, or null for 'Nothing'.
deriving newtype instance ToYaml a => ToYaml (Mon.First a)

-- | The value inside, or null for 'Nothing'.
deriving newtype instance ToYaml a => ToYaml (Mon.Last a)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Sem.Dual a)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Sem.Sum a)

-- | The value inside.
deriving newtype instance ToYaml a => ToYaml (Sem.Product a)

-- | The value inside.
deriving newtype instance ToYaml Sem.All

-- | The value inside.
deriving newtype instance ToYaml Sem.Any

-- | A mapping with one key, @Left@ or @Right@, e.g. @{Left: 1}@.
instance (ToYaml a, ToYaml b) => ToYaml (Either a b) where
  toYaml = \case
    Left a -> mapping ["Left" .= a]
    Right b -> mapping ["Right" .= b]

instance (ToYaml a1, ToYaml a2) => ToYaml (a1, a2) where
  toYaml (a1, a2) =
    S.sequenceNode
      [ toYaml a1
      , toYaml a2
      ]

instance (ToYaml a1, ToYaml a2, ToYaml a3) => ToYaml (a1, a2, a3) where
  toYaml (a1, a2, a3) =
    S.sequenceNode
      [ toYaml a1
      , toYaml a2
      , toYaml a3
      ]

instance (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4) => ToYaml (a1, a2, a3, a4) where
  toYaml (a1, a2, a3, a4) =
    S.sequenceNode
      [ toYaml a1
      , toYaml a2
      , toYaml a3
      , toYaml a4
      ]

instance (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4, ToYaml a5) => ToYaml (a1, a2, a3, a4, a5) where
  toYaml (a1, a2, a3, a4, a5) =
    S.sequenceNode
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
    S.sequenceNode
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
    S.sequenceNode
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
    S.sequenceNode
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
    S.sequenceNode
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
    S.sequenceNode
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
-- Generic

-- The default method has no INLINE pragma, because GHC copies the pragma of a
-- default method to each derived method. Then each use of a derived instance,
-- e.g. in a list, gets a copy of the whole encoder. The default method calls
-- this function without the argument, so the function does not inline in the
-- library. GHC inlines both in the derived method, and it must do so before
-- the specializer runs. Otherwise the specializer makes a copy of the code for
-- each node of the representation, and a large type takes several times
-- longer to compile. For the same reason, the top of the representation goes
-- to a plain function, not to a class with one method. GHC represents the
-- dictionary of such a class as a partial application of the method, and it
-- does not inline that.
genericToYaml
  :: forall a d f
   . ( Generic a
     , GenericYaml a
     , Rep a ~ D1 d f
     , GConstructors f
     , GEncoding (SumEncoding a) f
     , GToConstructor f
     )
  => a -> S.Node
genericToYaml x =
  -- Forcing the encoding forces the check of the shape, e.g. with deferred
  -- type errors in a test of the errors.
  let enc = gEncoding @(SumEncoding a) @f
  in enc `seq` gToYaml (yamlOptions @a) enc (from <$> yamlDefault @a) (from x)
{-# INLINE genericToYaml #-}

-- The encoder takes the default for 'omitNullFields': it leaves out a null
-- field only if the default of the field is null too. Otherwise the decoder
-- would fill the missing key from the default, and the value would not read
-- back.
gToYaml
  :: forall f d p
   . ( GConstructors f
     , GToConstructor f
     )
  => YamlOptions -> SumEncodingKind -> Maybe (D1 d f p) -> D1 d f p -> S.Node
gToYaml opts enc def (M1 x)
  | gNullary @f = scalar (String (gTag opts x))
  | otherwise = gToConstructor opts (if isTagged @f opts then Just enc else Nothing) (unM1 <$> def) x
-- Without the pragma, GHC 9.2 does not inline this function, and GHC 9.4 does
-- not inline it for an enumeration. Then the inspection tests of these
-- derived encoders fail. Later versions inline it anyway.
{-# INLINE gToYaml #-}

class GToConstructor f where
  gTag :: YamlOptions -> f p -> T.Text

  -- | The constructor, with the tag in the given encoding.
  gToConstructor :: YamlOptions -> Maybe SumEncodingKind -> Maybe (f p) -> f p -> S.Node

instance GToConstructor V1 where
  gTag _ = \case {}
  gToConstructor _ _ _ = \case {}

instance (GToConstructor f, GToConstructor g) => GToConstructor (f :+: g) where
  gTag opts = \case
    L1 x -> gTag opts x
    R1 x -> gTag opts x
  gToConstructor opts flat def = \case
    L1 x -> gToConstructor opts flat (def >>= \case L1 d -> Just d; R1 _ -> Nothing) x
    R1 x -> gToConstructor opts flat (def >>= \case R1 d -> Just d; L1 _ -> Nothing) x
  {-# INLINE gTag #-}
  {-# INLINE gToConstructor #-}

instance
  ( KnownSymbol name
  , GFields f
  , GToFields f
  )
  => GToConstructor (C1 (MetaCons name fixity isRecord) f)
  where
  gTag opts _ = constructorTag opts (symbolVal (Proxy @name))
  gToConstructor opts tagging def c@(M1 x) = case tagging of
    Just SingleField
      | gNamed @f -> mapping [(string (gTag opts c), mapping (gToEntries opts (unM1 <$> def) x))]
      | otherwise -> case gToValue x of
          Nothing -> string (gTag opts c)
          Just _ -> mapping [gToEntry (string (gTag opts c)) x]
    Just enc
      | gNamed @f -> mapping (withTagEntry (gToEntries opts (unM1 <$> def) x))
      | otherwise -> case gToValue x of
          Nothing -> mapping (withTagEntry [])
          Just v
            | enc == TaggedFlat, Just entries <- flatEntries opts v -> mapping (withTagEntry entries)
            | otherwise -> mapping (withTagEntry [gToEntry (string opts.contentsKey) x])
    Nothing
      | gNamed @f -> mapping (gToEntries opts (unM1 <$> def) x)
      | otherwise -> fromMaybe (mapping []) (gToValue x)
    where
      withTagEntry :: [(S.Node, S.Node)] -> [(S.Node, S.Node)]
      withTagEntry entries = (opts.tagKey .= gTag opts c) : entries
  {-# INLINE gTag #-}
  {-# INLINE gToConstructor #-}

-- | The entries of a field next to the tag, if the decoder can read them
-- back. The field must be a mapping with a key, and no key can be the tag
-- key. The key cannot be the contents key alone, because the decoder reads
-- such a mapping as the other form.
flatEntries :: YamlOptions -> S.Node -> Maybe [(S.Node, S.Node)]
flatEntries opts v = case v.content of
  S.Mapping _ kvs -> case kvs of
    [] -> Nothing
    [(k, _)] | isKey opts.contentsKey k -> Nothing
    _
      | any (isKey opts.tagKey . fst) kvs -> Nothing
      | otherwise -> Just kvs
  _ -> Nothing
  where
    isKey :: T.Text -> S.Node -> Bool
    isKey key k = case stringValue k of
      Just t -> t == key
      _ -> False

-- The shape check allows named fields, no fields, or one field without a
-- name. The default methods are for the kind of fields that never calls
-- them.
class GToFields f where
  -- | The entries of the named fields, with the given default.
  gToEntries :: YamlOptions -> Maybe (f p) -> f p -> [(S.Node, S.Node)]
  gToEntries _ _ _ = []

  -- | The value of the only field without a name.
  gToValue :: f p -> Maybe S.Node
  gToValue _ = Nothing

  -- | The mapping entry of the only field without a name under the key, e.g.
  -- with the comments of a 'Yamlet.Commented' field on the contents key.
  gToEntry :: S.Node -> f p -> (S.Node, S.Node)
  gToEntry k x = (k, fromMaybe (mapping []) (gToValue x))

instance GToFields U1

instance (GToFields f, GToFields g) => GToFields (f :*: g) where
  gToEntries opts def (a :*: b) =
    gToEntries opts ((\(d :*: _) -> d) <$> def) a ++ gToEntries opts ((\(_ :*: d) -> d) <$> def) b
  {-# INLINE gToEntries #-}

instance
  ( KnownSymbol name
  , ToYaml a
  )
  => GToFields (S1 (MetaSel (Just name) u s d) (Rec0 a))
  where
  gToEntries opts def (M1 (K1 x))
    | opts.omitNullFields && isNullNode (snd entry) && nullDefault = []
    | otherwise = [entry]
    where
      entry :: (S.Node, S.Node)
      entry = fieldKey @name opts .= x

      -- The decoder fills a missing key from the default.
      nullDefault :: Bool
      nullDefault = case def of
        Just (M1 (K1 d)) -> isNullNode (toYaml d)
        Nothing -> True
  {-# INLINE gToEntries #-}

instance ToYaml a => GToFields (S1 (MetaSel Nothing u s d) (Rec0 a)) where
  gToValue (M1 (K1 x)) = Just (toYaml x)
  gToEntry k (M1 (K1 x)) = toYamlField k x
  {-# INLINE gToValue #-}

----------------------------------------
-- Nodes

-- | The node of a value.
toSyntax :: Value -> S.Node
toSyntax = \case
  Sequence xs -> S.sequenceNode (map toSyntax xs)
  Mapping kvs -> S.mappingNode [(toSyntax k, toSyntax v) | (k, v) <- kvs]
  Tagged tag v -> (toSyntax v) {S.props = S.Props Nothing (S.Tag tag)}
  v -> scalar v

-- | A scalar in a style that reads back as the value. A collection is empty.
scalar :: Value -> S.Node
scalar = \case
  String t -> string t
  v -> S.plainNode (plainText v)

-- | A string as a literal block scalar if it has a line break. Otherwise it
-- is a plain scalar if the schema reads the text as a string, and in single
-- quotes if not. The renderer puts a plain scalar in quotes if its text
-- cannot be plain, e.g. @a: b@. It uses double quotes for a text with a tab
-- or a character that single quotes cannot hold.
--
-- A text that YAML 1.1 reads as another type, e.g. @yes@ or @12:30@, is in
-- quotes too, because many parsers still follow YAML 1.1.
string :: T.Text -> S.Node
string t
  | T.any (== '\n') t = S.scalarNode S.Literal t
  | isPlainString t && not (isYaml11NonString t) = S.plainNode t
  | otherwise = S.scalarNode S.SingleQuoted t

-- | The text of a value without quotes, or an empty collection in the flow
-- style.
plainText :: Value -> T.Text
plainText = \case
  Null -> "null"
  Bool b -> if b then "true" else "false"
  Int i -> T.pack (show i)
  Float (Finite s) -> finite s
  Float Infinity -> ".inf"
  Float NegativeZero -> "-0.0"
  Float NegativeInfinity -> "-.inf"
  Float NaN -> ".nan"
  String t -> t
  Sequence _ -> "[]"
  Mapping _ -> "{}"
  Tagged _ v -> plainText v
  where
    -- Decimal notation for the exponents from 'minDecimal' to 'maxDecimal',
    -- and exponential notation for other numbers. The text always has a dot,
    -- so the number reads back as a float, not as an integer.
    -- Sci.formatScientific takes quadratic time in the number of digits, and
    -- its exponent overflows close to the upper limit of Int.
    finite :: Sci.Scientific -> T.Text
    finite s = case T.uncons digits of
      Nothing -> "0.0"
      Just (d, rest)
        | ex >= minDecimal && ex < 0 ->
            T.concat [sign, "0.", T.replicate (fromInteger (negate ex - 1)) "0", digits]
        | ex >= 0 && ex <= maxDecimal ->
            let (int, frac) = T.splitAt integerDigits digits
            in T.concat [sign, T.justifyLeft integerDigits '0' int, ".", orZero frac]
        -- YAML 1.1 reads an exponent without a sign as a string.
        | otherwise -> T.concat [sign, T.singleton d, ".", orZero rest, if ex < 0 then "e" else "e+", decimal ex]
      where
        c :: Integer
        c = Sci.coefficient s

        written :: T.Text
        written = decimal (abs c)

        digits :: T.Text
        digits = T.dropWhileEnd (== '0') written

        -- The exponent of the first digit.
        ex :: Integer
        ex = toInteger (Sci.base10Exponent s) + toInteger (T.length written) - 1

        integerDigits :: Int
        integerDigits = fromInteger ex + 1

        sign :: T.Text
        sign = if c < 0 then "-" else ""

        orZero :: T.Text -> T.Text
        orZero t = if T.null t then "0" else t

        decimal :: Integer -> T.Text
        decimal = B.runBuilder . B.fromUnboundedDec

        -- The exponents of the first digit that Number::toString of
        -- ECMAScript writes in decimal notation, for the numbers from 10^-6
        -- up to 10^21. JSON.stringify uses the same notation.
        minDecimal, maxDecimal :: Integer
        minDecimal = -6
        maxDecimal = 20
