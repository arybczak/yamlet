{-# OPTIONS_HADDOCK not-home #-}

-- | The class t'ToYaml', its instances and the parts of the encoder that the
-- generic instances share with it. "Yamlet.Encode" exports the public parts.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.ToYaml
  ( -- * Class
    ToYaml (..)
  , (.=)
  , mapping

    -- * Parts of the generic instances
  , string
  , scalar
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
import Math.NumberTheory.Logarithms
import Numeric.Natural

import Yamlet.Internal.Schema
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Internal.Utils
import Yamlet.Value

----------------------------------------
-- Class

-- | Types that can be converted to a node. A type with a
-- t'GHC.Generics.Generic' instance can derive the instance via
-- t'Yamlet.Generic.GenericYaml'.
--
-- An instance for a record writes a mapping with 'mapping' and '.=':
--
-- >>> :{
-- data Server = Server {host :: T.Text, port :: Int, tags :: [T.Text]}
-- instance ToYaml Server where
--   toYaml s = mapping ["host" .= s.host, "port" .= s.port, "tags" .= s.tags]
-- :}
--
-- >>> T.putStr (encodeText (Server "example.com" 80 ["web", "yes"]))
-- host: example.com
-- port: 80
-- tags:
-- - web
-- - 'yes'
--
-- The string @yes@ gets quotes, because YAML 1.1 parsers read it as a
-- boolean.
class ToYaml a where
  toYaml :: a -> S.Node

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
-- of the first line go on the key, or on the value without a key. The lines
-- after the value replace its own.
instance ToYaml a => ToYaml (S.Commented a) where
  toYaml c =
    let v = withLinesAfter c.comments.after (toYaml c.value)
        vc = v.comments
    in S.withComments
         vc
           { S.before = c.comments.before ++ vc.before
           , S.inline = c.comments.inline <|> vc.inline
           }
         v
  toYamlField k c = (key, withLinesAfter c.comments.after (toYaml c.value))
    where
      key :: S.Node
      key =
        S.withComments
          ( S.Comments
              { S.before = c.comments.before
              , S.inline = c.comments.inline
              , S.after = k.comments.after
              }
          )
          k

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
  | otherwise = S.withComments v.comments {S.after = ls} v

-- | An empty list, as a tuple without elements.
instance ToYaml () where toYaml _ = S.sequenceNode []

instance ToYaml Bool where toYaml = scalar . Bool
instance ToYaml Integer where toYaml = scalar . Int
instance ToYaml Natural where toYaml = integral
instance ToYaml Int where toYaml = integral
instance ToYaml Int8 where toYaml = integral
instance ToYaml Int16 where toYaml = integral
instance ToYaml Int32 where toYaml = integral
instance ToYaml Int64 where toYaml = integral
instance ToYaml Word where toYaml = integral
instance ToYaml Word8 where toYaml = integral
instance ToYaml Word16 where toYaml = integral
instance ToYaml Word32 where toYaml = integral
instance ToYaml Word64 where toYaml = integral

integral :: Integral a => a -> S.Node
integral = scalar . Int . toInteger

-- | Decimal notation from 10^-6 up to 10^21, as JavaScript writes numbers,
-- and exponential notation otherwise. The text always has a dot, so that it
-- reads back as a float. A value that is not a number or is infinite is
-- @.nan@, @.inf@ or @-.inf@.
--
-- >>> T.putStr (encodeText [12, 0.01, 1.5e-7, 2.0e21, 0 / 0, -1 / 0 :: Double])
-- - 12.0
-- - 0.01
-- - 1.5e-7
-- - 2.0e+21
-- - .nan
-- - -.inf
instance ToYaml Double where toYaml = scalar . Float . realFloatToFloatValue

instance ToYaml Float where toYaml = scalar . Float . realFloatToFloatValue

-- | A value whose exponent in scientific notation is beyond the range from
-- -1000 to 1000, e.g. @1e1001@, does not read back, see 'Finite'.
instance ToYaml Sci.Scientific where toYaml = scalar . Float . Finite

-- | The decoder accepts a year of at most 15 digits, so a larger year, e.g.
-- @10^15@, does not read back.
instance ToYaml Day where toYaml = timestamp buildDay

instance ToYaml TimeOfDay where toYaml = iso8601 timeOfDay

-- | The decoder accepts a year of at most 15 digits, so a larger year does
-- not read back.
instance ToYaml LocalTime where toYaml = timestamp localTime

-- | The decoder accepts an offset of less than 24 hours, so a larger offset,
-- e.g. @+25:00@, does not read back. Neither does a year of more than 15
-- digits.
instance ToYaml ZonedTime where
  toYaml = timestamp (\(ZonedTime t z) -> localTime t <> buildTimeZone z)

-- | The decoder accepts a year of at most 15 digits, so a larger year does
-- not read back.
instance ToYaml UTCTime where
  toYaml =
    timestamp (\(UTCTime d s) -> localTime (LocalTime d (timeToTimeOfDay s)) <> "Z")

-- | The time of day without the trailing zeros of the fraction, e.g.
-- @12:30:15.5@, as in aeson. text-iso8601 writes the fraction in groups of
-- three digits.
timeOfDay :: TimeOfDay -> TLB.Builder
timeOfDay (TimeOfDay h m (MkFixed ps)) =
  buildTimeOfDay (TimeOfDay h m (MkFixed (ps - frac))) <> fraction
  where
    frac :: Integer
    frac = ps `rem` (10 ^ picoDecimals)

    fraction :: TLB.Builder
    fraction
      | frac == 0 = mempty
      | otherwise =
          "."
            <> TLB.fromText
              ( T.dropWhileEnd
                  (== '0')
                  (T.justifyRight picoDecimals '0' (T.pack (show frac)))
              )

localTime :: LocalTime -> TLB.Builder
localTime (LocalTime d t) = buildDay d <> "T" <> timeOfDay t

-- | A number of seconds. A value whose exponent in scientific notation is
-- beyond the range from -1000 to 1000, e.g. @10^1001@ seconds, does not read
-- back, see 'Finite'.
instance ToYaml NominalDiffTime where
  toYaml d = let MkFixed ps = nominalDiffTimeToSeconds d in seconds ps

-- | A number of seconds. A value whose exponent in scientific notation is
-- beyond the range from -1000 to 1000, e.g. @10^1001@ seconds, does not read
-- back, see 'Finite'.
instance ToYaml DiffTime where
  toYaml = seconds . diffTimeToPicoseconds

-- | The seconds of a number of picoseconds.
seconds :: Integer -> S.Node
seconds ps = scalar (Float (Finite (Sci.scientific ps (negate picoDecimals))))

-- | The text form with hyphens, e.g. @123e4567-e89b-12d3-a456-426614174000@.
instance ToYaml UUID.UUID where toYaml = scalar . String . UUID.toText

-- | The decoder accepts a year of at most 15 digits, so a larger year does
-- not read back.
instance ToYaml Month where toYaml = iso8601 buildMonth

-- | The decoder accepts a year of at most 15 digits, so a larger year does
-- not read back.
instance ToYaml Quarter where toYaml = iso8601 buildQuarter

instance ToYaml QuarterOfYear where toYaml = iso8601 buildQuarterOfYear

-- | A string in an ISO 8601 format, the same as in aeson.
iso8601 :: (a -> TLB.Builder) -> a -> S.Node
iso8601 build = scalar . String . TL.toStrict . TLB.toLazyText . build

-- | Like 'iso8601', but plain if YAML 1.1 reads the text as a timestamp,
-- because the value is one.
timestamp :: (a -> TLB.Builder) -> a -> S.Node
timestamp build x
  | isPlainString t && (isYaml11Timestamp t || not (isYaml11NonString t)) = S.plainNode t
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

-- | A string of the character, and a t'String' as a string. A surrogate code
-- point, which t'Data.Text.Text' cannot hold, becomes U+FFFD, so it does not
-- read back.
instance ToYaml Char where
  toYaml = scalar . String . T.singleton
  toYamlList = scalar . String . T.pack

instance ToYaml a => ToYaml [a] where
  toYaml = toYamlList

instance ToYaml a => ToYaml (NE.NonEmpty a) where
  toYaml = items . NE.toList

-- | 'Nothing' is null. @'Just' 'Nothing'@ is null too, so it reads back as
-- 'Nothing', as in aeson. The key of an entry goes to the value inside, e.g.
-- for a 'Yamlet.Commented' value.
instance ToYaml a => ToYaml (Maybe a) where
  toYaml = maybe (scalar Null) toYaml
  toYamlField k = maybe (k, scalar Null) (toYamlField k)

-- | Two keys that give equal nodes give a mapping that does not read back,
-- e.g. 'Nothing' and @'Just' 'Nothing'@, two NaN values, or two v'Mapping'
-- values with the same entries in a different order.
instance (ToYaml k, ToYaml v) => ToYaml (M.Map k v) where
  toYaml m = mapping [toYamlField (toYaml k) v | (k, v) <- M.toList m]

instance ToYaml v => ToYaml (IM.IntMap v) where
  toYaml m = mapping [toYamlField (toYaml k) v | (k, v) <- IM.toList m]

-- | A list in ascending order.
instance ToYaml a => ToYaml (Set.Set a) where
  toYaml = items . Set.toAscList

-- | A list in ascending order.
instance ToYaml IS.IntSet where
  toYaml = toYaml . IS.toAscList

instance ToYaml a => ToYaml (Seq.Seq a) where
  toYaml = items . toList

-- | A sequence of the items. Unlike a list, it is never a string, e.g. for
-- items of type 'Char', because only t'String' is text.
items :: ToYaml a => [a] -> S.Node
items = S.sequenceNode . map toYaml

-- | A list of the label and the subtrees, e.g. @[a, [[b, []]]]@.
instance ToYaml a => ToYaml (Tree.Tree a) where
  toYaml t = toYaml (Tree.rootLabel t, Tree.subForest t)

-- | @LT@, @EQ@ or @GT@.
instance ToYaml Ordering where
  toYaml = scalar . String . T.pack . show

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
--
-- A value whose exponent in scientific notation is beyond the range from
-- -1000 to 1000, e.g. @10^1001@, does not read back, see 'Finite'.
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

instance
  (ToYaml a1, ToYaml a2, ToYaml a3, ToYaml a4, ToYaml a5)
  => ToYaml (a1, a2, a3, a4, a5)
  where
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
  ( ToYaml a1
  , ToYaml a2
  , ToYaml a3
  , ToYaml a4
  , ToYaml a5
  , ToYaml a6
  , ToYaml a7
  , ToYaml a8
  , ToYaml a9
  )
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
-- Nodes

-- | The node of a value.
toSyntax :: Value -> S.Node
toSyntax = \case
  Sequence xs -> S.sequenceNode (map toSyntax xs)
  Mapping kvs -> S.mappingNode [(toSyntax k, toSyntax v) | (k, v) <- kvs]
  Tagged tag v
    | T.compareLength tag 1 == GT -> (toSyntax v) {S.props = S.Props Nothing (S.Tag tag)}
    -- YAML has no syntax for such a tag. The non-specific tag ! would make
    -- the value a string in YAML 1.2, but not in YAML 1.1 parsers.
    | otherwise -> toSyntax v
  v -> scalar v

-- | A scalar in a style that reads back as the value. For a collection or a
-- tagged value, use 'toSyntax'.
scalar :: Value -> S.Node
scalar = \case
  String t -> string t
  v -> S.plainNode (plainText v)

-- | A string as a literal block scalar if it has a line break. A text of
-- only line breaks is in double quotes, because quotes are easier to read
-- than empty lines. Otherwise it is a plain scalar if the schema reads the
-- text as a string, and in single quotes if not. The renderer puts a plain
-- scalar in quotes if its text cannot be plain, e.g. @a: b@. It uses double
-- quotes for a text with a tab or a character that single quotes cannot
-- hold.
--
-- A text that YAML 1.1 reads as another type, e.g. @yes@ or @12:30@, is in
-- quotes too, because many parsers still follow YAML 1.1.
string :: T.Text -> S.Node
string t
  | T.all (== '\n') t, not (T.null t) = S.scalarNode S.DoubleQuoted t
  | T.any (== '\n') t = S.scalarNode S.Literal t
  | isPlainString t && not (isYaml11NonString t) = S.plainNode t
  | otherwise = S.scalarNode S.SingleQuoted t

-- | The text of a scalar value without quotes.
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
    -- The exponent of Sci.formatScientific overflows close to the upper limit
    -- of Int.
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
        | otherwise ->
            T.concat
              [ sign
              , T.singleton d
              , "."
              , orZero rest
              , if ex < 0 then "e" else "e+"
              , decimal ex
              ]
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

-- $setup
-- >>> import Data.Text.IO qualified as T
-- >>> import Yamlet
