-- | Conversion of nodes to Haskell values, with errors that point to the
-- node that caused them.
module Yamlet.Decode
  ( -- * Class
    FromYaml (..)

    -- * Parser
  , Parser
  , runParser
  , parseNode
  , failAt
  , typeMismatch
  , orElse

    -- * Views
  , View (..)
  , view
  , describeNode

    -- * Scalars
  , withNull
  , withBool
  , withInt
  , withFloat
  , withScientific
  , withBoundedScientific
  , withText

    -- * Collections
  , withSequence
  , withMapping
  , Object
  , objectNode
  , objectEntries
  , objectKeys
  , lookupKey
  , (.:)
  , (.:?)
  , (.:!)
  , (.!=)
  , rejectUnknownKeys
  ) where

import Control.Monad
import Data.Char
import Data.Fixed
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid qualified as Mon
import Data.Ord
import Data.Proxy
import Data.Scientific qualified as Sci
import Data.Semigroup qualified as Sem
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Data.Time.FromText
import Data.Tree qualified as Tree
import Data.UUID.Types qualified as UUID
import Data.Version
import Data.Void
import Data.Word
import GHC.Generics
import GHC.Real
import GHC.TypeLits hiding (Natural)
import Math.NumberTheory.Logarithms
import Numeric.Natural

import Yamlet.Internal.Compose
import Yamlet.Internal.Generic
import Yamlet.Internal.Schema
import Yamlet.Internal.Syntax qualified as S
import Yamlet.Internal.Utils
import Yamlet.Node

-- | A parser of nodes. Its errors point to the node that the parser works on,
-- unless 'failAt' names another one.
--
-- The parser has no 'Control.Applicative.Alternative' instance. To try
-- another parser after a failure, use 'orElse'. To reject a value, fail with
-- a message that says why:
--
-- @
-- port <- parseYaml n
-- unless (port > 0 && port < 65536) $ fail "the port must be from 1 to 65535"
-- @
newtype Parser a = Parser (Offset -> Either (Offset, String) a)

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap f . g

instance Applicative Parser where
  pure a = Parser $ \_ -> Right a
  (<*>) = ap

instance Monad Parser where
  Parser g >>= k = Parser $ \off -> case g off of
    Right a -> let Parser h = k a in h off
    Left err -> Left err

instance MonadFail Parser where
  fail msg = Parser $ \off -> Left (off, msg)

-- | Run a parser on a node. Return the offset of the node that caused an
-- error with the error message.
--
-- The node first goes through the checks of 'Yamlet.resolveDocument', e.g.
-- for duplicate keys, and its aliases are replaced with the nodes that they
-- refer to.
runParser :: (S.Node -> Parser a) -> S.Node -> Either (Offset, String) a
runParser f n0 = prepare n0 >>= runChecked f

-- | Run a parser on a node that passed 'prepare'.
runChecked :: (S.Node -> Parser a) -> S.Node -> Either (Offset, String) a
runChecked f n = let Parser g = parseNode f n in g n.offset

-- | Run a parser on a node, so that 'fail' points to the node.
parseNode :: (S.Node -> Parser a) -> S.Node -> Parser a
parseNode f n = let Parser g = f n in Parser $ \_ -> g n.offset

-- | Fail with an error that points to the given node.
failAt :: S.Node -> String -> Parser a
failAt n msg = Parser $ \_ -> Left (n.offset, msg)

-- | Fail with an error about the kind of the node, e.g. "expected a list, but
-- got a string".
typeMismatch :: String -> S.Node -> Parser a
typeMismatch expected n = failAt n (mismatchMessage expected n)

-- Without the message, 'typeMismatch' is small enough to inline. Then the
-- optimizer sees the failure and removes the code after it, e.g. the generic
-- representation in a derived decoder.
mismatchMessage :: String -> S.Node -> String
mismatchMessage expected n = "expected " ++ expected ++ ", but got " ++ describeNode n
{-# NOINLINE mismatchMessage #-}

-- | The value of a node with its tag resolved. The items and the entries of a
-- collection stay nodes of the syntax tree.
data View
  = ScalarView !Value
  | SequenceView [S.Node]
  | MappingView [(S.Node, S.Node)]
  | -- | 'runParser' replaces the aliases, so only a node that a parser builds
    -- can have one.
    AliasView !T.Text

-- | The view of a node.
view :: S.Node -> View
view n = case n.content of
  S.Scalar style t -> ScalarView (scalarValue n.props.tag style t)
  S.Sequence _ xs -> SequenceView xs
  S.Mapping _ kvs -> MappingView kvs
  S.Alias name -> AliasView name
{-# INLINE view #-}

-- | The value of a scalar with the tag and the style.
scalarValue :: S.Tag -> S.ScalarStyle -> T.Text -> Value
scalarValue tag style t = case tag of
  S.NoTag
    | style == S.Plain -> resolvePlain t
    | otherwise -> String t
  S.NonSpecificTag -> String t
  -- 'runParser' rejects a value that is not valid for its tag.
  S.Tag tag' -> fromMaybe (String t) (resolveTagged tag' t)
{-# NOINLINE scalarValue #-}

-- | The kind of a node in plain words, e.g. "a list".
describeNode :: S.Node -> String
describeNode n = case view n of
  ScalarView v -> describe v
  SequenceView _ -> "a list"
  MappingView _ -> "a mapping"
  AliasView _ -> "an alias"

-- | The node is null.
isNullNode :: S.Node -> Bool
isNullNode n = case view n of
  ScalarView Null -> True
  _ -> False

-- | The text of a string node.
stringValue :: S.Node -> Maybe T.Text
stringValue n = case view n of
  ScalarView (String t) -> Just t
  _ -> Nothing

-- | The null node for a missing value.
nullNode :: S.Node
nullNode = S.Node S.noOffset S.noOffset S.noProps S.noComments (S.Scalar S.Plain "")

-- | Run the second parser if the first one fails. The error of the second one
-- wins, e.g.
--
-- @
-- (Left \<$> withInt pure n) \`orElse\` (Right \<$> withText pure n)
-- @
orElse :: Parser a -> Parser a -> Parser a
orElse (Parser g) (Parser h) = Parser $ \off -> case g off of
  Left _ -> h off
  r -> r

infixl 3 `orElse`

----------------------------------------
-- Scalars

-- | Run the parser if the node is null.
withNull :: Parser a -> S.Node -> Parser a
withNull p = parseNode $ \n -> case view n of
  ScalarView Null -> p
  _ -> typeMismatch "null" n

-- | The value of a boolean.
withBool :: (Bool -> Parser a) -> S.Node -> Parser a
withBool f = parseNode $ \n -> case view n of
  ScalarView (Bool b) -> f b
  ScalarView (String t)
    | isYaml11Bool t ->
        failAt n $
          "expected a boolean, but got the string "
            ++ show t
            ++ ", which is a boolean only in YAML 1.1"
  _ -> typeMismatch "a boolean" n

-- | The value of an integer.
withInt :: (Integer -> Parser a) -> S.Node -> Parser a
withInt f = parseNode $ \n -> case view n of
  ScalarView (Int i) -> f i
  _ -> typeMismatch "an integer" n

-- | The nearest double. An integer counts as a floating-point number too.
withFloat :: (Double -> Parser a) -> S.Node -> Parser a
withFloat f = parseNode $ \n -> case view n of
  ScalarView (Float v) -> f (floatValueToDouble v)
  ScalarView (Int i) -> f (fromInteger i)
  _ -> typeMismatch "a number" n

-- | The exact value of a finite number. An integer counts too, and negative
-- zero becomes 0.
--
-- For a conversion to an exact type, e.g. with 'truncate', use
-- 'withBoundedScientific', because a node that a program built can have any
-- exponent.
withScientific :: (Sci.Scientific -> Parser a) -> S.Node -> Parser a
withScientific f = parseNode $ \n -> case view n of
  ScalarView (Float (Finite s)) -> f s
  ScalarView (Float NegativeZero) -> f 0
  ScalarView (Int i) -> f (Sci.scientific i 0)
  ScalarView (Float _) -> fail "expected a finite number"
  _ -> typeMismatch "a number" n

-- | Like 'withScientific', but the exponent of the first digit must be in
-- the range from -1000 to 1000. Then a conversion to an exact integer, e.g.
-- with 'truncate', computes at most about 1000 more digits than the
-- coefficient has. The decoder applies a similar limit to floats, so the
-- check matters mostly for a node that a program built.
withBoundedScientific :: (Sci.Scientific -> Parser a) -> S.Node -> Parser a
withBoundedScientific f = withScientific $ \s ->
  let c = Sci.coefficient s
  in if
       -- The exponent of a zero also makes 'truncate' compute its power of 10.
       | c == 0 -> f 0
       | abs (toInteger (Sci.base10Exponent s) + toInteger (integerLog10 (abs c))) > maxExponent ->
           fail exponentOutOfRange
       | otherwise -> f s

-- | The text is a copy, so it does not keep the input alive.
withText :: (T.Text -> Parser a) -> S.Node -> Parser a
withText f = parseNode $ \n -> case view n of
  ScalarView (String t) -> f (T.copy t)
  _ -> typeMismatch "a string" n

----------------------------------------
-- Collections

-- | The items of a sequence.
withSequence :: ([S.Node] -> Parser a) -> S.Node -> Parser a
withSequence f = parseNode $ \n -> case n.content of
  S.Sequence _ xs -> f xs
  _ -> typeMismatch "a list" n

-- | The entries of a mapping. As for 'withText', the tag of a string key does
-- not matter, so two string keys with the same text are an error, e.g. @a@
-- and @!foo a@.
withMapping :: (Object -> Parser a) -> S.Node -> Parser a
withMapping f = parseNode $ \n -> case n.content of
  S.Mapping _ kvs -> mkObject n kvs >>= f
  _ -> typeMismatch "a mapping" n

-- | A mapping with fast access to the values of string keys.
data Object = Object
  { node :: !S.Node
  , entries :: [(S.Node, S.Node)]
  , index :: M.Map T.Text (S.Node, S.Node)
  , otherKeys :: [(S.Node, Value)]
  -- ^ The keys that are not strings, for the error of a lookup.
  }

-- A list with linear lookups is faster only up to about 10 keys, and it saves
-- only about 1% of the time to decode a typical record.
mkObject :: S.Node -> [(S.Node, S.Node)] -> Parser Object
mkObject n kvs = do
  index <- foldM insert M.empty kvs
  pure
    Object
      { node = n
      , entries = kvs
      , index = index
      , otherKeys = [(k, v) | (k, _) <- kvs, ScalarView v <- [view k], case v of String _ -> False; _ -> True]
      }
  where
    insert :: M.Map T.Text (S.Node, S.Node) -> (S.Node, S.Node) -> Parser (M.Map T.Text (S.Node, S.Node))
    insert m kv@(k, _) = case stringValue k of
      Just t -> case M.insertLookupWithKey (\_ _ old -> old) t kv m of
        (Just _, _) -> failAt k $ "duplicate key " ++ show t
        (Nothing, m') -> pure m'
      _ -> pure m

-- | The node of the mapping.
objectNode :: Object -> S.Node
objectNode o = o.node

-- | The entries of the mapping in the order of the input.
objectEntries :: Object -> [(S.Node, S.Node)]
objectEntries o = o.entries

-- | The string keys of the mapping in the order of the input.
objectKeys :: Object -> [T.Text]
objectKeys o = [T.copy t | (k, _) <- o.entries, Just t <- [stringValue k]]

-- | The value of a string key.
lookupKey :: T.Text -> Object -> Maybe S.Node
lookupKey key o = snd <$> M.lookup key o.index

-- | The value of a key. It is an error if the key is missing.
(.:) :: FromYaml a => Object -> T.Text -> Parser a
o .: key = case lookupKey key o of
  Just v -> parseNode parseYaml v
  Nothing -> missingKey o key

-- | The value of a key, or 'Nothing' if the key is missing or its value is
-- null.
(.:?) :: FromYaml a => Object -> T.Text -> Parser (Maybe a)
o .:? key =
  findKey o key >>= \case
    Just v | isNullNode v -> pure Nothing
    mv -> traverse (parseNode parseYaml) mv

-- | The value of a key, or 'Nothing' if the key is missing. Unlike '.:?', a
-- null value goes to the parser of the value, e.g. @'Maybe' a@ gives
-- @'Just' 'Nothing'@ for a null value.
(.:!) :: FromYaml a => Object -> T.Text -> Parser (Maybe a)
o .:! key = findKey o key >>= traverse (parseNode parseYaml)

-- | The value of a string key, or 'Nothing' if the key is missing. A key with
-- the same text that is not a string, e.g. 404, is an error, so that its
-- value does not go away.
findKey :: Object -> T.Text -> Parser (Maybe S.Node)
findKey o key = case M.lookup key o.index of
  Just (_, v) -> pure (Just v)
  Nothing -> case L.find (\(_, v) -> v == plain) o.otherKeys of
    Just (k, v) -> failAt k $ "the key " ++ T.unpack key ++ " is " ++ describe v ++ ", not a string"
    Nothing -> pure Nothing
  where
    plain :: Value
    plain = resolvePlain key

-- | The error for a string key that 'lookupKey' does not find. As for
-- 'findKey', a key with the same text that is not a string is the error
-- instead.
missingKey :: Object -> T.Text -> Parser a
missingKey o key = Parser $ \off ->
  -- The 'Left' must stay outside. Then the optimizer splits the function
  -- into a worker for the error and a small wrapper that adds the 'Left'.
  -- The wrapper inlines, so the failure is visible at the call site, and the
  -- code after it goes away, e.g. the generic representation in a derived
  -- decoder.
  let Parser g = findKey o key
  in Left $ case g off of
       Left err -> err
       Right _ -> (o.node.offset, "missing key " ++ show key)

-- | A default for an optional value.
(.!=) :: Parser (Maybe a) -> a -> Parser a
p .!= def = fromMaybe def <$> p

infixl 9 .:, .:?, .:!
infixl 8 .!=

-- | Fail at the first key that is not in the list. If a key in the list is
-- close to the unknown key, e.g. "host" to "hots", the error suggests it.
rejectUnknownKeys :: [T.Text] -> Object -> Parser ()
rejectUnknownKeys known o = forM_ o.entries $ \(k, _) -> case stringValue k of
  Just t
    | t `elem` known -> pure ()
    | otherwise ->
        failAt k $
          "unknown key " ++ show t ++ case suggestion (T.unpack t) of
            Just s -> ", did you mean " ++ show s ++ "?"
            Nothing -> ", expected one of: " ++ L.intercalate ", " (map T.unpack known)
  _ -> typeMismatch "a string as the key" k
  where
    suggestion :: String -> Maybe T.Text
    suggestion t =
      case L.sortOn fst [(d, s) | s <- known, let d = distance t (T.unpack s), d <= maxEdits, d < length t] of
        (_, s) : _ -> Just s
        [] -> Nothing
      where
        -- A swap of two adjacent characters, e.g. "hots" for "host", takes
        -- two edits.
        maxEdits :: Int
        maxEdits = 2

    -- The Levenshtein distance: the number of characters to insert, delete
    -- or change. After i characters of xs, the row holds the distance from
    -- them to each prefix of ys.
    distance :: String -> String -> Int
    distance xs ys = last (L.foldl' nextRow [0 .. length ys] (zip [1 ..] xs))
      where
        nextRow :: [Int] -> (Int, Char) -> [Int]
        nextRow row (i, x) = scanl cell i (zip3 ys row (drop 1 row))
          where
            -- The distances to the left, diagonally above and above.
            cell :: Int -> (Char, Int, Int) -> Int
            cell left (y, diagonal, above) =
              minimum [left + 1, above + 1, diagonal + if x == y then 0 else 1]

----------------------------------------
-- Class

-- | Types that can be parsed from a node. A type with a 'Generic' instance
-- can derive the instance, see "Yamlet.Generic".
class FromYaml a where
  parseYaml :: S.Node -> Parser a
  default parseYaml
    :: ( Generic a
       , GenericYaml a
       , Rep a ~ D1 d f
       , GConstructors f
       , GFlatten (FlattenFields a) f
       , GFromConstructor f
       )
    => S.Node -> Parser a
  parseYaml = genericParseYaml

  -- | Parse a list. The instance for 'Char' parses a string instead.
  parseYamlList :: S.Node -> Parser [a]
  parseYamlList = withSequence (mapM (parseNode parseYaml))

-- | The node of the syntax tree, with its styles and comments. An alias in
-- the input gives a copy of the node that it refers to.
instance FromYaml S.Node where
  parseYaml = pure

-- | The node with its tag and value resolved.
instance FromYaml Node where
  parseYaml n = case represent n of
    Right r -> pure r
    Left err -> Parser $ \_ -> Left err

instance FromYaml () where
  parseYaml = withNull (pure ())

instance FromYaml Bool where
  parseYaml = withBool pure

instance FromYaml Integer where
  parseYaml = withInt pure

instance FromYaml Natural where
  parseYaml = withInt $ \i ->
    if i < 0
      then fail "expected a non-negative integer"
      else pure (fromInteger i)

instance FromYaml Int where parseYaml = bounded
instance FromYaml Int8 where parseYaml = bounded
instance FromYaml Int16 where parseYaml = bounded
instance FromYaml Int32 where parseYaml = bounded
instance FromYaml Int64 where parseYaml = bounded
instance FromYaml Word where parseYaml = bounded
instance FromYaml Word8 where parseYaml = bounded
instance FromYaml Word16 where parseYaml = bounded
instance FromYaml Word32 where parseYaml = bounded
instance FromYaml Word64 where parseYaml = bounded

-- | An integer in the range of a bounded type.
bounded :: forall a. (Bounded a, Integral a) => S.Node -> Parser a
bounded = withInt $ \i ->
  if i < toInteger (minBound @a) || i > toInteger (maxBound @a)
    then
      fail $
        "the integer is out of the range from "
          ++ show (toInteger (minBound @a))
          ++ " to "
          ++ show (toInteger (maxBound @a))
    else pure (fromInteger i)

instance FromYaml Double where
  parseYaml = withFloat pure

instance FromYaml Sci.Scientific where
  parseYaml = withScientific pure

-- | @YYYY-MM-DD@, e.g. @2026-09-25@.
instance FromYaml Day where
  parseYaml = withIso8601 "expected a date such as 2026-09-25" parseDay

-- | @HH:MM@, with optional seconds and a fraction of a second of at most 12
-- digits, e.g. @12:30:05.25@.
instance FromYaml TimeOfDay where
  parseYaml = withIso8601 "expected a time such as 12:30:00" parseTimeOfDay

-- | A date and a time, separated by @T@ or a space, e.g.
-- @2026-09-25T12:30:00@.
instance FromYaml LocalTime where
  parseYaml =
    withIso8601 "expected a date and a time such as 2026-09-25T12:30:00" parseLocalTime

-- | A date, a time and a time zone, e.g. @2026-09-25T12:30:00+02:00@. The
-- time zone is @Z@, @+HH:MM@, @+HHMM@ or @+HH@.
instance FromYaml ZonedTime where
  parseYaml = withIso8601 zonedTimeMismatch parseZonedTime

-- | Like 'ZonedTime', converted to UTC.
instance FromYaml UTCTime where
  parseYaml = withIso8601 zonedTimeMismatch parseUTCTime

-- | A number of seconds, rounded down to a picosecond.
instance FromYaml NominalDiffTime where
  parseYaml = withBoundedScientific $ pure . secondsToNominalDiffTime . MkFixed . picoseconds

-- | A number of seconds, rounded down to a picosecond.
instance FromYaml DiffTime where
  parseYaml = withBoundedScientific $ pure . picosecondsToDiffTime . picoseconds

-- | The text form with hyphens, e.g. @123e4567-e89b-12d3-a456-426614174000@.
instance FromYaml UUID.UUID where
  parseYaml = withText $ maybe (fail "expected a UUID such as 123e4567-e89b-12d3-a456-426614174000") pure . UUID.fromText

-- | @YYYY-MM@, e.g. @2026-09@.
instance FromYaml Month where
  parseYaml = withIso8601 "expected a month such as 2026-09" parseMonth

-- | @YYYY-qN@, e.g. @2026-q3@.
instance FromYaml Quarter where
  parseYaml = withIso8601 "expected a quarter such as 2026-q3" parseQuarter

-- | @q1@ to @q4@.
instance FromYaml QuarterOfYear where
  parseYaml = withIso8601 "expected a quarter of a year such as q3" parseQuarterOfYear

-- | The English name in any case, e.g. @monday@.
instance FromYaml DayOfWeek where
  parseYaml = withText $ \t ->
    maybe (fail "expected a day of the week such as monday") pure $
      lookup (T.toLower t) [(T.toLower (T.pack (show d)), d) | d <- [Monday .. Sunday]]

-- | A mapping with the keys @months@ and @days@, e.g. @{months: 1, days: 2}@.
instance FromYaml CalendarDiffDays where
  parseYaml = withMapping $ \o -> do
    rejectUnknownKeys ["months", "days"] o
    CalendarDiffDays <$> o .: "months" <*> o .: "days"

-- | A mapping with the keys @months@ and @time@, a number of seconds, e.g.
-- @{months: 1, time: 1.5}@.
instance FromYaml CalendarDiffTime where
  parseYaml = withMapping $ \o -> do
    rejectUnknownKeys ["months", "time"] o
    CalendarDiffTime <$> o .: "months" <*> o .: "time"

zonedTimeMismatch :: String
zonedTimeMismatch = "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"

-- | A string in an ISO 8601 format, with the same rules as aeson.
withIso8601 :: String -> (T.Text -> Either String a) -> S.Node -> Parser a
withIso8601 mismatch p = withText $ either (const (fail mismatch)) pure . p

-- | The picoseconds in a number of seconds, rounded down.
picoseconds :: Sci.Scientific -> Integer
picoseconds s
  | k >= 0 = c * 10 ^ k
  | otherwise = c `div` 10 ^ negate k
  where
    c :: Integer
    c = Sci.coefficient s

    k :: Integer
    k = toInteger (Sci.base10Exponent s) + toInteger picoDecimals

-- | The nearest float. A conversion by way of 'Double' could round twice.
instance FromYaml Float where
  parseYaml = parseNode $ \n -> case view n of
    ScalarView (Float v) -> pure (floatValueToFloat v)
    ScalarView (Int i) -> pure (fromInteger i)
    _ -> typeMismatch "a number" n

instance FromYaml T.Text where
  parseYaml = withText pure

instance FromYaml TL.Text where
  parseYaml = withText (pure . TL.fromStrict)

instance FromYaml Char where
  parseYaml = withText $ \t -> case T.unpack t of
    [c] -> pure c
    _ -> fail "expected a single character"
  parseYamlList = withText (pure . T.unpack)

instance FromYaml a => FromYaml [a] where
  parseYaml = parseYamlList

instance FromYaml a => FromYaml (NE.NonEmpty a) where
  parseYaml = withSequence $ \case
    [] -> fail "expected a non-empty list"
    x : xs -> (NE.:|) <$> parseNode parseYaml x <*> mapM (parseNode parseYaml) xs

-- | Null is 'Nothing'.
instance FromYaml a => FromYaml (Maybe a) where
  parseYaml n = case view n of
    ScalarView Null -> pure Nothing
    _ -> Just <$> parseYaml n

-- | Two keys that convert to the same key, e.g. @1@ and @1.0@ for 'Double',
-- are an error.
--
-- Each key decodes with the instance of its type, so a map with 'T.Text' keys
-- rejects a key such as @404@ or @true@, because YAML reads it as an integer
-- or a boolean. Quote such a key in the input, e.g. @\"404\": not found@, or
-- use a key type that matches it, e.g. 'Int'.
instance (Ord k, FromYaml k, FromYaml v) => FromYaml (M.Map k v) where
  -- The index of 'withMapping' would be of no use here.
  parseYaml = parseNode $ \n -> case n.content of
    S.Mapping _ kvs -> foldM insert M.empty kvs
    _ -> typeMismatch "a mapping" n
    where
      insert :: M.Map k v -> (S.Node, S.Node) -> Parser (M.Map k v)
      insert m (k, v) = do
        k' <- parseNode parseYaml k
        M.alterF value k' m
        where
          value :: Maybe v -> Parser (Maybe v)
          value = \case
            Just _ -> failAt k "duplicate key after conversion"
            Nothing -> Just <$> parseNode parseYaml v

-- | Two keys that convert to the same key are an error.
instance FromYaml v => FromYaml (IM.IntMap v) where
  parseYaml = parseNode $ \n -> case n.content of
    S.Mapping _ kvs -> foldM insert IM.empty kvs
    _ -> typeMismatch "a mapping" n
    where
      insert :: IM.IntMap v -> (S.Node, S.Node) -> Parser (IM.IntMap v)
      insert m (k, v) = do
        k' <- parseNode parseYaml k
        IM.alterF value k' m
        where
          value :: Maybe v -> Parser (Maybe v)
          value = \case
            Just _ -> failAt k "duplicate key after conversion"
            Nothing -> Just <$> parseNode parseYaml v

-- | A list. Two elements that convert to the same value, e.g. @1@ and @1.0@
-- for 'Double', are an error.
instance (Ord a, FromYaml a) => FromYaml (Set.Set a) where
  parseYaml = withSequence (foldM insert Set.empty)
    where
      insert :: Set.Set a -> S.Node -> Parser (Set.Set a)
      insert s n = do
        x <- parseNode parseYaml n
        Set.alterF (\present -> if present then failAt n "duplicate element after conversion" else pure True) x s

-- | A list. Two equal elements are an error.
instance FromYaml IS.IntSet where
  parseYaml = withSequence (foldM insert IS.empty)
    where
      insert :: IS.IntSet -> S.Node -> Parser IS.IntSet
      insert s n = do
        x <- parseNode parseYaml n
        IS.alterF (\present -> if present then failAt n "duplicate element" else pure True) x s

instance FromYaml a => FromYaml (Seq.Seq a) where
  parseYaml = fmap Seq.fromList . parseYaml

-- | A list of the label and the subtrees, e.g. @[a, [[b, []]]]@.
instance FromYaml a => FromYaml (Tree.Tree a) where
  parseYaml = fmap (uncurry Tree.Node) . parseYaml

-- | @LT@, @EQ@ or @GT@.
instance FromYaml Ordering where
  parseYaml = withText $ \case
    "LT" -> pure LT
    "EQ" -> pure EQ
    "GT" -> pure GT
    _ -> fail "expected LT, EQ or GT"

-- | A string such as @1.2.3@. YAML reads a version with one dot, e.g. @1.10@,
-- as a number, so a number is an error.
instance FromYaml Version where
  parseYaml = parseNode $ \n -> case view n of
    ScalarView (String t) -> maybe (fail "expected a version such as 1.2.3") pure (version t)
    ScalarView v
      | Int _ <- v -> number
      | Float _ <- v -> number
      where
        number :: Parser Version
        number = fail $ "expected a version, but got " ++ describe v ++ ", quote the version, e.g. \"1.10\""
    _ -> typeMismatch "a version" n
    where
      -- The syntax that 'showVersion' writes. 'parseVersion' reads it too,
      -- but it takes quadratic time in the number of parts, and a part
      -- beyond the range of Int wraps around.
      version :: T.Text -> Maybe Version
      version t = case T.splitOn "-" t of
        branch : tags
          | all (\tag -> not (T.null tag) && T.all isAlphaNum tag) tags ->
              (\parts -> Version parts (map T.unpack tags)) <$> mapM readBoundedInt (T.splitOn "." branch)
        _ -> Nothing

-- | Null.
instance FromYaml (Proxy a) where
  parseYaml = withNull (pure Proxy)

instance FromYaml Void where
  parseYaml _ = fail "the type Void has no values"

-- | A mapping with the keys @numerator@ and @denominator@, e.g.
-- @{numerator: 1, denominator: 3}@.
instance (Integral a, FromYaml a) => FromYaml (Ratio a) where
  parseYaml = withMapping $ \o -> do
    rejectUnknownKeys ["numerator", "denominator"] o
    n <- (.:) @a o "numerator"
    d <- (.:) @a o "denominator"
    when (d == 0) $ fail "the denominator is 0"
    -- The reduction happens in Integer, where the gcd is fast. For another
    -- type, the gcd takes quadratic time in the number of digits, and in a
    -- bounded type, a negation can overflow, e.g. of minBound.
    let r = toInteger n % toInteger d
        fits :: Integer -> Bool
        fits x = toInteger (fromInteger @a x) == x
    if fits (numerator r) && fits (denominator r)
      then pure (fromInteger (numerator r) :% fromInteger (denominator r))
      else fail "the fraction is out of the range of the type"

-- | A number that is a multiple of the step of the type, e.g. @1.25@ for
-- 'Centi'. A number with more digits after the point is an error, not a
-- rounded value.
--
-- If the resolution is not a product of 2s and 5s, e.g. 3, most multiples of
-- the step have no decimal form, so they cannot come from YAML. For such a
-- resolution, use 'Rational' instead.
instance HasResolution a => FromYaml (Fixed a) where
  parseYaml = withBoundedScientific $ \s ->
    let scaled = s * fromInteger res
    in if Sci.isInteger scaled
         then pure (MkFixed (truncate scaled))
         else fail $ "expected a multiple of " ++ step
    where
      res :: Integer
      res = resolution (Proxy @a)

      -- 'show' rounds the step to the number of digits of the resolution, e.g.
      -- 0.03 for 1/40 and 0.4 for 1/3.
      step :: String
      step = case decimalPlaces res of
        Just places -> Sci.formatScientific Sci.Fixed Nothing (Sci.scientific (10 ^ places `div` res) (negate places))
        Nothing -> "1/" ++ show res

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Identity a)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Const a b)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Down a)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Sem.Min a)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Sem.Max a)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Sem.First a)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Sem.Last a)

-- | The value inside, or null for 'Nothing'.
deriving newtype instance FromYaml a => FromYaml (Mon.First a)

-- | The value inside, or null for 'Nothing'.
deriving newtype instance FromYaml a => FromYaml (Mon.Last a)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Sem.Dual a)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Sem.Sum a)

-- | The value inside.
deriving newtype instance FromYaml a => FromYaml (Sem.Product a)

-- | The value inside.
deriving newtype instance FromYaml Sem.All

-- | The value inside.
deriving newtype instance FromYaml Sem.Any

-- | A mapping with one key, @Left@ or @Right@, e.g. @{Left: 1}@.
instance (FromYaml a, FromYaml b) => FromYaml (Either a b) where
  parseYaml = withMapping $ \o -> case objectEntries o of
    [(k, v)] -> case stringValue k of
      Just "Left" -> Left <$> parseNode parseYaml v
      Just "Right" -> Right <$> parseNode parseYaml v
      _ -> failAt k "expected the key Left or Right"
    _ -> fail "expected a mapping with one key, Left or Right"

instance (FromYaml a1, FromYaml a2) => FromYaml (a1, a2) where
  parseYaml = withSequence $ \case
    [a1, a2] -> (,) <$> element a1 <*> element a2
    xs -> tupleSize 2 xs

instance (FromYaml a1, FromYaml a2, FromYaml a3) => FromYaml (a1, a2, a3) where
  parseYaml = withSequence $ \case
    [a1, a2, a3] -> (,,) <$> element a1 <*> element a2 <*> element a3
    xs -> tupleSize 3 xs

instance (FromYaml a1, FromYaml a2, FromYaml a3, FromYaml a4) => FromYaml (a1, a2, a3, a4) where
  parseYaml = withSequence $ \case
    [a1, a2, a3, a4] -> (,,,) <$> element a1 <*> element a2 <*> element a3 <*> element a4
    xs -> tupleSize 4 xs

instance
  (FromYaml a1, FromYaml a2, FromYaml a3, FromYaml a4, FromYaml a5)
  => FromYaml (a1, a2, a3, a4, a5)
  where
  parseYaml = withSequence $ \case
    [a1, a2, a3, a4, a5] ->
      (,,,,) <$> element a1 <*> element a2 <*> element a3 <*> element a4 <*> element a5
    xs -> tupleSize 5 xs

instance
  (FromYaml a1, FromYaml a2, FromYaml a3, FromYaml a4, FromYaml a5, FromYaml a6)
  => FromYaml (a1, a2, a3, a4, a5, a6)
  where
  parseYaml = withSequence $ \case
    [a1, a2, a3, a4, a5, a6] ->
      (,,,,,)
        <$> element a1
        <*> element a2
        <*> element a3
        <*> element a4
        <*> element a5
        <*> element a6
    xs -> tupleSize 6 xs

instance
  (FromYaml a1, FromYaml a2, FromYaml a3, FromYaml a4, FromYaml a5, FromYaml a6, FromYaml a7)
  => FromYaml (a1, a2, a3, a4, a5, a6, a7)
  where
  parseYaml = withSequence $ \case
    [a1, a2, a3, a4, a5, a6, a7] ->
      (,,,,,,)
        <$> element a1
        <*> element a2
        <*> element a3
        <*> element a4
        <*> element a5
        <*> element a6
        <*> element a7
    xs -> tupleSize 7 xs

instance
  (FromYaml a1, FromYaml a2, FromYaml a3, FromYaml a4, FromYaml a5, FromYaml a6, FromYaml a7, FromYaml a8)
  => FromYaml (a1, a2, a3, a4, a5, a6, a7, a8)
  where
  parseYaml = withSequence $ \case
    [a1, a2, a3, a4, a5, a6, a7, a8] ->
      (,,,,,,,)
        <$> element a1
        <*> element a2
        <*> element a3
        <*> element a4
        <*> element a5
        <*> element a6
        <*> element a7
        <*> element a8
    xs -> tupleSize 8 xs

instance
  ( FromYaml a1
  , FromYaml a2
  , FromYaml a3
  , FromYaml a4
  , FromYaml a5
  , FromYaml a6
  , FromYaml a7
  , FromYaml a8
  , FromYaml a9
  )
  => FromYaml (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
  parseYaml = withSequence $ \case
    [a1, a2, a3, a4, a5, a6, a7, a8, a9] ->
      (,,,,,,,,)
        <$> element a1
        <*> element a2
        <*> element a3
        <*> element a4
        <*> element a5
        <*> element a6
        <*> element a7
        <*> element a8
        <*> element a9
    xs -> tupleSize 9 xs

instance
  ( FromYaml a1
  , FromYaml a2
  , FromYaml a3
  , FromYaml a4
  , FromYaml a5
  , FromYaml a6
  , FromYaml a7
  , FromYaml a8
  , FromYaml a9
  , FromYaml a10
  )
  => FromYaml (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  where
  parseYaml = withSequence $ \case
    [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10] ->
      (,,,,,,,,,)
        <$> element a1
        <*> element a2
        <*> element a3
        <*> element a4
        <*> element a5
        <*> element a6
        <*> element a7
        <*> element a8
        <*> element a9
        <*> element a10
    xs -> tupleSize 10 xs

-- | An element of a tuple.
element :: FromYaml a => S.Node -> Parser a
element = parseNode parseYaml

-- | The error for a list with the wrong number of elements for a tuple.
tupleSize :: Int -> [S.Node] -> Parser a
tupleSize n xs = fail $ "expected a list of " ++ show n ++ " elements, but got " ++ show (length xs)

----------------------------------------
-- Generic

-- The default method calls this function for the reasons at
-- 'Yamlet.Encode.genericToYaml'.
genericParseYaml
  :: forall a d f
   . ( Generic a
     , GenericYaml a
     , Rep a ~ D1 d f
     , GConstructors f
     , GFlatten (FlattenFields a) f
     , GFromConstructor f
     )
  => S.Node -> Parser a
genericParseYaml n = gParseYaml (yamlOptions @a) (gFlatten @(FlattenFields a) @f) (from <$> yamlDefault @a) to n
{-# INLINE genericParseYaml #-}

-- The decoders of the constructors take a continuation, which starts as
-- 'to' and grows by 'M1', 'L1' or 'R1' at each level of the sum. So each
-- constructor applies 'to' to its own representation, e.g.
-- @to (M1 (L1 (M1 fields)))@, and the optimizer reduces this to the real
-- constructor in the same place.
--
-- In the direct style, each constructor returns its representation, the
-- branches meet in 'mplus', and 'to' comes after them. The optimizer then no
-- longer knows which branch produced the value, so the program builds 'L1',
-- 'R1' and ':*:' at run time and 'to' matches on them again.
--
-- The fields of one constructor need no continuation, because they build
-- their product in one place, and 'to' of the same branch consumes it.
--
-- The representation of the default goes down with the options, so that each
-- field finds its default value.
gParseYaml
  :: forall f d p a
   . ( GConstructors f
     , GFromConstructor f
     )
  => YamlOptions -> Bool -> Maybe (D1 d f p) -> (D1 d f p -> a) -> S.Node -> Parser a
gParseYaml opts flat def k n
  | isEnum @f opts =
      withText (\t -> maybe (unknown "value" t) id (gFromTag opts (k . M1) t)) n
  | isTagged @f opts = withMapping tagged n
  | otherwise = gFromUntagged opts (unM1 <$> def) (k . M1) n
  where
    tagged :: Object -> Parser a
    tagged o = case lookupKey opts.tagKey o of
      Nothing -> missingKey o opts.tagKey
      Just tn -> do
        t <- parseNode (parseYaml @T.Text) tn
        maybe (parseNode (\_ -> unknown "tag" t) tn) id (gFromTagged opts flat (unM1 <$> def) (k . M1) t o)

    unknown :: String -> T.Text -> Parser a
    unknown what t =
      fail $
        "unknown "
          ++ what
          ++ " "
          ++ show t
          ++ ", expected one of: "
          ++ T.unpack (T.intercalate ", " (map (constructorTag opts) (gConstructorNames @f)))
{-# INLINE gParseYaml #-}

class GFromConstructor f where
  -- | The constructor without fields with the tag.
  gFromTag :: YamlOptions -> (f p -> a) -> T.Text -> Maybe (Parser a)

  -- | The constructor with the tag, from the mapping that holds the tag, with
  -- the flag of 'FlattenFields'.
  gFromTagged :: YamlOptions -> Bool -> Maybe (f p) -> (f p -> a) -> T.Text -> Object -> Maybe (Parser a)

  -- | The only constructor, without a tag.
  gFromUntagged :: YamlOptions -> Maybe (f p) -> (f p -> a) -> S.Node -> Parser a

instance TypeError NoConstructors => GFromConstructor V1 where
  gFromTag _ _ _ = Nothing
  gFromTagged _ _ _ _ _ _ = Nothing
  gFromUntagged _ _ _ _ = fail "expected a type with constructors"

instance (GFromConstructor f, GFromConstructor g) => GFromConstructor (f :+: g) where
  gFromTag opts k t = gFromTag opts (k . L1) t `mplus` gFromTag opts (k . R1) t
  gFromTagged opts flat def k t o =
    gFromTagged opts flat (def >>= \case L1 x -> Just x; R1 _ -> Nothing) (k . L1) t o
      `mplus` gFromTagged opts flat (def >>= \case R1 x -> Just x; L1 _ -> Nothing) (k . R1) t o

  -- A type with several constructors always has a tag.
  gFromUntagged _ _ _ _ = fail "expected a tag"
  {-# INLINE gFromTag #-}
  {-# INLINE gFromTagged #-}

instance
  ( KnownSymbol name
  , GFields f
  , GFromFields f
  )
  => GFromConstructor (C1 (MetaCons name fixity isRecord) f)
  where
  gFromTag opts k t
    | t == tag && gArity @f == 0 = Just (k . M1 . fst <$> gFromValues [])
    | otherwise = Nothing
    where
      tag :: T.Text
      tag = constructorTag opts (symbolVal (Proxy @name))

  gFromTagged opts flat def k t o
    | t == constructorTag opts (symbolVal (Proxy @name)) = Just (k . M1 <$> fromObject opts flat [opts.tagKey] (unM1 <$> def) o)
    | otherwise = Nothing

  gFromUntagged opts def k n
    | gNamed @f || gArity @f == 0 = withMapping (fmap (k . M1) . fromObject opts False [] (unM1 <$> def)) n
    | gArity @f == 1 = k . M1 . fst <$> gFromValues [n]
    | otherwise = withSequence (fmap (k . M1) . fromList (gArity @f)) n

  {-# INLINE gFromTag #-}
  {-# INLINE gFromTagged #-}
  {-# INLINE gFromUntagged #-}

-- | The fields of a constructor from a mapping. The given keys, e.g. the tag
-- key, are no fields but valid keys.
fromObject
  :: forall f p
   . ( GFields f
     , GFromFields f
     )
  => YamlOptions -> Bool -> [T.Text] -> Maybe (f p) -> Object -> Parser (f p)
fromObject opts flat keys def o
  | gNamed @f = do
      reject (gNames @f opts)
      gFromObject opts def o
  | gArity @f == 0 = do
      reject []
      fst <$> gFromValues []
  | flat && not (all (isKey opts.contentsKey . fst) others) = merged
  | otherwise = do
      reject [opts.contentsKey]
      case lookupKey opts.contentsKey o of
        Just contents -> fromContents contents
        -- A missing contents key is null, if the fields accept null. A flat
        -- field can also have only optional keys.
        Nothing
          | Just fields <- def -> pure fields
          | flat -> either (const merged) pure (runChecked fromContents nullNode)
          | otherwise -> either (const (missingKey o opts.contentsKey)) pure (runChecked fromContents nullNode)
  where
    reject :: [T.Text] -> Parser ()
    reject fields = when opts.rejectUnknownFields $ rejectUnknownKeys (keys ++ fields) o

    -- The field decodes from the mapping without the given keys.
    merged :: Parser (f p)
    merged =
      let n = objectNode o
          style = case n.content of
            S.Mapping s _ -> s
            _ -> S.Block
      in fromContents (S.Node n.offset n.endOffset n.props n.comments (S.Mapping style others))

    others :: [(S.Node, S.Node)]
    others = foldr removeKey (objectEntries o) keys

    -- The keys are unique, so the entries after the match stay shared.
    removeKey :: T.Text -> [(S.Node, S.Node)] -> [(S.Node, S.Node)]
    removeKey key = \case
      kv@(k, _) : kvs
        | isKey key k -> kvs
        | otherwise -> kv : removeKey key kvs
      [] -> []

    isKey :: T.Text -> S.Node -> Bool
    isKey key k = case stringValue k of
      Just t -> t == key
      _ -> False

    fromContents :: S.Node -> Parser (f p)
    fromContents contents = case gArity @f of
      1 -> fst <$> gFromValues [contents]
      k -> withSequence (fromList k) contents
    {-# INLINE merged #-}
    {-# INLINE fromContents #-}
{-# INLINE fromObject #-}

-- | The fields of a constructor without field names from a list.
fromList :: GFromFields f => Int -> [S.Node] -> Parser (f p)
fromList k ns
  | length ns == k = fst <$> gFromValues ns
  | otherwise = fail $ "expected a list of " ++ show k ++ " elements, but got " ++ show (length ns)
{-# INLINE fromList #-}

class GFromFields f where
  -- | The fields from a mapping, with the given default for missing keys.
  gFromObject :: YamlOptions -> Maybe (f p) -> Object -> Parser (f p)

  -- | The fields from the start of the list, and the rest of the list.
  gFromValues :: [S.Node] -> Parser (f p, [S.Node])

instance GFromFields U1 where
  gFromObject _ _ _ = pure U1
  gFromValues ns = pure (U1, ns)

instance (GFromFields f, GFromFields g) => GFromFields (f :*: g) where
  gFromObject opts def o =
    (:*:)
      <$> gFromObject opts ((\(a :*: _) -> a) <$> def) o
      <*> gFromObject opts ((\(_ :*: b) -> b) <$> def) o
  gFromValues ns = do
    (a, rest) <- gFromValues ns
    (b, rest') <- gFromValues rest
    pure (a :*: b, rest')
  {-# INLINE gFromObject #-}
  {-# INLINE gFromValues #-}

instance
  ( KnownSymbol name
  , FromYaml a
  )
  => GFromFields (S1 (MetaSel (Just name) u s d) (Rec0 a))
  where
  gFromObject opts def o =
    M1 . K1 <$> case lookupKey key o of
      Just v -> parseNode parseYaml v
      Nothing -> case def of
        Just (M1 (K1 x)) -> pure x
        -- A missing field is null, if its type accepts null.
        Nothing -> either (const (missingKey o key)) pure (runChecked parseYaml nullNode)
    where
      key :: T.Text
      key = fieldKey @name opts
  gFromValues = nextField
  {-# INLINE gFromObject #-}
  {-# INLINE gFromValues #-}

instance FromYaml a => GFromFields (S1 (MetaSel Nothing u s d) (Rec0 a)) where
  gFromObject _ _ o = fail $ "expected a field without a name in " ++ describeNode (objectNode o)
  gFromValues = nextField
  {-# INLINE gFromValues #-}

-- | A field from the start of the list.
nextField :: FromYaml a => [S.Node] -> Parser (S1 m (Rec0 a) p, [S.Node])
nextField = \case
  n : ns -> (\x -> (M1 (K1 x), ns)) <$> parseNode parseYaml n
  [] -> fail "expected another field"
