-- | Conversion of nodes to Haskell values, with errors that point to the
-- node that caused them.
module Yamlet.Decode
  ( -- * Class
    FromYAML (..)

    -- * Parser
  , Parser
  , runParser
  , parseNode
  , failAt
  , typeMismatch
  , orElse

    -- * Scalars
  , withNull
  , withBool
  , withInt
  , withFloat
  , withScientific
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
import Data.Fixed
import Data.Int
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Scientific qualified as Sci
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Word
import Numeric.Natural

import Yamlet.Internal.Schema
import Yamlet.Internal.Time
import Yamlet.Node

-- | A parser of nodes. Its errors point to the node that the parser works on,
-- unless 'failAt' names another one.
--
-- The parser has no 'Control.Applicative.Alternative' instance. To try
-- another parser after a failure, use 'orElse'. To reject a value, fail with
-- a message that says why:
--
-- @
-- port <- parseYAML n
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
runParser :: (Node -> Parser a) -> Node -> Either (Offset, String) a
runParser f n = let Parser g = parseNode f n in g n.offset

-- | Run a parser on a node, so that 'fail' points to the node.
parseNode :: (Node -> Parser a) -> Node -> Parser a
parseNode f n = let Parser g = f n in Parser $ \_ -> g n.offset

-- | Fail with an error that points to the given node.
failAt :: Node -> String -> Parser a
failAt n msg = Parser $ \_ -> Left (n.offset, msg)

-- | Fail with an error about the kind of the node, e.g. "expected a list, but
-- got a string".
typeMismatch :: String -> Node -> Parser a
typeMismatch expected n =
  failAt n $
    "expected " ++ expected ++ ", but got " ++ describe n.value

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
withNull :: Parser a -> Node -> Parser a
withNull p = parseNode $ \n -> case n.value of
  Null -> p
  _ -> typeMismatch "null" n

-- | The value of a boolean.
withBool :: (Bool -> Parser a) -> Node -> Parser a
withBool f = parseNode $ \n -> case n.value of
  Bool b -> f b
  String t
    | isYaml11Bool t ->
        failAt n $
          "expected a boolean, but got the string "
            ++ show t
            ++ ", which is a boolean only in YAML 1.1"
  _ -> typeMismatch "a boolean" n

-- | The value of an integer.
withInt :: (Integer -> Parser a) -> Node -> Parser a
withInt f = parseNode $ \n -> case n.value of
  Int i -> f i
  _ -> typeMismatch "an integer" n

-- | The nearest double. An integer counts as a floating-point number too.
withFloat :: (Double -> Parser a) -> Node -> Parser a
withFloat f = parseNode $ \n -> case n.value of
  Float v -> f (floatValueToDouble v)
  Int i -> f (fromInteger i)
  _ -> typeMismatch "a number" n

-- | The exact value of a finite number. An integer counts too.
withScientific :: (Sci.Scientific -> Parser a) -> Node -> Parser a
withScientific f = parseNode $ \n -> case n.value of
  Float (Finite s) -> f s
  Float NegativeZero -> f 0
  Int i -> f (Sci.scientific i 0)
  Float _ -> fail "expected a finite number"
  _ -> typeMismatch "a number" n

-- | The text is a copy, so it does not keep the input alive.
withText :: (T.Text -> Parser a) -> Node -> Parser a
withText f = parseNode $ \n -> case n.value of
  String t -> f (T.copy t)
  _ -> typeMismatch "a string" n

----------------------------------------
-- Collections

-- | The items of a sequence.
withSequence :: ([Node] -> Parser a) -> Node -> Parser a
withSequence f = parseNode $ \n -> case n.value of
  Sequence xs -> f xs
  _ -> typeMismatch "a list" n

-- | The entries of a mapping. As for 'withText', the tag of a string key does
-- not matter, so two string keys with the same text are an error, e.g. @a@
-- and @!foo a@.
withMapping :: (Object -> Parser a) -> Node -> Parser a
withMapping f = parseNode $ \n -> case n.value of
  Mapping kvs -> mkObject n kvs >>= f
  _ -> typeMismatch "a mapping" n

-- | A mapping with fast access to the values of string keys.
data Object = Object
  { node :: !Node
  , entries :: [(Node, Node)]
  , index :: M.Map T.Text (Node, Node)
  , otherKeys :: [Node]
  -- ^ The keys that are not strings, for the error of a lookup.
  }

-- A list with linear lookups is faster only up to about 10 keys, and it saves
-- only about 1% of the time to decode a typical record.
mkObject :: Node -> [(Node, Node)] -> Parser Object
mkObject n kvs = do
  index <- foldM insert M.empty kvs
  pure
    Object
      { node = n
      , entries = kvs
      , index = index
      , otherKeys = [k | (k, _) <- kvs, case k.value of String _ -> False; _ -> True]
      }
  where
    insert :: M.Map T.Text (Node, Node) -> (Node, Node) -> Parser (M.Map T.Text (Node, Node))
    insert m kv@(k, _) = case k.value of
      String t -> case M.insertLookupWithKey (\_ _ old -> old) t kv m of
        (Just _, _) -> failAt k $ "duplicate key " ++ show t
        (Nothing, m') -> pure m'
      _ -> pure m

-- | The node of the mapping.
objectNode :: Object -> Node
objectNode o = o.node

-- | The entries of the mapping in the order of the input.
objectEntries :: Object -> [(Node, Node)]
objectEntries o = o.entries

-- | The string keys of the mapping in the order of the input.
objectKeys :: Object -> [T.Text]
objectKeys o = [T.copy t | (k, _) <- o.entries, String t <- [k.value]]

-- | The value of a string key.
lookupKey :: T.Text -> Object -> Maybe Node
lookupKey key o = snd <$> M.lookup key o.index

-- | The value of a key. It is an error if the key is missing.
(.:) :: FromYAML a => Object -> T.Text -> Parser a
o .: key =
  findKey o key >>= \case
    Just v -> parseNode parseYAML v
    Nothing -> failAt o.node $ "missing key " ++ show key

-- | The value of a key, or 'Nothing' if the key is missing or its value is
-- null.
(.:?) :: FromYAML a => Object -> T.Text -> Parser (Maybe a)
o .:? key =
  findKey o key >>= \case
    Just v | Null <- v.value -> pure Nothing
    mv -> traverse (parseNode parseYAML) mv

-- | The value of a key, or 'Nothing' if the key is missing. Unlike '.:?', a
-- null value goes to the parser of the value, e.g. @'Maybe' a@ gives
-- @'Just' 'Nothing'@ for a null value.
(.:!) :: FromYAML a => Object -> T.Text -> Parser (Maybe a)
o .:! key = findKey o key >>= traverse (parseNode parseYAML)

-- | The value of a string key, or 'Nothing' if the key is missing. A key with
-- the same text that is not a string, e.g. 404, is an error, so that its
-- value does not go away.
findKey :: Object -> T.Text -> Parser (Maybe Node)
findKey o key = case M.lookup key o.index of
  Just (_, v) -> pure (Just v)
  Nothing -> case L.find (\k -> k.value == plain) o.otherKeys of
    Just k -> failAt k $ "the key " ++ T.unpack key ++ " is " ++ describe k.value ++ ", not a string"
    Nothing -> pure Nothing
  where
    plain :: Value
    plain = resolvePlain key

-- | A default for an optional value.
(.!=) :: Parser (Maybe a) -> a -> Parser a
p .!= def = fromMaybe def <$> p

infixl 9 .:, .:?, .:!
infixl 8 .!=

-- | Fail at the first key that is not in the list. If a key in the list is
-- close to the unknown key, e.g. "host" to "hots", the error suggests it.
rejectUnknownKeys :: [T.Text] -> Object -> Parser ()
rejectUnknownKeys known o = forM_ o.entries $ \(k, _) -> case k.value of
  String t
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
      case L.sortOn fst [(d, s) | s <- known, let d = distance t (T.unpack s), d <= 2, d < length t] of
        (_, s) : _ -> Just s
        [] -> Nothing

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

-- | Types that can be parsed from a node.
class FromYAML a where
  parseYAML :: Node -> Parser a

  -- | Parse a list. The instance for 'Char' parses a string instead.
  parseYAMLList :: Node -> Parser [a]
  parseYAMLList = withSequence (mapM (parseNode parseYAML))

instance FromYAML Node where
  parseYAML = pure

instance FromYAML () where
  parseYAML = withNull (pure ())

instance FromYAML Bool where
  parseYAML = withBool pure

instance FromYAML Integer where
  parseYAML = withInt pure

instance FromYAML Natural where
  parseYAML = withInt $ \i ->
    if i < 0
      then fail "expected a non-negative integer"
      else pure (fromInteger i)

instance FromYAML Int where parseYAML = bounded
instance FromYAML Int8 where parseYAML = bounded
instance FromYAML Int16 where parseYAML = bounded
instance FromYAML Int32 where parseYAML = bounded
instance FromYAML Int64 where parseYAML = bounded
instance FromYAML Word where parseYAML = bounded
instance FromYAML Word8 where parseYAML = bounded
instance FromYAML Word16 where parseYAML = bounded
instance FromYAML Word32 where parseYAML = bounded
instance FromYAML Word64 where parseYAML = bounded

-- | An integer in the range of a bounded type.
bounded :: forall a. (Bounded a, Integral a) => Node -> Parser a
bounded = withInt $ \i ->
  if i < toInteger (minBound @a) || i > toInteger (maxBound @a)
    then
      fail $
        "the integer is out of the range from "
          ++ show (toInteger (minBound @a))
          ++ " to "
          ++ show (toInteger (maxBound @a))
    else pure (fromInteger i)

instance FromYAML Double where
  parseYAML = withFloat pure

instance FromYAML Sci.Scientific where
  parseYAML = withScientific pure

-- | @YYYY-MM-DD@, e.g. @2026-09-25@.
instance FromYAML Day where
  parseYAML = withText $ maybe (fail "expected a date such as 2026-09-25") pure . parseDay

-- | @HH:MM@, with optional seconds and a fraction of a second, e.g.
-- @12:30:05.25@.
instance FromYAML TimeOfDay where
  parseYAML = withText $ maybe (fail "expected a time such as 12:30:00") pure . parseTimeOfDay

-- | A date and a time, separated by @T@ or a space, e.g.
-- @2026-09-25T12:30:00@.
instance FromYAML LocalTime where
  parseYAML =
    withText $ maybe (fail "expected a date and a time such as 2026-09-25T12:30:00") pure . parseLocalTime

-- | A date, a time and a time zone, e.g. @2026-09-25T12:30:00+02:00@. The
-- time zone is @Z@, @+HH:MM@, @+HHMM@ or @+HH@.
instance FromYAML ZonedTime where
  parseYAML = withText $ maybe (fail zonedTimeMismatch) pure . parseZonedTime

-- | Like 'ZonedTime', converted to UTC.
instance FromYAML UTCTime where
  parseYAML = withText $ maybe (fail zonedTimeMismatch) pure . parseUTCTime

-- | A number of seconds, rounded down to a picosecond.
instance FromYAML NominalDiffTime where
  parseYAML = withScientific $ fmap (secondsToNominalDiffTime . MkFixed) . duration

-- | A number of seconds, rounded down to a picosecond.
instance FromYAML DiffTime where
  parseYAML = withScientific $ fmap picosecondsToDiffTime . duration

zonedTimeMismatch :: String
zonedTimeMismatch = "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"

-- | The picoseconds in a number of seconds.
duration :: Sci.Scientific -> Parser Integer
duration = maybe (fail "the duration is out of range") pure . picoseconds

-- | The nearest float. A conversion by way of 'Double' could round twice.
instance FromYAML Float where
  parseYAML = parseNode $ \n -> case n.value of
    Float v -> pure (floatValueToFloat v)
    Int i -> pure (fromInteger i)
    _ -> typeMismatch "a number" n

instance FromYAML T.Text where
  parseYAML = withText pure

instance FromYAML TL.Text where
  parseYAML = withText (pure . TL.fromStrict)

instance FromYAML Char where
  parseYAML = withText $ \t -> case T.unpack t of
    [c] -> pure c
    _ -> fail "expected a single character"
  parseYAMLList = withText (pure . T.unpack)

instance FromYAML a => FromYAML [a] where
  parseYAML = parseYAMLList

instance FromYAML a => FromYAML (NE.NonEmpty a) where
  parseYAML = withSequence $ \case
    [] -> fail "expected a non-empty list"
    x : xs -> (NE.:|) <$> parseNode parseYAML x <*> mapM (parseNode parseYAML) xs

-- | Null is 'Nothing'.
instance FromYAML a => FromYAML (Maybe a) where
  parseYAML n = case n.value of
    Null -> pure Nothing
    _ -> Just <$> parseYAML n

-- | Two keys that convert to the same key, e.g. @1@ and @1.0@ for 'Double',
-- are an error.
--
-- Each key decodes with the instance of its type, so a map with 'T.Text' keys
-- rejects a key such as @404@ or @true@, because YAML reads it as an integer
-- or a boolean. Quote such a key in the input, e.g. @\"404\": not found@, or
-- use a key type that matches it, e.g. 'Int'.
instance (Ord k, FromYAML k, FromYAML v) => FromYAML (M.Map k v) where
  -- The index of 'withMapping' would be of no use here.
  parseYAML = parseNode $ \n -> case n.value of
    Mapping kvs -> foldM insert M.empty kvs
    _ -> typeMismatch "a mapping" n
    where
      insert :: M.Map k v -> (Node, Node) -> Parser (M.Map k v)
      insert m (k, v) = do
        k' <- parseNode parseYAML k
        M.alterF value k' m
        where
          value :: Maybe v -> Parser (Maybe v)
          value = \case
            Just _ -> failAt k "duplicate key after conversion"
            Nothing -> Just <$> parseNode parseYAML v

-- | Two keys that convert to the same key are an error.
instance FromYAML v => FromYAML (IM.IntMap v) where
  parseYAML = parseNode $ \n -> case n.value of
    Mapping kvs -> foldM insert IM.empty kvs
    _ -> typeMismatch "a mapping" n
    where
      insert :: IM.IntMap v -> (Node, Node) -> Parser (IM.IntMap v)
      insert m (k, v) = do
        k' <- parseNode parseYAML k
        IM.alterF value k' m
        where
          value :: Maybe v -> Parser (Maybe v)
          value = \case
            Just _ -> failAt k "duplicate key after conversion"
            Nothing -> Just <$> parseNode parseYAML v

-- | A list. Two elements that convert to the same value, e.g. @1@ and @1.0@
-- for 'Double', are an error.
instance (Ord a, FromYAML a) => FromYAML (Set.Set a) where
  parseYAML = withSequence (foldM insert Set.empty)
    where
      insert :: Set.Set a -> Node -> Parser (Set.Set a)
      insert s n = do
        x <- parseNode parseYAML n
        Set.alterF (\present -> if present then failAt n "duplicate element after conversion" else pure True) x s

-- | A list. Two equal elements are an error.
instance FromYAML IS.IntSet where
  parseYAML = withSequence (foldM insert IS.empty)
    where
      insert :: IS.IntSet -> Node -> Parser IS.IntSet
      insert s n = do
        x <- parseNode parseYAML n
        IS.alterF (\present -> if present then failAt n "duplicate element" else pure True) x s

instance FromYAML a => FromYAML (Seq.Seq a) where
  parseYAML = fmap Seq.fromList . parseYAML

-- | A mapping with one key, @Left@ or @Right@, e.g. @{Left: 1}@.
instance (FromYAML a, FromYAML b) => FromYAML (Either a b) where
  parseYAML = withMapping $ \o -> case objectEntries o of
    [(k, v)] -> case k.value of
      String "Left" -> Left <$> parseNode parseYAML v
      String "Right" -> Right <$> parseNode parseYAML v
      _ -> failAt k "expected the key Left or Right"
    _ -> fail "expected a mapping with one key, Left or Right"

instance (FromYAML a1, FromYAML a2) => FromYAML (a1, a2) where
  parseYAML = withSequence $ \case
    [a1, a2] -> (,) <$> element a1 <*> element a2
    xs -> tupleSize 2 xs

instance (FromYAML a1, FromYAML a2, FromYAML a3) => FromYAML (a1, a2, a3) where
  parseYAML = withSequence $ \case
    [a1, a2, a3] -> (,,) <$> element a1 <*> element a2 <*> element a3
    xs -> tupleSize 3 xs

instance (FromYAML a1, FromYAML a2, FromYAML a3, FromYAML a4) => FromYAML (a1, a2, a3, a4) where
  parseYAML = withSequence $ \case
    [a1, a2, a3, a4] -> (,,,) <$> element a1 <*> element a2 <*> element a3 <*> element a4
    xs -> tupleSize 4 xs

instance
  (FromYAML a1, FromYAML a2, FromYAML a3, FromYAML a4, FromYAML a5)
  => FromYAML (a1, a2, a3, a4, a5)
  where
  parseYAML = withSequence $ \case
    [a1, a2, a3, a4, a5] ->
      (,,,,) <$> element a1 <*> element a2 <*> element a3 <*> element a4 <*> element a5
    xs -> tupleSize 5 xs

instance
  (FromYAML a1, FromYAML a2, FromYAML a3, FromYAML a4, FromYAML a5, FromYAML a6)
  => FromYAML (a1, a2, a3, a4, a5, a6)
  where
  parseYAML = withSequence $ \case
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
  (FromYAML a1, FromYAML a2, FromYAML a3, FromYAML a4, FromYAML a5, FromYAML a6, FromYAML a7)
  => FromYAML (a1, a2, a3, a4, a5, a6, a7)
  where
  parseYAML = withSequence $ \case
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
  (FromYAML a1, FromYAML a2, FromYAML a3, FromYAML a4, FromYAML a5, FromYAML a6, FromYAML a7, FromYAML a8)
  => FromYAML (a1, a2, a3, a4, a5, a6, a7, a8)
  where
  parseYAML = withSequence $ \case
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
  ( FromYAML a1
  , FromYAML a2
  , FromYAML a3
  , FromYAML a4
  , FromYAML a5
  , FromYAML a6
  , FromYAML a7
  , FromYAML a8
  , FromYAML a9
  )
  => FromYAML (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
  parseYAML = withSequence $ \case
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
  ( FromYAML a1
  , FromYAML a2
  , FromYAML a3
  , FromYAML a4
  , FromYAML a5
  , FromYAML a6
  , FromYAML a7
  , FromYAML a8
  , FromYAML a9
  , FromYAML a10
  )
  => FromYAML (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  where
  parseYAML = withSequence $ \case
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
element :: FromYAML a => Node -> Parser a
element = parseNode parseYAML

-- | The error for a list with the wrong number of elements for a tuple.
tupleSize :: Int -> [Node] -> Parser a
tupleSize n xs = fail $ "expected a list of " ++ show n ++ " elements, but got " ++ show (length xs)
