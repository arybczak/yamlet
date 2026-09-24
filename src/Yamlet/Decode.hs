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

import Control.Applicative
import Control.Monad
import Data.Int
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word
import Numeric.Natural

import Yamlet.Node

-- | A parser of nodes. Its errors point to the node that the parser works on,
-- unless 'failAt' names another one.
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

-- | Try the second parser if the first one fails. The error of the second one
-- wins.
instance Alternative Parser where
  empty = fail "no parse"
  Parser g <|> Parser h = Parser $ \off -> case g off of
    Left _ -> h off
    r -> r

instance MonadPlus Parser

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
  _ -> typeMismatch "a boolean" n

-- | The value of an integer.
withInt :: (Integer -> Parser a) -> Node -> Parser a
withInt f = parseNode $ \n -> case n.value of
  Int i -> f i
  _ -> typeMismatch "an integer" n

-- | The nearest double. An integer counts as a floating-point number too.
withFloat :: (Double -> Parser a) -> Node -> Parser a
withFloat f = parseNode $ \n -> case n.value of
  Float v -> f (floatToDouble v)
  Int i -> f (fromInteger i)
  _ -> typeMismatch "a number" n

-- | The exact value of a finite number. An integer counts too.
withScientific :: (Sci.Scientific -> Parser a) -> Node -> Parser a
withScientific f = parseNode $ \n -> case n.value of
  Float (Finite s) -> f s
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
  }

mkObject :: Node -> [(Node, Node)] -> Parser Object
mkObject n kvs = do
  index <- foldM insert M.empty kvs
  pure Object {node = n, entries = kvs, index = index}
  where
    insert :: M.Map T.Text (Node, Node) -> (Node, Node) -> Parser (M.Map T.Text (Node, Node))
    insert m kv@(k, _) = case k.value of
      String t
        | t `M.member` m -> failAt k $ "duplicate key " ++ show t
        | otherwise -> pure $ M.insert t kv m
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
o .: key = case M.lookup key o.index of
  Just (_, v) -> parseNode parseYAML v
  Nothing -> failAt o.node $ "missing key " ++ show key

-- | The value of a key, or 'Nothing' if the key is missing or its value is
-- null.
(.:?) :: FromYAML a => Object -> T.Text -> Parser (Maybe a)
o .:? key = case M.lookup key o.index of
  Just (_, v) -> case v.value of
    Null -> pure Nothing
    _ -> Just <$> parseNode parseYAML v
  Nothing -> pure Nothing

-- | The value of a key, or 'Nothing' if the key is missing. Unlike '.:?', a
-- null value goes to the parser of the value, e.g. @'Maybe' a@ gives
-- @'Just' 'Nothing'@ for a null value.
(.:!) :: FromYAML a => Object -> T.Text -> Parser (Maybe a)
o .:! key = case M.lookup key o.index of
  Just (_, v) -> Just <$> parseNode parseYAML v
  Nothing -> pure Nothing

-- | A default for an optional value.
(.!=) :: Parser (Maybe a) -> a -> Parser a
p .!= def = maybe def id <$> p

infixl 9 .:, .:?, .:!
infixl 8 .!=

-- | Fail at the first key that is not in the list.
rejectUnknownKeys :: [T.Text] -> Object -> Parser ()
rejectUnknownKeys known o = forM_ o.entries $ \(k, _) -> case k.value of
  String t
    | t `elem` known -> pure ()
    | otherwise ->
        failAt k $
          "unknown key "
            ++ show t
            ++ ", expected one of: "
            ++ L.intercalate ", " (map T.unpack known)
  _ -> typeMismatch "a string" k

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

instance FromYAML Float where
  parseYAML = withFloat (pure . realToFrac)

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
instance (Ord k, FromYAML k, FromYAML v) => FromYAML (M.Map k v) where
  parseYAML = withMapping $ \o -> foldM insert M.empty o.entries
    where
      insert :: M.Map k v -> (Node, Node) -> Parser (M.Map k v)
      insert m (k, v) = do
        k' <- parseNode parseYAML k
        when (k' `M.member` m) $ failAt k "duplicate key after conversion"
        v' <- parseNode parseYAML v
        pure $ M.insert k' v' m

instance (FromYAML a, FromYAML b) => FromYAML (a, b) where
  parseYAML = withSequence $ \case
    [a, b] -> (,) <$> parseNode parseYAML a <*> parseNode parseYAML b
    _ -> fail "expected a list of 2 elements"

instance (FromYAML a, FromYAML b, FromYAML c) => FromYAML (a, b, c) where
  parseYAML = withSequence $ \case
    [a, b, c] ->
      (,,)
        <$> parseNode parseYAML a
        <*> parseNode parseYAML b
        <*> parseNode parseYAML c
    _ -> fail "expected a list of 3 elements"
