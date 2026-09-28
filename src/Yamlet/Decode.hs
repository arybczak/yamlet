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

import Control.Applicative
import Control.Monad
import Data.Fixed
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
import Yamlet.Internal.View
import Yamlet.Value

-- | A parser of nodes. Its errors point to the node that the parser works on,
-- unless 'failAt' names another one.
--
-- The parser has no t'Control.Applicative.Alternative' instance. To try
-- another parser after a failure, use 'orElse'. To reject a value, fail with
-- a message that says why:
--
-- @
-- port <- parseYaml n
-- unless (port > 0 && port < 65536) $ fail "the port must be from 1 to 65535"
-- @
--
-- The applicative operators collect the errors of both parts: '<*>', '*>',
-- '<*', 'liftA2', and the functions that use them, e.g. 'traverse',
-- 'mapM' on a list and 'Data.Foldable.for_'. This parser gives the errors of
-- the unknown keys and of all fields together:
--
-- @
-- rejectUnknownKeys [\"name\", \"paths\"] o
--   *> (Config \<$> o .: \"name\" \<*> o .: \"paths\")
-- @
--
-- '>>=' and '>>' stop at the first error. A statement of a @do@ block also
-- stops, e.g. the check of the port above. The functions that use '>>' also
-- stop, e.g. 'Control.Monad.mapM_' and 'Control.Monad.forM_'.
--
-- The operator changes only the errors, never the result. With
-- @ApplicativeDo@, GHC turns the independent statements of a @do@ block that
-- ends with 'pure' into '<*>'. Then they collect errors.
newtype Parser a = Parser (S.Offset -> Result a)

-- | The errors of a parser and its value. The value of a parser with errors
-- is 'failed'.
--
-- '<*>' applies the values without a branch on the errors, and it joins the
-- errors apart from them. The optimizer can then combine the values of a
-- derived decoder as for a pure function, and the generic representation
-- goes away.
data Result a = Result !Errors a

-- | The errors of a parser in a tree, so that two sets of errors join in
-- constant time.
data Errors
  = NoErrors
  | -- | An error with the notes that go right after it, e.g. the first key of
    -- a duplicate key.
    OneError !S.Offset String [(S.Offset, String)]
  | BothErrors Errors Errors

bothErrors :: Errors -> Errors -> Errors
bothErrors e1 e2 = case (e1, e2) of
  (NoErrors, _) -> e2
  (_, NoErrors) -> e1
  _ -> BothErrors e1 e2
-- If GHC inlines this function into '<*>', the branches on the errors take
-- the values of the parts with them. Then the inspection test of the derived
-- decoder with 100 fields fails.
{-# NOINLINE bothErrors #-}

-- | The value of a parser with errors. Nothing reads it, because each
-- consumer of a result looks at the errors first.
failed :: a
failed = errorWithoutStackTrace "Yamlet.Decode: the value of a failed parser"

-- | A result with one error.
failure :: S.Offset -> String -> Result a
failure off msg = Result (OneError off msg []) failed

-- | The errors in the order of their offsets, each with its notes after it.
-- Errors at the same offset keep their order.
sortedErrors :: Errors -> [(S.Offset, String)]
sortedErrors = concatMap (\(off, msg, notes) -> (off, msg) : notes) . L.sortOn (\(off, _, _) -> off) . flip go []
  where
    go :: Errors -> [(S.Offset, String, [(S.Offset, String)])] -> [(S.Offset, String, [(S.Offset, String)])]
    go = \case
      NoErrors -> id
      OneError off msg notes -> ((off, msg, notes) :)
      BothErrors e1 e2 -> go e1 . go e2

instance Functor Parser where
  fmap f (Parser g) = Parser $ \off -> case g off of
    Result e a -> Result e (f a)

-- '<*>' differs from 'ap', and '>>' differs from '*>', in the errors, but not
-- in the results.
instance Applicative Parser where
  pure a = Parser $ \_ -> Result NoErrors a
  Parser f <*> Parser g = Parser $ \off -> case f off of
    Result e1 h -> case g off of
      Result e2 a -> Result (bothErrors e1 e2) (h a)

-- A statement of a @do@ block must not run after a failed check, e.g. an
-- index into a list after the check of its length. The default of '>>' uses
-- '>>=', which stops there.
instance Monad Parser where
  Parser g >>= k = Parser $ \off -> case g off of
    Result NoErrors a -> let Parser h = k a in h off
    Result e _ -> Result e failed

instance MonadFail Parser where
  fail msg = Parser $ \off -> failure off msg

-- | Run a parser on a node. Each error is the offset of the node that caused
-- it and the message. The errors are in the order of the offsets. A note on
-- an error comes right after it, e.g. the first key of a duplicate key.
--
-- First, the function makes the checks of 'Yamlet.decodeDocument' on the
-- node, e.g. for duplicate keys. It also replaces each alias with the node
-- that the alias refers to. If a check fails, the result has only the error
-- of that check, with its notes.
runParser :: (S.Node -> Parser a) -> S.Node -> Either (NE.NonEmpty (S.Offset, String)) a
runParser f n0 = case prepare n0 of
  Left err -> Left err
  Right n -> case runChecked f n of
    Result NoErrors a -> Right a
    Result e _ -> Left (NE.fromList (map (withMergeHint (mergeValues n)) (sortedErrors e)))
  where
    -- An error at the value of a key << that is a collection, e.g. an alias of
    -- a mapping, is likely from a merge key of YAML 1.1.
    withMergeHint :: Set.Set S.Offset -> (S.Offset, String) -> (S.Offset, String)
    withMergeHint offs (off, msg)
      | off `Set.member` offs = (off, msg ++ ", " ++ noMergeKeys)
      | otherwise = (off, msg)

    mergeValues :: S.Node -> Set.Set S.Offset
    mergeValues n = case n.content of
      S.Sequence _ xs -> foldMap mergeValues xs
      S.Mapping _ kvs -> foldMap (\(k, v) -> mergeValue k v <> mergeValues k <> mergeValues v) kvs
      _ -> Set.empty

    mergeValue :: S.Node -> S.Node -> Set.Set S.Offset
    mergeValue k v = case (stringValue k, v.content) of
      (Just "<<", S.Mapping {}) -> Set.singleton v.offset
      (Just "<<", S.Sequence {}) -> Set.singleton v.offset
      _ -> Set.empty

-- | Run a parser on a node that passed 'prepare'.
runChecked :: (S.Node -> Parser a) -> S.Node -> Result a
runChecked f n = let Parser g = parseNode f n in g n.offset

-- | The value of a parser on a node that passed 'prepare', if it has no
-- errors.
succeeds :: (S.Node -> Parser a) -> S.Node -> Maybe a
succeeds f n = case runChecked f n of
  Result NoErrors a -> Just a
  Result _ _ -> Nothing

-- | Run a parser on a node, so that 'fail' points to the node.
parseNode :: (S.Node -> Parser a) -> S.Node -> Parser a
parseNode f n = let Parser g = f n in Parser $ \_ -> g n.offset

-- | Fail with an error that points to the given node.
failAt :: S.Node -> String -> Parser a
failAt n msg = Parser $ \_ -> failure n.offset msg

-- | Fail with an error about the kind of the node, e.g. "expected a list, but
-- got a string".
typeMismatch :: String -> S.Node -> Parser a
typeMismatch expected n = failAt n (mismatchMessage expected n)

mismatchMessage :: String -> S.Node -> String
mismatchMessage expected n = "expected " ++ expected ++ ", but got " ++ describeNode n

-- | The null node for a missing value.
nullNode :: S.Node
nullNode = S.Node S.noOffset S.noOffset S.noProps S.noComments (S.Scalar S.Plain "")

-- | Run the second parser if the first one fails. The error of the second one
-- wins, e.g.
--
-- >>> :{
-- newtype Port = Port (Either Integer T.Text)
--   deriving stock (Show)
-- instance FromYaml Port where
--   parseYaml n = Port <$> ((Left <$> withInt pure n) `orElse` (Right <$> withText pure n))
-- :}
--
-- >>> decodeText @Port "8080"
-- Right (Port (Left 8080))
--
-- >>> decodeText @Port "http"
-- Right (Port (Right "http"))
orElse :: Parser a -> Parser a -> Parser a
orElse (Parser g) (Parser h) = Parser $ \off -> case g off of
  r@(Result NoErrors _) -> r
  _ -> h off

infixl 3 `orElse`

----------------------------------------
-- Scalars

-- | Run the parser if the node is null.
withNull :: Parser a -> S.Node -> Parser a
withNull p = parseNode $ \n -> case view n of
  NullView -> p
  _ -> typeMismatch "null" n

-- | The value of a boolean.
withBool :: (Bool -> Parser a) -> S.Node -> Parser a
withBool f = parseNode $ \n -> case view n of
  BoolView b -> f b
  StringView t
    | S.Scalar S.Plain _ <- n.content
    , S.NoTag <- n.props.tag
    , isYaml11Bool t ->
        failAt n $
          "expected a boolean, but got the string "
            ++ show t
            ++ ", which is a boolean only in YAML 1.1, use true or false"
  _ -> typeMismatch "a boolean" n

-- | The value of an integer.
withInt :: (Integer -> Parser a) -> S.Node -> Parser a
withInt f = parseNode $ \n -> case view n of
  IntView i -> f i
  _ -> typeMismatch "an integer" n

-- | The nearest double. An integer counts as a floating-point number too.
withFloat :: (Double -> Parser a) -> S.Node -> Parser a
withFloat f = parseNode $ \n -> case view n of
  FloatView v -> f (floatValueToDouble v)
  IntView i -> f (fromInteger i)
  _ -> typeMismatch "a number" n

-- | The exact value of a finite number. An integer counts too, and negative
-- zero becomes 0.
--
-- For a conversion to an exact type, e.g. with 'truncate', use
-- 'withBoundedScientific', because a node that a program built can have any
-- exponent.
withScientific :: (Sci.Scientific -> Parser a) -> S.Node -> Parser a
withScientific f = parseNode $ \n -> case view n of
  FloatView (Finite s) -> f s
  FloatView NegativeZero -> f 0
  IntView i -> f (Sci.scientific i 0)
  FloatView _ -> fail "expected a finite number"
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

-- | The text of a string. The text is a copy, so it does not keep the input
-- alive. For a plain scalar that YAML reads as a number or a boolean, e.g.
-- @3.10@, the error suggests quotes.
withText :: (T.Text -> Parser a) -> S.Node -> Parser a
withText f = parseNode $ \n -> case view n of
  StringView t -> f (T.copy t)
  _ -> failAt n (stringMismatch n)

-- | A string that is one of the names, e.g. the tags of the constructors. For
-- another node, the error suggests quotes only if the quoted text is a name,
-- e.g. not for @null@. Otherwise it lists the names.
withName :: [T.Text] -> (T.Text -> Parser a) -> S.Node -> Parser a
withName names f = parseNode $ \n -> case (view n, n.content) of
  (StringView t, _) -> f t
  (_, S.Scalar S.Plain t) | S.NoTag <- n.props.tag, t `elem` names -> failAt n (stringMismatch n)
  _ -> typeMismatch ("one of: " ++ L.intercalate ", " (map T.unpack names)) n

-- | The message for a node that is not a string, with the hint to quote a
-- plain number, boolean or written null.
stringMismatch :: S.Node -> String
stringMismatch n = mismatchMessage "a string" n ++ hint
  where
    hint :: String
    hint = case n.content of
      S.Scalar S.Plain t
        | S.NoTag <- n.props.tag
        , notString t ->
            ", quote the value, e.g. '" ++ T.unpack t ++ "'"
      _ -> ""

    notString :: T.Text -> Bool
    notString t = case view n of
      IntView _ -> True
      FloatView _ -> True
      BoolView _ -> True
      -- An empty value is more likely a forgotten value than a string.
      NullView -> not (T.null t)
      _ -> False
-- Without the pragma, the interface file has no unfolding of 'withText', so
-- other modules cannot inline it.
{-# NOINLINE stringMismatch #-}

----------------------------------------
-- Collections

-- | The items of a sequence. As for 'withMapping', the lines above the
-- sequence go to its first item.
withSequence :: ([S.Node] -> Parser a) -> S.Node -> Parser a
withSequence f = parseNode $ \n -> case n.content of
  S.Sequence _ xs -> f (items n xs)
  _ -> typeMismatch "a list" n

-- | The items of a sequence, with the lines above the sequence moved to its
-- first item.
items :: S.Node -> [S.Node] -> [S.Node]
items n = \case
  x : xs | ls@(_ : _) <- linesAbove n -> withLinesAbove ls x : xs
  xs -> xs
-- If GHC inlines this function into 'withSequence', 'withSequence' becomes
-- too large to inline. A derived decoder then keeps the code after its type
-- error, and the inspection test of the derived decoder of a sum type fails.
{-# NOINLINE items #-}

-- | The lines above a collection, and the comment on its first line as a line
-- too, e.g. after its tag. They go above its first item or key.
linesAbove :: S.Node -> [S.Line]
linesAbove n = n.comments.before ++ [S.Comment c | Just c <- [n.comments.inline]]

-- | The node with the lines above it after the given ones.
withLinesAbove :: [S.Line] -> S.Node -> S.Node
withLinesAbove ls n =
  let c = n.comments
  in S.Node n.offset n.endOffset n.props c {S.before = ls ++ c.before} n.content

-- | The node without its comments.
withoutComments :: S.Node -> S.Node
withoutComments n = S.Node n.offset n.endOffset n.props S.noComments n.content

-- | The entries of a mapping. As for 'withText', the tag of a string key does
-- not matter, so two string keys with the same text are an error, e.g. @a@
-- and @!foo a@.
--
-- The lines above the mapping go to its first key. The comment on the first
-- line of the mapping, e.g. after its tag, goes there too as a line.
--
-- The parser gives a mapping the lines up to the last empty line above its
-- first key, e.g. a comment at the top of a file. A record has no place for
-- these lines, but 'Yamlet.Commented' on its first field keeps them.
withMapping :: (Object -> Parser a) -> S.Node -> Parser a
withMapping f = parseNode $ \n -> case n.content of
  S.Mapping _ kvs -> mkObject n (keyEntries n kvs) >>= f
  _ -> typeMismatch "a mapping" n

-- | The entries of a mapping, with the lines above the mapping moved to its
-- first key. The renderer writes both at the same place.
keyEntries :: S.Node -> [(S.Node, S.Node)] -> [(S.Node, S.Node)]
keyEntries n = \case
  (k, v) : rest | ls@(_ : _) <- linesAbove n -> (withLinesAbove ls k, v) : rest
  kvs -> kvs

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
      , otherKeys = [(k, v) | (k@S.Node {S.content = S.Scalar style t}, _) <- kvs, let v = scalarValue k.props.tag style t, case v of String _ -> False; _ -> True]
      }
  where
    insert :: M.Map T.Text (S.Node, S.Node) -> (S.Node, S.Node) -> Parser (M.Map T.Text (S.Node, S.Node))
    insert m kv@(k, _) = case stringValue k of
      Just t -> case M.insertLookupWithKey (\_ _ old -> old) t kv m of
        (Just (first, _), _) ->
          Parser $ \_ -> Result (OneError k.offset ("duplicate key " ++ show t) [(first.offset, "the first key " ++ show t)]) failed
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
o .: key = case M.lookup key o.index of
  Just entry -> parseEntry entry
  Nothing -> missingKey o key

-- | The value of a key, or 'Nothing' if the key is missing or its value is
-- null.
(.:?) :: FromYaml a => Object -> T.Text -> Parser (Maybe a)
o .:? key =
  findKey o key >>= \case
    Just (_, v) | isNullNode v -> pure Nothing
    entry -> traverse parseEntry entry

-- | The value of a key, or 'Nothing' if the key is missing. Unlike '.:?', a
-- null value goes to the parser of the value, e.g. @'Maybe' a@ gives
-- @'Just' 'Nothing'@ for a null value.
--
-- >>> :{
-- newtype Limit = Limit (Maybe (Maybe Int))
--   deriving stock (Show)
-- instance FromYaml Limit where
--   parseYaml = withMapping $ \o -> Limit <$> o .:! "limit"
-- :}
--
-- >>> decodeText @Limit "limit: null\n"
-- Right (Limit (Just Nothing))
--
-- >>> decodeText @Limit "{}"
-- Right (Limit Nothing)
(.:!) :: FromYaml a => Object -> T.Text -> Parser (Maybe a)
o .:! key = findKey o key >>= traverse parseEntry

-- | The value of an entry, with errors that point to the value.
parseEntry :: FromYaml a => (S.Node, S.Node) -> Parser a
parseEntry (k, v) = parseNode (parseYamlField k) v

-- | The entry of a string key, or 'Nothing' if the key is missing. A key with
-- the same text that is not a string, e.g. 404, is an error, so that its
-- value does not go away.
findKey :: Object -> T.Text -> Parser (Maybe (S.Node, S.Node))
findKey o key = case M.lookup key o.index of
  Just entry -> pure (Just entry)
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
  let Parser g = findKey o key
  in case g off of
       Result NoErrors _
         | M.member "<<" o.index -> failure o.node.offset ("missing key " ++ show key ++ ", " ++ noMergeKeys)
         | otherwise -> failure o.node.offset ("missing key " ++ show key)
       Result e _ -> Result e failed

-- | A default for an optional value.
(.!=) :: Parser (Maybe a) -> a -> Parser a
p .!= def = fromMaybe def <$> p

infixl 9 .:, .:?, .:!
infixl 8 .!=

-- | Fail at each key that is not in the list. If a key in the list is close
-- to an unknown key, e.g. "host" to "hots", its error suggests it. Otherwise
-- the first such error of the mapping lists the known keys, and the others
-- do not repeat the list.
rejectUnknownKeys :: [T.Text] -> Object -> Parser ()
rejectUnknownKeys known o = go True o.entries
  where
    -- The flag tells if no error listed the known keys yet.
    go :: Bool -> [(S.Node, S.Node)] -> Parser ()
    go unlisted = \case
      [] -> pure ()
      (k, _) : rest -> case stringValue k of
        Just t
          | t `elem` known -> go unlisted rest
          | t == "<<" -> unknown k t (", " ++ noMergeKeys) *> go unlisted rest
          | Just s <- closeName known t -> unknown k t (didYouMean s) *> go unlisted rest
          | unlisted -> unknown k t (expectedOneOf known) *> go False rest
          | otherwise -> unknown k t "" *> go False rest
        _ -> typeMismatch "a string as the key" k *> go unlisted rest

    unknown :: S.Node -> T.Text -> String -> Parser ()
    unknown k t hint = failAt k $ "unknown key " ++ show t ++ hint

-- | The end of the error for an unknown name: the known name that is close to
-- it, or else all known names.
alternatives :: [T.Text] -> T.Text -> String
alternatives known t = maybe (expectedOneOf known) didYouMean (closeName known t)

didYouMean :: T.Text -> String
didYouMean s = ", did you mean " ++ show s ++ "?"

expectedOneOf :: [T.Text] -> String
expectedOneOf known = ", expected one of: " ++ L.intercalate ", " (map T.unpack known)

-- | The known name that is close to the name, e.g. "host" for "hots".
closeName :: [T.Text] -> T.Text -> Maybe T.Text
closeName known t = suggestion (T.unpack t)
  where
    suggestion :: String -> Maybe T.Text
    suggestion u =
      case L.sortOn fst [(d, s) | s <- known, let d = distance u (T.unpack s), d <= maxEdits, d < length u] of
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
--
-- An instance for a record reads a mapping with 'withMapping':
--
-- >>> :{
-- data Server = Server {host :: T.Text, port :: Int, tags :: [T.Text]}
--   deriving stock (Show)
-- instance FromYaml Server where
--   parseYaml = withMapping $ \o ->
--     rejectUnknownKeys ["host", "port", "tags"] o
--       *> (Server <$> o .: "host" <*> o .:? "port" .!= 80 <*> o .:? "tags" .!= [])
-- :}
--
-- >>> decodeText @Server "host: example.com\ntags:\n- web\n"
-- Right (Server {host = "example.com", port = 80, tags = ["web"]})
--
-- The decoder reports the errors of all fields together:
--
-- >>> either (mapM_ (putStrLn . prettyError "server.yaml")) print (decodeText @Server "hots: example.com\nport: http\n")
-- server.yaml:1:1: unknown key "hots", did you mean "host"?
--   |
-- 1 | hots: example.com
--   | ^
-- server.yaml:1:1: missing key "host"
--   |
-- 1 | hots: example.com
--   | ^
-- server.yaml:2:7: port: expected an integer, but got a string
--   |
-- 2 | port: http
--   |       ^
class FromYaml a where
  parseYaml :: S.Node -> Parser a
  default parseYaml
    :: ( Generic a
       , GenericYaml a
       , Rep a ~ D1 d f
       , GConstructors f
       , GEncoding (SumEncoding a) f
       , GFromConstructor f
       )
    => S.Node -> Parser a
  parseYaml = genericParseYaml

  -- | Parse a list. The instance for 'Char' parses a string instead.
  parseYamlList :: S.Node -> Parser [a]
  parseYamlList = withSequence (mapM (parseNode parseYaml))

  -- | Parse the value of a mapping entry, with its key, e.g. to keep the
  -- comments of the key as 'Yamlet.Commented' does. '.:', the derived decoders
  -- and the instances for maps use it. The default ignores the key.
  parseYamlField :: S.Node -> S.Node -> Parser a
  parseYamlField _ = parseYaml

-- | The node of the syntax tree, with its styles and comments, e.g. to write
-- a part of a document back as it was written. An alias in the input gives a
-- copy of the node that it refers to.
--
-- The texts of the node are copies, so that a small part of a document does
-- not keep the whole input alive. For a whole document without a copy, use
-- 'Yamlet.Syntax.parseDocuments'.
instance FromYaml S.Node where
  parseYaml = pure . S.copyNode

-- | The value with the comments of its entry, or of its node if it has no key,
-- copied like every decoded text.
instance FromYaml a => FromYaml (S.Commented a) where
  parseYaml v =
    flip S.Commented (S.copyComments v.comments)
      <$> parseYaml (withoutComments v)
  parseYamlField k v = flip S.Commented (S.copyComments c) <$> parseYaml v'
    where
      c :: S.Comments
      v' :: S.Node
      (c, v') = entryComments k v

-- | The value with the offset of its node. The key of an entry goes to the
-- value inside, e.g. for a 'Yamlet.Commented' value.
instance FromYaml a => FromYaml (S.Located a) where
  parseYaml n = flip S.Located n.offset <$> parseYaml n
  parseYamlField k n = flip S.Located n.offset <$> parseYamlField k n

-- | The comments of a mapping entry, and the value without them. The lines
-- above a value on the line of its key or in the flow style go above the
-- entry, as the renderer writes them. The lines above the first entry of a
-- block collection stay in the value, because the renderer writes them below
-- the key.
entryComments :: S.Node -> S.Node -> (S.Comments, S.Node)
entryComments k v = (S.Comments before inline v.comments.after, value)
  where
    block :: Bool
    block = case v.content of
      S.Sequence S.Block (_ : _) -> True
      S.Mapping S.Block (_ : _) -> True
      _ -> False

    above :: [S.Line]
    above = k.comments.before ++ if block then [] else v.comments.before

    -- A line has one comment at its end. With an explicit key, both nodes can
    -- have one, and the renderer writes the comment of the key above.
    before :: [S.Line]
    inline :: Maybe T.Text
    (before, inline) = case (k.comments.inline, v.comments.inline) of
      (Just kc, Just vc) -> (above ++ [S.Comment kc], Just vc)
      (kc, vc) -> (above, vc <|> kc)

    value :: S.Node
    value =
      let rest = S.Comments (if block then v.comments.before else []) Nothing []
      in S.Node v.offset v.endOffset v.props rest v.content

-- | The value of the node, with the tags resolved and the aliases replaced.
instance FromYaml Value where
  parseYaml n = case represent n of
    Right r -> pure r
    Left ((off, msg) NE.:| notes) -> Parser $ \_ -> Result (OneError off msg notes) failed

-- | An empty list, as a tuple without elements.
instance FromYaml () where
  parseYaml = parseNode $ \n -> case view n of
    SequenceView [] -> pure ()
    _ -> typeMismatch "an empty list" n

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
  parseYaml = withMapping $ \o ->
    rejectUnknownKeys ["months", "days"] o
      *> (CalendarDiffDays <$> o .: "months" <*> o .: "days")

-- | A mapping with the keys @months@ and @time@, a number of seconds, e.g.
-- @{months: 1, time: 1.5}@.
instance FromYaml CalendarDiffTime where
  parseYaml = withMapping $ \o ->
    rejectUnknownKeys ["months", "time"] o
      *> (CalendarDiffTime <$> o .: "months" <*> o .: "time")

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
    FloatView v -> pure (floatValueToFloat v)
    IntView i -> pure (fromInteger i)
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

-- | Null is 'Nothing'. The key of an entry goes to the value inside, e.g. for
-- a 'Yamlet.Commented' value.
--
-- >>> decodeText @[Maybe Int] "- 1\n- null\n- ~\n-\n"
-- Right [Just 1,Nothing,Nothing,Nothing]
instance FromYaml a => FromYaml (Maybe a) where
  parseYaml n = case view n of
    NullView -> pure Nothing
    _ -> Just <$> parseYaml n
  parseYamlField k n = case view n of
    NullView -> pure Nothing
    _ -> Just <$> parseYamlField k n

-- | Two keys that convert to the same key, e.g. @1@ and @1.0@ for 'Double',
-- are an error.
--
-- Each key decodes with the instance of its type, so a map with
-- t'Data.Text.Text' keys rejects a key such as @404@ or @true@, because YAML
-- reads it as an integer or a boolean. Quote such a key in the input, e.g.
-- @\"404\": not found@, or use a key type that matches it, e.g. t'Int'.
--
-- >>> decodeText @(M.Map Int T.Text) "404: not found\n"
-- Right (fromList [(404,"not found")])
--
-- >>> either (mapM_ (putStrLn . prettyError "input.yaml")) print (decodeText @(M.Map T.Text T.Text) "404: not found\n")
-- input.yaml:1:1: expected a string, but got an integer, quote the value, e.g. '404'
--   |
-- 1 | 404: not found
--   | ^
instance (Ord k, FromYaml k, FromYaml v) => FromYaml (M.Map k v) where
  -- The index of 'withMapping' would be of no use here.
  parseYaml = parseNode $ \n -> case n.content of
    S.Mapping _ kvs ->
      insertUnique fst mapEntry fst (\(k, v) -> M.alterF (\old -> (isJust old, old <|> Just v)) k) M.empty "duplicate key after conversion" "the first key" (keyEntries n kvs)
    _ -> typeMismatch "a mapping" n

-- | Two keys that convert to the same key are an error.
instance FromYaml v => FromYaml (IM.IntMap v) where
  parseYaml = parseNode $ \n -> case n.content of
    S.Mapping _ kvs ->
      insertUnique fst mapEntry fst (\(k, v) -> IM.alterF (\old -> (isJust old, old <|> Just v)) k) IM.empty "duplicate key after conversion" "the first key" (keyEntries n kvs)
    _ -> typeMismatch "a mapping" n

-- | A list. Two elements that convert to the same value, e.g. @1@ and @1.0@
-- for 'Double', are an error.
instance (Ord a, FromYaml a) => FromYaml (Set.Set a) where
  parseYaml =
    withSequence $
      insertUnique id (parseNode parseYaml) id (Set.alterF (,True)) Set.empty "duplicate element after conversion" "the first element"

-- | A list. Two equal elements are an error.
instance FromYaml IS.IntSet where
  parseYaml =
    withSequence $
      insertUnique id (parseNode parseYaml) id (IS.alterF (,True)) IS.empty "duplicate element" "the first element"

-- | The key and the value of a map entry.
mapEntry :: (FromYaml k, FromYaml v) => (S.Node, S.Node) -> Parser (k, v)
mapEntry (k, v) = (,) <$> parseNode parseYaml k <*> parseEntry (k, v)

-- | Decode the items and insert them in their order, with the errors of all
-- items. Each item that is already there is an error at its node, with the
-- note at the first equal item. The insert tells if the item was there, and
-- the key tells which items are equal.
insertUnique
  :: forall a x s c
   . Ord c
  => (a -> S.Node) -> (a -> Parser x) -> (x -> c) -> (x -> s -> (Bool, s)) -> s -> String -> String -> [a] -> Parser s
insertUnique node item key insert start msg note xs = Parser $ \off -> go off start NoErrors [] xs
  where
    -- The duplicates are in reverse.
    go :: S.Offset -> s -> Errors -> [(c, S.Node)] -> [a] -> Result s
    go off !acc errs dups = \case
      [] -> case (errs, dups) of
        (NoErrors, []) -> Result NoErrors acc
        _ -> Result (L.foldl' bothErrors errs (map (duplicateError (firsts off)) dups)) failed
      a : rest ->
        let Parser p = item a
        in case p off of
             Result NoErrors x -> case insert x acc of
               (False, acc') -> go off acc' errs dups rest
               (True, acc') -> go off acc' errs ((key x, node a) : dups) rest
             Result e _ -> go off acc (bothErrors errs e) dups rest

    duplicateError :: M.Map c S.Node -> (c, S.Node) -> Errors
    duplicateError fs (c, n) = OneError n.offset msg [(first.offset, note) | Just first <- [M.lookup c fs]]

    -- A second pass finds the first items, only if there are duplicates.
    firsts :: S.Offset -> M.Map c S.Node
    firsts off =
      M.fromListWith
        (\_ old -> old)
        [(key x, node a) | a <- xs, let Parser p = item a, Result NoErrors x <- [p off]]

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

-- | Null.
instance FromYaml (Proxy a) where
  parseYaml = withNull (pure Proxy)

instance FromYaml Void where
  parseYaml _ = fail "the type Void has no values"

-- | A mapping with the keys @numerator@ and @denominator@, e.g.
-- @{numerator: 1, denominator: 3}@.
instance (Integral a, FromYaml a) => FromYaml (Ratio a) where
  parseYaml = withMapping $ \o -> do
    (n, d) <-
      rejectUnknownKeys ["numerator", "denominator"] o
        *> ((,) <$> (.:) @a o "numerator" <*> (.:) @a o "denominator")
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
--
-- >>> decodeText @(Either Int T.Text) "Left: 1\n"
-- Right (Left 1)
instance (FromYaml a, FromYaml b) => FromYaml (Either a b) where
  parseYaml = withMapping $ \o -> case objectEntries o of
    [(k, v)] -> case stringValue k of
      Just "Left" -> Left <$> parseEntry (k, v)
      Just "Right" -> Right <$> parseEntry (k, v)
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
     , GEncoding (SumEncoding a) f
     , GFromConstructor f
     )
  => S.Node -> Parser a
genericParseYaml n =
  -- Forcing the encoding forces the check of the shape, e.g. with deferred
  -- type errors in a test of the errors.
  let enc = gEncoding @(SumEncoding a) @f
  in enc `seq` gParseYaml (yamlOptions @a) enc (from <$> yamlDefault @a) to n
{-# INLINE genericParseYaml #-}

-- Each constructor applies 'to' to its own representation, e.g.
-- @to (M1 (L1 (M1 fields)))@, and the optimizer reduces this to the real
-- constructor in the same place. For this, the decoders of the constructors
-- take a continuation. It starts as 'to' and grows by 'M1', 'L1' or 'R1' at
-- each level of the sum.
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
  => YamlOptions -> SumEncodingKind -> Maybe (D1 d f p) -> (D1 d f p -> a) -> S.Node -> Parser a
gParseYaml opts enc def k n
  | gNullary @f =
      withName tags (\t -> fromMaybe (unknown n "value" t) (gFromTag opts (k . M1) n t)) n
  | isTagged @f opts, enc == SingleField = single
  | isTagged @f opts = withMapping tagged n
  | otherwise = gFromUntagged opts (unM1 <$> def) (k . M1) n
  where
    tagged :: Object -> Parser a
    tagged o = case lookupKey opts.tagKey o of
      Nothing -> missingKey o opts.tagKey
      Just tn -> do
        t <- withName tags pure tn
        fromMaybe (unknown tn "tag" t) (gFromTagged opts (enc == TaggedFlat) (unM1 <$> def) (k . M1) t o)

    -- A constructor without fields is its tag, and another constructor is a
    -- mapping with its tag as the only key.
    single :: Parser a
    single = case view n of
      StringView t -> fromMaybe (withoutValue t) (gFromTag opts (k . M1) n t)
      _ | S.Mapping {} <- n.content -> withMapping singleEntry n
      _ -> typeMismatch "a string or a mapping with one key" n

    singleEntry :: Object -> Parser a
    singleEntry o = case objectEntries o of
      [(kn, v)] -> do
        t <- withName tags pure kn
        fromMaybe (unknown kn "constructor" t) (gFromSingle opts (unM1 <$> def) (k . M1) t (kn, v))
      _ : (kn, _) : _ -> failAt kn "expected a mapping with one key, but got a second key"
      [] -> failAt n "expected a mapping with one key, but got an empty mapping"

    -- A string that is the tag of a constructor with fields.
    withoutValue :: T.Text -> Parser a
    withoutValue t
      | t `elem` tags = failAt n $ "expected a mapping with the key " ++ show t ++ ", because the constructor has fields"
      | otherwise = unknown n "constructor" t

    unknown :: S.Node -> String -> T.Text -> Parser a
    unknown node what t = failAt node $ "unknown " ++ what ++ " " ++ show t ++ alternatives tags t

    tags :: [T.Text]
    tags = map (constructorTag opts) (gConstructorNames @f)
{-# INLINE gParseYaml #-}

class GFromConstructor f where
  -- | The constructor without fields with the tag, from the node of the tag.
  gFromTag :: YamlOptions -> (f p -> a) -> S.Node -> T.Text -> Maybe (Parser a)

  -- | The constructor with the tag, from the mapping that holds the tag, with
  -- the flag of 'TaggedFlat'.
  gFromTagged :: YamlOptions -> Bool -> Maybe (f p) -> (f p -> a) -> T.Text -> Object -> Maybe (Parser a)

  -- | The constructor with the tag, from the only entry of a mapping, for
  -- 'SingleField'.
  gFromSingle :: YamlOptions -> Maybe (f p) -> (f p -> a) -> T.Text -> (S.Node, S.Node) -> Maybe (Parser a)

  -- | The only constructor, without a tag.
  gFromUntagged :: YamlOptions -> Maybe (f p) -> (f p -> a) -> S.Node -> Parser a

instance GFromConstructor V1 where
  gFromTag _ _ _ _ = Nothing
  gFromTagged _ _ _ _ _ _ = Nothing
  gFromSingle _ _ _ _ _ = Nothing
  gFromUntagged _ _ _ _ = fail "expected a type with constructors"

instance (GFromConstructor f, GFromConstructor g) => GFromConstructor (f :+: g) where
  gFromTag opts k n t = gFromTag opts (k . L1) n t `mplus` gFromTag opts (k . R1) n t
  gFromTagged opts flat def k t o =
    gFromTagged opts flat (def >>= \case L1 x -> Just x; R1 _ -> Nothing) (k . L1) t o
      `mplus` gFromTagged opts flat (def >>= \case R1 x -> Just x; L1 _ -> Nothing) (k . R1) t o
  gFromSingle opts def k t entry =
    gFromSingle opts (def >>= \case L1 x -> Just x; R1 _ -> Nothing) (k . L1) t entry
      `mplus` gFromSingle opts (def >>= \case R1 x -> Just x; L1 _ -> Nothing) (k . R1) t entry

  -- A type with several constructors always has a tag.
  gFromUntagged _ _ _ _ = fail "expected a tag"
  {-# INLINE gFromTag #-}
  {-# INLINE gFromTagged #-}
  {-# INLINE gFromSingle #-}

instance
  ( KnownSymbol name
  , GFields f
  , GFromFields f
  )
  => GFromConstructor (C1 (MetaCons name fixity isRecord) f)
  where
  gFromTag opts k n t
    | t == tag && gArity @f == 0 = Just (k . M1 <$> gFromValue n)
    | otherwise = Nothing
    where
      tag :: T.Text
      tag = constructorTag opts (symbolVal (Proxy @name))

  gFromTagged opts flat def k t o
    | t == constructorTag opts (symbolVal (Proxy @name)) = Just (k . M1 <$> fromObject opts flat [opts.tagKey] (unM1 <$> def) o)
    | otherwise = Nothing

  gFromSingle opts def k t entry@(kn, v)
    | t /= constructorTag opts (symbolVal (Proxy @name)) = Nothing
    | gNamed @f = Just (withMapping (fmap (k . M1) . fromObject opts False [] (unM1 <$> def)) v)
    | gArity @f == 0 = Just (failAt kn $ "expected the string " ++ show t ++ ", because the constructor has no fields")
    | otherwise = Just (k . M1 <$> gFromEntry entry)

  gFromUntagged opts def k n
    | gNamed @f || gArity @f == 0 = withMapping (fmap (k . M1) . fromObject opts False [] (unM1 <$> def)) n
    | otherwise = k . M1 <$> gFromValue n

  {-# INLINE gFromTag #-}
  {-# INLINE gFromTagged #-}
  {-# INLINE gFromSingle #-}
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
  | gNamed @f || gArity @f == 0 = checked (gNames @f opts) (gFromObject opts def o)
  | flat && not (all (isKey opts.contentsKey . fst) others) = merged
  | otherwise = checked [opts.contentsKey] $ case M.lookup opts.contentsKey o.index of
      Just entry -> gFromEntry entry
      -- A missing contents key is null, if the fields accept null. A flat
      -- field can also have only optional keys.
      Nothing
        | Just fields <- def -> pure fields
        | flat -> maybe merged pure (succeeds gFromValue nullNode)
        | otherwise -> maybe (missingKey o opts.contentsKey) pure (succeeds gFromValue nullNode)
  where
    -- The fields, with the errors of the unknown keys if the options reject
    -- them.
    checked :: [T.Text] -> Parser (f p) -> Parser (f p)
    checked fields = (when opts.rejectUnknownFields (rejectUnknownKeys (keys ++ fields) o) *>)

    -- The field decodes from the mapping without the given keys. The first
    -- key already has the lines above the mapping.
    merged :: Parser (f p)
    merged =
      let n = objectNode o
          style = case n.content of
            S.Mapping s _ -> s
            _ -> S.Block
      in gFromValue (S.Node n.offset n.endOffset n.props S.noComments (S.Mapping style others))

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
    {-# INLINE merged #-}
{-# INLINE fromObject #-}

-- The shape check allows named fields, no fields, or one field without a
-- name. The default methods are for the kind of fields that never calls
-- them.
class GFromFields f where
  -- | The fields from a mapping, with the given default for missing keys.
  gFromObject :: YamlOptions -> Maybe (f p) -> Object -> Parser (f p)
  gFromObject _ _ o = fail $ "expected a field without a name in " ++ describeNode (objectNode o)

  -- | The only field without a name from its value.
  gFromValue :: S.Node -> Parser (f p)
  gFromValue n = fail $ "expected named fields in " ++ describeNode n

  -- | The only field from a mapping entry, with the key, e.g. for the
  -- comments of a 'Yamlet.Commented' field under the contents key.
  gFromEntry :: (S.Node, S.Node) -> Parser (f p)
  gFromEntry (_, v) = gFromValue v

-- The value of a constructor without fields is its tag.
instance GFromFields U1 where
  gFromObject _ _ _ = pure U1
  gFromValue _ = pure U1

instance (GFromFields f, GFromFields g) => GFromFields (f :*: g) where
  gFromObject opts def o =
    (:*:)
      <$> gFromObject opts ((\(a :*: _) -> a) <$> def) o
      <*> gFromObject opts ((\(_ :*: b) -> b) <$> def) o
  {-# INLINE gFromObject #-}

instance
  ( KnownSymbol name
  , FromYaml a
  )
  => GFromFields (S1 (MetaSel (Just name) u s d) (Rec0 a))
  where
  gFromObject opts def o =
    M1 . K1 <$> case M.lookup key o.index of
      Just entry -> parseEntry entry
      Nothing -> case def of
        Just (M1 (K1 x)) -> x <$ findKey o key
        -- A missing field is null, if its type accepts null.
        Nothing -> maybe (missingKey o key) (<$ findKey o key) (succeeds parseYaml nullNode)
    where
      key :: T.Text
      key = fieldKey @name opts
  {-# INLINE gFromObject #-}

instance FromYaml a => GFromFields (S1 (MetaSel Nothing u s d) (Rec0 a)) where
  gFromValue n = M1 . K1 <$> parseNode parseYaml n
  gFromEntry entry = M1 . K1 <$> parseEntry entry
  {-# INLINE gFromValue #-}

-- $setup
-- >>> import Yamlet
