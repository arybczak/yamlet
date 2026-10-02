{-# OPTIONS_HADDOCK not-home #-}

-- | The class t'FromYaml', its instances and the parts of the decoder that
-- the generic instances share with it. "Yamlet.Decode" exports the public
-- parts.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.FromYaml
  ( -- * Class
    FromYaml (..)

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
  , withName
  , oneOf

    -- * Collections
  , withSequence
  , withMapping
  , Object (..)
  , objectNode
  , objectEntries
  , objectKeys
  , lookupKey
  , parseField
  , parseFieldMaybe
  , parseFieldIfPresent
  , parseFieldDefault
  , parseFieldWith
  , parseFieldMaybeWith
  , parseFieldIfPresentWith
  , parseFieldDefaultWith
  , rejectUnknownKeys

    -- * Parts of the generic instances
  , parseItems
  , parseEntry
  , findKey
  , missingKey
  , closeName
  , unknownName
  , succeeds
  , nullNode
  ) where

import Control.Applicative
import Control.Monad
import Data.Containers.ListUtils
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
import GHC.Real
import Numeric.Natural

import Yamlet.Internal.Compose
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
--   *> (Config \<$> parseField o \"name\" \<*> parseField o \"paths\")
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
-- it and the message. The errors are in the order of the offsets, and equal
-- errors come only once. A note on an error comes right after it, e.g. the
-- first key of a duplicate key.
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
    -- The errors in the order of their offsets, each with its notes after it.
    -- Errors at the same offset keep their order. An error comes only once:
    -- the nodes inside an alias have the offset of the alias, so the same
    -- error in several of them repeats at that offset.
    sortedErrors :: Errors -> [(S.Offset, String)]
    sortedErrors = concatMap (\(off, msg, notes) -> (off, msg) : notes) . nubOrd . L.sortOn (\(off, _, _) -> off) . flip go []
      where
        go :: Errors -> [(S.Offset, String, [(S.Offset, String)])] -> [(S.Offset, String, [(S.Offset, String)])]
        go = \case
          NoErrors -> id
          OneError off msg notes -> ((off, msg, notes) :)
          BothErrors e1 e2 -> go e1 . go e2

    -- An error at the value of a key << that is a collection, e.g. an alias of
    -- a mapping, is likely from a merge key of YAML 1.1.
    withMergeHint :: Set.Set S.Offset -> (S.Offset, String) -> (S.Offset, String)
    withMergeHint offs (off, msg)
      | off `Set.member` offs = (off, msg ++ noMergeKeys)
      | otherwise = (off, msg)

    mergeValues :: S.Node -> Set.Set S.Offset
    mergeValues n = case n.content of
      S.SequenceContent _ xs -> foldMap mergeValues xs
      S.MappingContent _ kvs -> foldMap (\(k, v) -> mergeValue k v <> mergeValues k <> mergeValues v) kvs
      _ -> Set.empty

    mergeValue :: S.Node -> S.Node -> Set.Set S.Offset
    mergeValue k v = case (stringValue k, v.content) of
      (Just "<<", S.MappingContent {}) -> Set.singleton v.offset
      (Just "<<", S.SequenceContent {}) -> Set.singleton v.offset
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
nullNode = S.Node S.noOffset S.noOffset S.noProps S.noComments (S.ScalarContent S.Plain "")

-- | Run the second parser if the first one fails. A port can be a number or
-- a name:
--
-- >>> :{
-- newtype Port = Port (Either Integer T.Text)
--   deriving stock (Show)
-- instance FromYaml Port where
--   parseYaml n =
--     Port <$> ((Left <$> withInt pure n) `orElse` (Right <$> withText pure n))
-- :}
--
-- >>> decodeText @Port "8080"
-- Right (Port (Left 8080))
--
-- >>> decodeText @Port "http"
-- Right (Port (Right "http"))
--
-- If both parsers fail, the result has only the errors of the second one:
--
-- >>> either printErrors print (decodeText @Port "[80]")
-- input.yaml:1:1: expected a string, but got a list
--   |
-- 1 | [80]
--   | ^
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
    | S.ScalarContent S.Plain _ <- n.content
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
-- A conversion to an exact type, e.g. with 'truncate', is safe for untrusted
-- input. An integer has the digits of its text, and the decoder rejects a
-- float whose exponent in scientific notation is beyond the range from -1000
-- to 1000, also in a node that a program built.
withScientific :: (Sci.Scientific -> Parser a) -> S.Node -> Parser a
withScientific f = parseNode $ \n -> case view n of
  FloatView (Finite s) -> f s
  FloatView NegativeZero -> f 0
  IntView i -> f (Sci.scientific i 0)
  FloatView _ -> fail "expected a finite number"
  _ -> typeMismatch "a number" n

-- | The text of a string. The text is a copy, so it does not keep the input
-- alive. For a plain scalar that YAML reads as a number or a boolean, e.g.
-- @3.10@, the error suggests quotes.
withText :: (T.Text -> Parser a) -> S.Node -> Parser a
withText f = parseNode $ \n -> case view n of
  StringView t -> f $! T.copy t
  _ -> failAt n (stringMismatch n)

-- | A string that is one of the names, e.g. the tags of the constructors. For
-- another node, the error suggests quotes only if the quoted text is a name,
-- e.g. not for @null@. Otherwise it lists the names.
withName :: [T.Text] -> (T.Text -> Parser a) -> S.Node -> Parser a
withName names f = parseNode $ \n -> case (view n, n.content) of
  (StringView t, _) -> f t
  (_, S.ScalarContent S.Plain t) | S.NoTag <- n.props.tag, t `elem` names -> failAt n (stringMismatch n)
  _ -> typeMismatch ("one of: " ++ L.intercalate ", " (map T.unpack names)) n

-- | The value that goes with the string in the list of pairs, e.g. for names
-- that the program knows only at run time. An empty list rejects every value.
-- The errors are the same as for the constructors of an enumeration. An
-- unknown name gets the closest name or the list of names:
--
-- >>> :{
-- newtype Size = Size Int
--   deriving stock (Show)
-- instance FromYaml Size where
--   parseYaml = oneOf [("small", Size 1), ("large", Size 2)]
-- :}
--
-- >>> decodeText @Size "large"
-- Right (Size 2)
--
-- >>> either printErrors print (decodeText @Size "lage")
-- input.yaml:1:1: unknown value "lage", did you mean "large"?
--   |
-- 1 | lage
--   | ^
oneOf :: [(T.Text, a)] -> S.Node -> Parser a
oneOf choices n = withName names (\t -> maybe (unknownName "value" names n t) pure (lookup t choices)) n
  where
    names :: [T.Text]
    names = map fst choices

-- | The message for a node that is not a string, with the hint to quote a
-- plain number, boolean or written null.
stringMismatch :: S.Node -> String
stringMismatch n = mismatchMessage "a string" n ++ hint
  where
    hint :: String
    hint = case n.content of
      S.ScalarContent S.Plain t
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

-- | The items of a sequence. As for 'withMapping', the comments of the
-- sequence stay with it, not with its first item.
withSequence :: ([S.Node] -> Parser a) -> S.Node -> Parser a
withSequence f = parseNode $ \n -> case n.content of
  S.SequenceContent _ xs -> f xs
  _ -> typeMismatch "a list" n

-- | The values of the items, with the errors of all items, as with 'mapM'.
-- Unlike 'mapM', the stack does not grow with the number of items, because
-- the errors and the values are in accumulators until the end.
parseItems :: forall a. (S.Node -> Parser a) -> [S.Node] -> Parser [a]
parseItems p xs0 = Parser $ \off -> go off NoErrors [] xs0
  where
    go :: S.Offset -> Errors -> [a] -> [S.Node] -> Result [a]
    go off !errs acc = \case
      [] -> case errs of
        NoErrors -> Result NoErrors (reverse acc)
        _ -> Result errs failed
      x : xs ->
        let Parser g = parseNode p x
        in case g off of
             Result e a -> go off (bothErrors errs e) (a : acc) xs

-- | The entries of a mapping. As for 'withText', the tag of a string key does
-- not matter, so two string keys with the same text are an error, e.g. @a@
-- and @!foo a@.
--
-- The comments of the mapping stay with it, not with its first key. The parser
-- gives a mapping the lines up to the last empty line above its first key,
-- e.g. a comment at the top of a file, and the comment on its first line, e.g.
-- after its tag. A record has no place for them.
withMapping :: (Object -> Parser a) -> S.Node -> Parser a
withMapping f = parseNode $ \n -> case n.content of
  S.MappingContent _ kvs -> case mkObject n kvs of
    (NoErrors, o) -> f o
    -- The errors of the fields come with the duplicate keys, and a field
    -- reads the value of the first key.
    (errs, o) ->
      let Parser g = f o
      in Parser $ \off -> case g off of
           Result e _ -> Result (bothErrors errs e) failed
  _ -> typeMismatch "a mapping" n
  where
    -- The object and the errors of its duplicate keys. The index has the
    -- first of equal keys.
    --
    -- A list with linear lookups is faster only for a few keys, and it saves
    -- little of the time to decode a typical record.
    mkObject :: S.Node -> [(S.Node, S.Node)] -> (Errors, Object)
    mkObject n kvs =
      let (index, errs) = L.foldl' insert (M.empty, NoErrors) kvs
      in ( errs
         , Object
             { node = n
             , entries = kvs
             , index = index
             , otherKeys = [(k, v) | (k@S.Node {S.content = S.ScalarContent style t}, _) <- kvs, let v = scalarValue k.props.tag style t, case v of String _ -> False; _ -> True]
             , duplicates = case errs of
                 NoErrors -> False
                 _ -> True
             }
         )
      where
        insert
          :: (M.Map T.Text (S.Node, S.Node), Errors)
          -> (S.Node, S.Node)
          -> (M.Map T.Text (S.Node, S.Node), Errors)
        insert (!m, !errs) kv@(k, _) = case stringValue k of
          Just t -> case M.insertLookupWithKey (\_ _ old -> old) t kv m of
            (Just (first, _), _) ->
              (m, bothErrors errs (OneError k.offset ("duplicate key " ++ show t) [(first.offset, "the first key " ++ show t)]))
            (Nothing, m') -> (m', errs)
          _ -> (m, errs)

-- | A mapping with fast access to the values of string keys.
data Object = Object
  { node :: !S.Node
  , entries :: [(S.Node, S.Node)]
  , index :: M.Map T.Text (S.Node, S.Node)
  , otherKeys :: [(S.Node, Value)]
  -- ^ The keys that are not strings, for the error of a lookup.
  , duplicates :: !Bool
  -- ^ Two string keys have the same text.
  }

-- | The node of the mapping.
objectNode :: Object -> S.Node
objectNode o = o.node

-- | The entries of the mapping in the order of the input.
objectEntries :: Object -> [(S.Node, S.Node)]
objectEntries o = o.entries

-- | The string keys of the mapping in the order of the input.
objectKeys :: Object -> [T.Text]
objectKeys o = [c | (k, _) <- o.entries, Just t <- [stringValue k], let !c = T.copy t]

-- | The value of a string key.
lookupKey :: T.Text -> Object -> Maybe S.Node
lookupKey key o = snd <$> M.lookup key o.index

-- | The value of a key. It is an error if the key is missing.
parseField :: FromYaml a => Object -> T.Text -> Parser a
parseField = entryField parseEntry

-- | The value of a key, or 'Nothing' if the key is missing or its value is
-- null.
parseFieldMaybe :: FromYaml a => Object -> T.Text -> Parser (Maybe a)
parseFieldMaybe = entryFieldMaybe parseEntry

-- | The value of a key, or 'Nothing' if the key is missing. Unlike
-- 'parseFieldMaybe', a null value goes to the parser of the value, e.g.
-- @'Maybe' a@ gives @'Just' 'Nothing'@ for a null value.
--
-- >>> :{
-- newtype Limit = Limit (Maybe (Maybe Int))
--   deriving stock (Show)
-- instance FromYaml Limit where
--   parseYaml = withMapping $ \o -> Limit <$> parseFieldIfPresent o "limit"
-- :}
--
-- >>> decodeText @Limit "limit: null\n"
-- Right (Limit (Just Nothing))
--
-- >>> decodeText @Limit "{}"
-- Right (Limit Nothing)
parseFieldIfPresent :: FromYaml a => Object -> T.Text -> Parser (Maybe a)
parseFieldIfPresent = entryFieldIfPresent parseEntry

-- | The value of a key, or the default if the key is missing or its value is
-- null.
--
-- >>> :{
-- newtype Server = Server Int
--   deriving stock (Show)
-- instance FromYaml Server where
--   parseYaml = withMapping $ \o -> Server <$> parseFieldDefault o "port" 80
-- :}
--
-- >>> decodeText @Server "{}"
-- Right (Server 80)
--
-- >>> decodeText @Server "port: null\n"
-- Right (Server 80)
--
-- >>> decodeText @Server "port: 8080\n"
-- Right (Server 8080)
parseFieldDefault :: FromYaml a => Object -> T.Text -> a -> Parser a
parseFieldDefault o key def = fromMaybe def <$> parseFieldMaybe o key

-- | Like 'parseField', with the given parser for the value, e.g. to check a
-- value without a new type for it. The errors of the parser point to the
-- value. The parser gets only the value, so it cannot keep the comments of
-- the key, as a 'Yamlet.Commented' field does with 'parseField'.
--
-- >>> :{
-- newtype Port = Port Int
--   deriving stock (Show)
-- instance FromYaml Port where
--   parseYaml = withMapping $ \o -> Port <$> parseFieldWith number o "port"
--     where
--       number :: Node -> Parser Int
--       number = withInt $ \i ->
--         if i >= 1 && i <= 65535
--           then pure (fromInteger i)
--           else fail "expected a port from 1 to 65535"
-- :}
--
-- >>> decodeText @Port "port: 80\n"
-- Right (Port 80)
--
-- >>> either printErrors print (decodeText @Port "port: 70000\n")
-- input.yaml:1:7: port: expected a port from 1 to 65535
--   |
-- 1 | port: 70000
--   |       ^
parseFieldWith :: (S.Node -> Parser a) -> Object -> T.Text -> Parser a
parseFieldWith p = entryField (parseNode p . snd)

-- | Like 'parseFieldMaybe', with the given parser for the value, as in
-- 'parseFieldWith'. The result is 'Nothing' if the key is missing or its
-- value is null. The parser never gets a null value, so a missing key and a
-- null value mean the same.
--
-- >>> :{
-- newtype Job = Job (Maybe Int)
--   deriving stock (Show)
-- instance FromYaml Job where
--   parseYaml = withMapping $ \o -> Job <$> parseFieldMaybeWith positive o "retries"
--     where
--       positive :: Node -> Parser Int
--       positive = withInt $ \i ->
--         if i > 0 then pure (fromInteger i) else fail "expected a positive number"
-- :}
--
-- >>> decodeText @Job "{}"
-- Right (Job Nothing)
--
-- >>> decodeText @Job "retries: null\n"
-- Right (Job Nothing)
--
-- >>> decodeText @Job "retries: 3\n"
-- Right (Job (Just 3))
--
-- To give a null value to the parser, use 'parseFieldIfPresentWith'.
parseFieldMaybeWith :: (S.Node -> Parser a) -> Object -> T.Text -> Parser (Maybe a)
parseFieldMaybeWith p = entryFieldMaybe (parseNode p . snd)

-- | Like 'parseFieldIfPresent', with the given parser for the value, as in
-- 'parseFieldWith'. The result is 'Nothing' only if the key is missing.
-- A null value goes to the parser, so the parser can give it a meaning of
-- its own. Here a missing key takes the default limit, and null means no
-- limit:
--
-- >>> :{
-- data Limit = Unlimited | Limit Int
--   deriving stock (Show)
-- newtype Job = Job (Maybe Limit)
--   deriving stock (Show)
-- instance FromYaml Job where
--   parseYaml = withMapping $ \o -> Job <$> parseFieldIfPresentWith limit o "limit"
--     where
--       limit :: Node -> Parser Limit
--       limit n = case view n of
--         NullView -> pure Unlimited
--         _ -> withInt (pure . Limit . fromInteger) n
-- :}
--
-- >>> decodeText @Job "{}"
-- Right (Job Nothing)
--
-- >>> decodeText @Job "limit: null\n"
-- Right (Job (Just Unlimited))
--
-- >>> decodeText @Job "limit: 3\n"
-- Right (Job (Just (Limit 3)))
--
-- With 'parseFieldMaybeWith', the null value would give 'Nothing', the
-- same as the missing key.
parseFieldIfPresentWith :: (S.Node -> Parser a) -> Object -> T.Text -> Parser (Maybe a)
parseFieldIfPresentWith p = entryFieldIfPresent (parseNode p . snd)

-- | Like 'parseFieldDefault', with the given parser for the value, as in
-- 'parseFieldWith'. The parser never gets a null value.
--
-- >>> :{
-- newtype Job = Job Int
--   deriving stock (Show)
-- instance FromYaml Job where
--   parseYaml = withMapping $ \o -> Job <$> parseFieldDefaultWith positive o "retries" 1
--     where
--       positive :: Node -> Parser Int
--       positive = withInt $ \i ->
--         if i > 0 then pure (fromInteger i) else fail "expected a positive number"
-- :}
--
-- >>> decodeText @Job "{}"
-- Right (Job 1)
--
-- >>> decodeText @Job "retries: 3\n"
-- Right (Job 3)
parseFieldDefaultWith :: (S.Node -> Parser a) -> Object -> T.Text -> a -> Parser a
parseFieldDefaultWith p o key def = fromMaybe def <$> parseFieldMaybeWith p o key

-- | The value of a key, with the given parser for the entry.
entryField :: ((S.Node, S.Node) -> Parser a) -> Object -> T.Text -> Parser a
entryField p o key = case M.lookup key o.index of
  Just entry -> p entry
  Nothing -> missingKey o key

-- | The value of a key that can be missing or null, with the given parser
-- for the entry.
entryFieldMaybe :: ((S.Node, S.Node) -> Parser a) -> Object -> T.Text -> Parser (Maybe a)
entryFieldMaybe p o key =
  findKey o key >>= \case
    Just (_, v) | isNullNode v -> pure Nothing
    entry -> traverse p entry

-- | The value of a key that can be missing, with the given parser for the
-- entry.
entryFieldIfPresent :: ((S.Node, S.Node) -> Parser a) -> Object -> T.Text -> Parser (Maybe a)
entryFieldIfPresent p o key = findKey o key >>= traverse p

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
         | M.member "<<" o.index -> failure o.node.offset ("missing key " ++ show key ++ noMergeKeys)
         | otherwise -> failure o.node.offset ("missing key " ++ show key)
       Result e _ -> Result e failed

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
          | t == "<<" -> unknown k t noMergeKeys *> go unlisted rest
          | Just s <- closeName known t -> unknown k t (didYouMean s) *> go unlisted rest
          | unlisted -> unknown k t (expectedOneOf known) *> go False rest
          | otherwise -> unknown k t "" *> go False rest
        _ -> typeMismatch "a string as the key" k *> go unlisted rest

    unknown :: S.Node -> T.Text -> String -> Parser ()
    unknown k t hint = failAt k $ "unknown key " ++ show t ++ hint

-- | The error at the node for a name that is none of the known names, e.g.
-- an unknown value, with the known name that is close to it, or else all
-- known names.
unknownName :: String -> [T.Text] -> S.Node -> T.Text -> Parser a
unknownName what known n t =
  failAt n $ "unknown " ++ what ++ " " ++ show t ++ maybe (expectedOneOf known) didYouMean (closeName known t)

didYouMean :: T.Text -> String
didYouMean s = ", did you mean " ++ show s ++ "?"

expectedOneOf :: [T.Text] -> String
expectedOneOf known = ", expected one of: " ++ L.intercalate ", " (map T.unpack known)

-- | The known name that is close to the name, e.g. "host" for "hots".
closeName :: [T.Text] -> T.Text -> Maybe T.Text
closeName known t =
  case L.sortOn fst [(d, s) | s <- known, abs (T.length s - n) <= maxEdits, let d = distance (T.unpack t) (T.unpack s), d <= maxEdits, d < n] of
    (_, s) : _ -> Just s
    [] -> Nothing
  where
    n :: Int
    n = T.length t

    -- A swap of two adjacent characters, e.g. "hots" for "host", takes two
    -- edits. The distance is at least the difference of the lengths, so a
    -- long input from an attacker needs no table of distances.
    maxEdits :: Int
    maxEdits = 2

    -- The Levenshtein distance: the number of characters to insert, delete
    -- or change. After i characters of xs, the row holds the distance from
    -- them to each prefix of ys.
    distance :: String -> String -> Int
    distance xs ys = case reverse (L.foldl' nextRow [0 .. length ys] (zip [1 ..] xs)) of
      d : _ -> d
      [] -> length ys
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

-- | Types that can be parsed from a node. A type with a
-- t'GHC.Generics.Generic' instance can derive the instance via
-- t'Yamlet.Generic.GenericYaml'.
--
-- An instance for a record reads a mapping with 'withMapping':
--
-- >>> :{
-- data Server = Server {host :: T.Text, port :: Int, tags :: [T.Text]}
--   deriving stock (Show)
-- instance FromYaml Server where
--   parseYaml = withMapping $ \o ->
--     rejectUnknownKeys ["host", "port", "tags"] o
--       *> ( Server
--              <$> parseField o "host"
--              <*> parseFieldDefault o "port" 80
--              <*> parseFieldDefault o "tags" []
--          )
-- :}
--
-- >>> decodeText @Server "host: example.com\ntags:\n- web\n"
-- Right (Server {host = "example.com", port = 80, tags = ["web"]})
--
-- The decoder reports the errors of all fields together:
--
-- >>> either printErrors print (decodeText @Server "hots: example.com\nport: http\n")
-- input.yaml:1:1: unknown key "hots", did you mean "host"?
--   |
-- 1 | hots: example.com
--   | ^
-- input.yaml:1:1: missing key "host"
--   |
-- 1 | hots: example.com
--   | ^
-- input.yaml:2:7: port: expected an integer, but got a string
--   |
-- 2 | port: http
--   |       ^
class FromYaml a where
  parseYaml :: S.Node -> Parser a

  -- | Parse a list. The instance for 'Char' parses a string instead.
  parseYamlList :: S.Node -> Parser [a]
  parseYamlList = withSequence (parseItems parseYaml)

  -- | Parse the value of a mapping entry, with its key, e.g. to keep the
  -- comments of the key as 'Yamlet.Commented' does. 'parseField' and the
  -- other lookups without a parser argument, the derived decoders and the
  -- instances for maps use it. The default ignores the key.
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
  parseYaml n = pure $! S.copyNode n

-- | The value with the comments of its entry, or of its node if it has no key,
-- copied like every decoded text.
instance FromYaml a => FromYaml (S.Commented a) where
  parseYaml v =
    flip S.Commented (S.copyComments v.comments)
      <$!> parseYaml (S.withComments S.noComments v)
  parseYamlField k v = flip S.Commented (S.copyComments c) <$!> parseYaml v'
    where
      c :: S.Comments
      v' :: S.Node
      (c, v') = entryComments k v

-- | The value with the offset of its node. The key of an entry goes to the
-- value inside, e.g. for a 'Yamlet.Commented' value.
instance FromYaml a => FromYaml (S.Located a) where
  parseYaml n = flip S.Located n.offset <$!> parseYaml n
  parseYamlField k n = flip S.Located n.offset <$!> parseYamlField k n

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
      S.SequenceContent S.Block (_ : _) -> True
      S.MappingContent S.Block (_ : _) -> True
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
      in S.withComments rest v

-- | The value of the node, with the tags resolved and the aliases replaced.
instance FromYaml Value where
  parseYaml n = case represent n of
    -- The value is built lazily. The copy visits a node once per alias of
    -- it, as the limit of 'represent' allows.
    Right r -> pure $! copy r
    Left ((off, msg) NE.:| notes) -> Parser $ \_ -> Result (OneError off msg notes) failed
    where
      -- The value in normal form, with copies of its texts.
      copy :: Value -> Value
      copy = \case
        String t -> String (T.copy t)
        Sequence xs -> Sequence $! strictMap copy xs
        Mapping kvs -> Mapping $! strictMap (\(k, v) -> let !k' = copy k; !v' = copy v in (k', v')) kvs
        Tagged tag v -> Tagged (T.copy tag) (copy v)
        v -> v

      -- The results are in reverse until the end, so that the stack does not
      -- grow with the length of the list.
      strictMap :: forall a b. (a -> b) -> [a] -> [b]
      strictMap f = go []
        where
          go :: [b] -> [a] -> [b]
          go acc = \case
            [] -> reverse acc
            x : xs -> let !y = f x in go (y : acc) xs

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

-- | Like t'ZonedTime', converted to UTC.
instance FromYaml UTCTime where
  parseYaml = withIso8601 zonedTimeMismatch parseUTCTime

-- | A number of seconds, rounded down to a picosecond.
instance FromYaml NominalDiffTime where
  parseYaml = withScientific $ pure . secondsToNominalDiffTime . MkFixed . picoseconds

-- | A number of seconds, rounded down to a picosecond.
instance FromYaml DiffTime where
  parseYaml = withScientific $ pure . picosecondsToDiffTime . picoseconds

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
      *> (CalendarDiffDays <$> parseField o "months" <*> parseField o "days")

-- | A mapping with the keys @months@ and @time@, a number of seconds, e.g.
-- @{months: 1, time: 1.5}@.
instance FromYaml CalendarDiffTime where
  parseYaml = withMapping $ \o ->
    rejectUnknownKeys ["months", "time"] o
      *> (CalendarDiffTime <$> parseField o "months" <*> parseField o "time")

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
    x : xs -> (NE.:|) <$> parseNode parseYaml x <*> parseItems parseYaml xs

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
-- >>> either printErrors print (decodeText @(M.Map T.Text T.Text) "404: not found\n")
-- input.yaml:1:1: expected a string, but got an integer, quote the value, e.g. '404'
--   |
-- 1 | 404: not found
--   | ^
instance (Ord k, FromYaml k, FromYaml v) => FromYaml (M.Map k v) where
  parseYaml = uniqueEntries M.alterF M.empty

-- | Two keys that convert to the same key are an error.
instance FromYaml v => FromYaml (IM.IntMap v) where
  parseYaml = uniqueEntries IM.alterF IM.empty

-- | A list. Two elements that convert to the same value, e.g. @1@ and @1.0@
-- for 'Double', are an error.
instance (Ord a, FromYaml a) => FromYaml (Set.Set a) where
  parseYaml =
    withSequence $
      insertUnique id (parseNode parseYaml) id (Set.alterF (,True)) Set.empty "duplicate element" "the first element"

-- | A list. Two elements that convert to the same value, e.g. @1@ and @0x1@,
-- are an error.
instance FromYaml IS.IntSet where
  parseYaml =
    withSequence $
      insertUnique id (parseNode parseYaml) id (IS.alterF (,True)) IS.empty "duplicate element" "the first element"

-- | A map from the entries of a mapping, with the alter function and the empty
-- map of its type. Two keys that convert to the same key are an error.
uniqueEntries
  :: (Ord k, FromYaml k, FromYaml v)
  => ((Maybe v -> (Bool, Maybe v)) -> k -> m -> (Bool, m)) -> m -> S.Node -> Parser m
uniqueEntries alter none = parseNode $ \n -> case n.content of
  -- The index of 'withMapping' would be of no use here.
  S.MappingContent _ kvs ->
    insertUnique fst entry fst (\(k, v) -> alter (\old -> (isJust old, old <|> Just v)) k) none "duplicate key after conversion" "the first key" kvs
  _ -> typeMismatch "a mapping" n
  where
    entry :: (FromYaml k, FromYaml v) => (S.Node, S.Node) -> Parser (k, v)
    entry (k, v) = (,) <$> parseNode parseYaml k <*> parseEntry (k, v)

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
        *> ((,) <$> parseField @a o "numerator" <*> parseField @a o "denominator")
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
  parseYaml = withScientific $ \s ->
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

-- $setup
-- >>> import Yamlet
-- >>> printErrors = mapM_ (putStrLn . prettyError "input.yaml")
