-- The instances for the value of aeson are orphans. No other package should
-- define them, because yamlet and this package have the same author.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Use the t'Data.Aeson.FromJSON' and t'Data.Aeson.ToJSON' instances of aeson
-- with yamlet, for a type that has no 'FromYaml' and 'ToYaml' instances yet,
-- e.g. in a program that moves from the yaml package to yamlet.
--
-- The examples use these external imports:
--
-- >>> import Data.Aeson qualified as A
-- >>> import Data.Map.Strict qualified as M
-- >>> import Data.Text qualified as T
-- >>> import Data.Text.IO qualified as T
--
-- A value in t'ViaAeson' decodes and encodes with the
-- t'Data.Aeson.FromJSON' and t'Data.Aeson.ToJSON' instances:
--
-- >>> :{
-- data Server = Server {port :: Int, host :: T.Text}
--   deriving stock (Generic, Show)
--   deriving anyclass (A.FromJSON)
-- instance A.ToJSON Server where
--   toEncoding = A.genericToEncoding A.defaultOptions
-- :}
--
-- >>> Right (ViaAeson server) = decodeText @(ViaAeson Server) "port: 80\nhost: localhost\n"
--
-- >>> server
-- Server {port = 80, host = "localhost"}
--
-- >>> T.putStr (encodeText (ViaAeson server))
-- port: 80
-- host: localhost
--
-- A type with t'Data.Aeson.FromJSON' and t'Data.Aeson.ToJSON' instances can
-- derive its 'FromYaml' and 'ToYaml' instances via t'ViaAeson', e.g. in a
-- program that reads and writes both JSON and YAML and keeps one set of
-- instances. Such a type can be a field of a type with instances of its own:
--
-- >>> :{
-- newtype Address = Address T.Text
--   deriving stock (Show)
--   deriving newtype (A.FromJSON, A.ToJSON)
--   deriving (FromYaml, ToYaml) via ViaAeson Address
-- data Mail = Mail {from :: Address, to :: [Address]}
--   deriving stock (Generic, Show)
--   deriving anyclass (GenericYamlOptions)
--   deriving (FromYaml, ToYaml) via GenericYaml Mail
-- :}
--
-- >>> decodeText @Mail "from: a@example.com\nto: [b@example.com]\n"
-- Right (Mail {from = Address "a@example.com", to = [Address "b@example.com"]})
--
-- An error of a decoder of aeson points to the node that caused it:
--
-- >>> input = "- port: 80\n  host: a\n- port: http\n  host: b\n"
--
-- >>> T.putStr input
-- - port: 80
--   host: a
-- - port: http
--   host: b
--
-- >>> either printErrors print (decodeText @(ViaAeson [Server]) input)
-- input.yaml:3:9: [1].port: parsing Int failed, expected Number, but encountered String
--   |
-- 3 | - port: http
--   |         ^
--
-- The module also has the 'FromYaml' and 'ToYaml' instances for an aeson
-- t'Data.Aeson.Value', e.g. for a field that holds any data. They convert as
-- the section [Conversion]("Yamlet.Aeson#conversion") says:
--
-- >>> decodeText @A.Value "name: a\nports: [80, 443]\n"
-- Right (Object (fromList [("name",String "a"),("ports",Array [Number 80.0,Number 443.0])]))
--
-- = Order of keys
--
-- The encoder writes the keys of a mapping in the order of
-- 'Data.Aeson.toEncoding'. That is the order of the fields if the instance
-- defines 'Data.Aeson.toEncoding', e.g. with 'Data.Aeson.genericToEncoding' as
-- @Server@ above, or with 'Data.Aeson.TH.deriveJSON'. The default
-- 'Data.Aeson.toEncoding' goes through 'Data.Aeson.toJSON', so the keys come
-- in the order of an aeson object, which is sorted by default.
--
-- 'Data.Aeson.parseJSON' gets the keys in no order, because an aeson object
-- has none. To keep the order of a mapping, decode the mapping with a decoder
-- of yamlet. Its values can still decode via t'ViaAeson':
--
-- >>> :{
-- newtype Servers = Servers [(T.Text, Server)]
--   deriving stock (Show)
-- instance FromYaml Servers where
--   parseYaml = withMapping $ \o ->
--     Servers
--       <$> traverse
--         ( \(k, v) ->
--             (,) <$> parseYaml k <*> fmap (.value) (parseYaml @(ViaAeson Server) v)
--         )
--         (objectEntries o)
-- :}
--
-- >>> decodeText @Servers "web: {port: 80, host: a}\napi: {port: 81, host: b}\n"
-- Right (Servers [("web",Server {port = 80, host = "a"}),("api",Server {port = 81, host = "b"})])
--
-- 'Yamlet.decodeWithDocument' keeps the whole document with the decoded
-- value, e.g. to write the document back with a change.
--
-- = Conversion
--
-- #conversion#
-- A YAML document converts to an aeson t'Data.Aeson.Value' as follows:
--
-- * A key is the text of its scalar, e.g. @"0x10"@ for @0x10@ and @"~"@ for
--   @~@, as in the yaml package. Two keys with the same text are an error,
--   e.g. @1@ and @\"1\"@, which are different keys in YAML. A key that is a
--   collection is an error.
--
-- * A key @<<@ is an ordinary key, because the merge keys of YAML 1.1 are
--   not supported.
--
-- * @.inf@ and @-.inf@ are the strings @"+inf"@ and @"-inf"@, and @.nan@ is
--   null. The t'Data.Aeson.FromJSON' and t'Data.Aeson.ToJSON' instances for
--   t'Double' and t'Float' read and write these values.
--
-- * @-0.0@ is the number 0, because a t'Data.Scientific.Scientific' has no
--   negative zero.
--
-- * A number whose exponent in scientific notation is beyond the range
--   from -1000 to 1000, e.g. @1e1001@, is an error, as in yamlet. A
--   'Data.Aeson.Number' converts to YAML as aeson writes it in JSON: as an
--   integer if its 'Data.Scientific.base10Exponent' is from 0 to 1024,
--   otherwise in scientific notation. Thus @1e1001@ reads back as an
--   integer, but @1e1025@ and @1e-1001@ do not read back.
--
-- * A tag that is not of the core schema makes a scalar a string, e.g.
--   @!secret 123@ is the string @"123"@. The yaml package reads it as the
--   number 123. A collection with such a tag converts as without it, e.g.
--   @!point {x: 1}@ is the object @{"x": 1}@.
--
-- A t'Data.Aeson.FromJSON' instance can convert two different keys to the
-- same key and then keep only one of the pairs, as it does for JSON, e.g. @1@
-- and @1.0@ for a @Map Int@. The 'FromYaml' instances for maps reject such
-- keys.
--
-- >>> decodeText @(ViaAeson (M.Map Int T.Text)) "1: a\n1.0: b\n"
-- Right (ViaAeson {value = fromList [(1,"a")]})
--
-- An aeson t'Data.Aeson.Value' converts to YAML as aeson writes it in JSON,
-- e.g. the keys of a @Map Int@ are strings, which the encoder quotes because
-- they look like numbers:
--
-- >>> T.putStr (encodeText (ViaAeson (M.fromList @Int @T.Text [(1, "a")])))
-- '1': a
module Yamlet.Aeson
  ( ViaAeson (..)
  ) where

import Control.Monad
import Data.Aeson qualified as A
import Data.Aeson.Decoding.ByteString.Lazy
import Data.Aeson.Decoding.Tokens
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as A
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Scientific qualified as Sci
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Yamlet
import Yamlet.Syntax qualified as S

-- | A value that decodes and encodes with its t'Data.Aeson.FromJSON' and
-- t'Data.Aeson.ToJSON' instances. The field has no selector function, so read
-- it with record dot syntax, e.g. @(.value)@, or with a pattern.
newtype ViaAeson a = ViaAeson {value :: a}
  deriving stock (Eq, Ord, Show)

-- | The value of a node, converted as the section
-- [Conversion]("Yamlet.Aeson#conversion") says.
instance FromYaml A.Value where
  parseYaml = parseNode $ \n -> case view n of
    NullView -> pure A.Null
    BoolView b -> pure $! A.Bool b
    IntView i -> pure $! A.Number (fromInteger i)
    FloatView f ->
      pure $! case f of
        Finite s
          | s == 0 -> zero
          | otherwise -> A.Number s
        NegativeZero -> zero
        Infinity -> A.String "+inf"
        NegativeInfinity -> A.String "-inf"
        NaN -> A.Null
    StringView t -> pure $! A.String (T.copy t)
    SequenceView xs -> A.Array . V.fromList <$!> parseItems parseYaml xs
    MappingView _ -> object <$!> parseYaml n
    AliasView _ -> typeMismatch "a value" n
    where
      -- yamlet gives every zero the exponent 0, and aeson writes a number
      -- with an exponent of at least 0 as an integer. The zero of aeson that
      -- reads 0.0 from JSON has the exponent -1.
      zero :: A.Value
      zero = A.Number (Sci.scientific 0 (-1))

      -- A key of aeson orders as its text, as KeyText does.
      object :: M.Map KeyText A.Value -> A.Value
      object = A.Object . KM.fromMap . M.mapKeysMonotonic (\(KeyText t) -> K.fromText t)

-- | The value as aeson writes it in JSON.
instance ToYaml A.Value where
  toYaml = toYaml . aesonValue

-- | The value with the t'Data.Aeson.FromJSON' instance. An error of
-- 'Data.Aeson.parseJSON' points to the node at its path. If the node has no
-- such path, the error points to the deepest node of the path and names the
-- rest of it.
--
-- An error of a key of a map points to the value of the key, because aeson
-- gives it the same path as an error of the value. The path at the start of
-- the message names the key:
--
-- >>> either printErrors print (decodeText @(ViaAeson (M.Map Int T.Text)) "1: a\nabc: b\n")
-- input.yaml:2:6: abc: parsing Int failed, Unexpected 'a' while parsing number literal
--   |
-- 2 | abc: b
--   |      ^
instance A.FromJSON a => FromYaml (ViaAeson a) where
  parseYaml n = do
    v <- parseYaml n
    case A.ifromJSON @a v of
      A.ISuccess a -> pure (ViaAeson a)
      A.IError path msg -> failAtPath path msg n
    where
      failAtPath :: A.JSONPath -> String -> S.Node -> Parser b
      failAtPath path msg node = case (path, node.content) of
        ([], _) -> failAt node msg
        (A.Key key : rest, S.MappingContent _ kvs)
          | Just (_, v) <- L.find (isKey (K.toText key) . fst) kvs ->
              failAtPath rest msg v
        (A.Index i : rest, S.SequenceContent _ xs)
          | i >= 0
          , x : _ <- drop i xs ->
              failAtPath rest msg x
        _ ->
          failAt node (msg ++ " at " ++ renderPath (pathFromElements (map element path)))

      isKey :: T.Text -> S.Node -> Bool
      isKey key k = case k.content of
        S.ScalarContent _ t -> t == key
        _ -> False

      element :: A.JSONPathElement -> PathElement
      element = \case
        A.Key key -> Key (K.toText key)
        A.Index i -> Index i

-- | The value with the t'Data.Aeson.ToJSON' instance, with the keys of each
-- mapping in the order of 'Data.Aeson.toEncoding'. Of two equal keys, the
-- first one stays, as when aeson decodes JSON.
--
-- If the encoding is not valid JSON, which only
-- 'Data.Aeson.Encoding.unsafeToEncoding' can cause, the conversion throws an
-- error.
instance A.ToJSON a => ToYaml (ViaAeson a) where
  -- The encode benchmarks of yamlet-aeson do not get faster with nodes built
  -- straight from the tokens, without the set of keys, or with a lazy
  -- conversion and the lexer of a strict ByteString. The last one makes
  -- toYaml faster, but the renderer keeps the whole tree alive, so the
  -- garbage collector copies it anyway.
  toYaml (ViaAeson a) = toYaml $ case value (lbsToTokens (A.encode a)) of
    Left err -> invalid err
    Right (v, rest)
      | LBS8.all (`elem` jsonSpace) rest -> v
      | otherwise -> invalid $ "unexpected " ++ show (LBS8.unpack rest) ++ " after the value"
    where
      -- The whitespace of RFC 8259.
      jsonSpace :: String
      jsonSpace = " \t\n\r"

      -- The call stack would only point to this module.
      invalid :: String -> b
      invalid err =
        errorWithoutStackTrace $
          "Yamlet.Aeson.ViaAeson: the toEncoding is not valid JSON: " ++ err

      value :: Tokens t String -> Either String (Value, t)
      value = \case
        TkLit l rest -> Right (lit l, rest)
        TkText t rest -> Right (String t, rest)
        TkNumber n rest -> Right (number n, rest)
        TkArrayOpen arr -> items [] arr
        TkRecordOpen r -> pairs [] r
        TkErr err -> Left err

      -- The items and the pairs are in reverse order until the end.
      items :: [Value] -> TkArray t String -> Either String (Value, t)
      items acc = \case
        TkItem toks -> value toks >>= \(v, rest) -> items (v : acc) rest
        TkArrayEnd rest -> Right (Sequence (reverse acc), rest)
        TkArrayErr err -> Left err

      pairs :: [(A.Key, Value)] -> TkRecord t String -> Either String (Value, t)
      pairs acc = \case
        TkPair key toks -> value toks >>= \(v, rest) -> pairs ((key, v) : acc) rest
        TkRecordEnd rest -> Right (Mapping (firstKeys Set.empty (reverse acc)), rest)
        TkRecordErr err -> Left err

      firstKeys :: Set.Set A.Key -> [(A.Key, Value)] -> [(Value, Value)]
      firstKeys seen = \case
        [] -> []
        (key, v) : rest
          | key `Set.member` seen -> firstKeys seen rest
          | otherwise -> (String (K.toText key), v) : firstKeys (Set.insert key seen) rest

      lit :: Lit -> Value
      lit = \case
        LitNull -> Null
        LitTrue -> Bool True
        LitFalse -> Bool False

      number :: Number -> Value
      number = \case
        NumInteger i -> Int i
        NumDecimal s -> Float (Finite s)
        NumScientific s -> Float (Finite s)

-- | The text of a scalar key, which is the key of an aeson object.
newtype KeyText = KeyText T.Text
  deriving stock (Eq, Ord)

instance FromYaml KeyText where
  parseYaml = parseNode $ \k -> case k.content of
    S.ScalarContent _ t -> pure $! KeyText (T.copy t)
    _ -> typeMismatch "a scalar key" k

-- | The value as aeson writes it in JSON.
aesonValue :: A.Value -> Value
aesonValue = \case
  A.Null -> Null
  A.Bool b -> Bool b
  A.Number s -> number s
  A.String t -> String t
  A.Array xs -> Sequence (map aesonValue (V.toList xs))
  A.Object o -> Mapping [(String (K.toText k), aesonValue v) | (k, v) <- KM.toList o]
  where
    -- The bounds of the exponent for an integer are those of the encoder of
    -- aeson, @Data.Aeson.Encoding.Builder.scientific@, so that a value converts
    -- as its JSON encoding does for 'ViaAeson'.
    number :: Sci.Scientific -> Value
    number s
      | e < 0 || e > 1024 = Float (Finite s)
      | otherwise = Int (Sci.coefficient s * 10 ^ e)
      where
        e :: Int
        e = Sci.base10Exponent s

-- $setup
-- >>> import Yamlet
-- >>> printErrors = mapM_ (putStrLn . prettyError "input.yaml")
