{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Control.DeepSeq
import Control.Monad
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.YAML qualified as H
import Data.YAML.Event qualified as HE
import Data.Yaml qualified as Y
import GHC.Generics
import Test.Tasty.Bench

import Yamlet
import Yamlet.Syntax qualified as S

main :: IO ()
main =
  defaultMain
    [ input @[Record] "records" $ records 5000
    , input @[FlowRecord] "flow" $ flow 5000
    , input @(M.Map T.Text T.Text) "text" $ text 2000
    ]

-- | The benchmarks of an input. The type is the result of the benchmarks that
-- decode the input into a Haskell value.
input
  :: forall a
   . (NFData a, FromYAML a, H.FromYAML a, J.FromJSON a)
  => String
  -> T.Text
  -> Benchmark
input name t = env (pure (bs, bl)) $ \ ~(strict, lazy) ->
  bgroup
    (name ++ " (" ++ show (BS.length bs `div` 1024) ++ " KiB)")
    [ bench "yamlet (syntax)" $ nf S.parseDocuments strict
    , bench "yamlet (nodes)" $ nf (decodeInput >=> decodeNodes) strict
    , bench "yamlet (type)" $ nf (either (const Nothing) Just . decode @a) strict
    , bench "HsYAML (events)" $ nf HE.parseEvents lazy
    , bench "HsYAML (nodes)" $ nf (either (const ()) (foldMap (\(H.Doc n) -> forceNode n)) . H.decodeNode) lazy
    , bench "HsYAML (type)" $ nf (either (const Nothing) Just . H.decode1Strict @a) strict
    , bench "yaml (libyaml)" $ nf (either (const Nothing) Just . Y.decodeEither' @J.Value) strict
    , bench "yaml (type)" $ nf (either (const Nothing) Just . Y.decodeEither' @a) strict
    ]
  where
    bs :: BS.ByteString
    bs = T.encodeUtf8 t

    bl :: BL.ByteString
    bl = BL.fromStrict bs

-- | A block sequence of block mappings, as in a configuration file.
records :: Int -> T.Text
records n = T.concat $ map record [1 .. n]
  where
    record :: Int -> T.Text
    record i =
      T.unlines
        [ "- name: item " <> num i
        , "  id: " <> num i
        , "  tags: [alpha, beta, gamma]"
        , "  description: \"an \\\"escaped\\\" string\\twith a tab\""
        , "  path: /usr/local/share/item-" <> num i
        , "  enabled: true"
        , "  nested:"
        , "    x: 1.5"
        , "    y: -3"
        , "    list:"
        , "      - one"
        , "      - 'two'"
        ]

-- | JSON-like flow collections.
flow :: Int -> T.Text
flow n = "[" <> T.intercalate ",\n " (map record [1 .. n]) <> "]\n"
  where
    record :: Int -> T.Text
    record i =
      T.concat
        [ "{\"id\": "
        , num i
        , ", \"name\": \"item "
        , num i
        , "\", \"values\": [1, 2.5, true, null], \"child\": {\"a\": \"b\"}}"
        ]

-- | Block scalars and multi-line plain scalars.
text :: Int -> T.Text
text n = T.concat $ map entry [1 .. n]
  where
    entry :: Int -> T.Text
    entry i =
      T.unlines
        [ "key" <> num i <> ": |"
        , "  Lorem ipsum dolor sit amet, consectetur adipiscing elit."
        , "  Sed do eiusmod tempor incididunt ut labore et dolore."
        , ""
        , "    Ut enim ad minim veniam, quis nostrud exercitation."
        , "folded" <> num i <> ": >-"
        , "  Duis aute irure dolor in reprehenderit in voluptate velit"
        , "  esse cillum dolore eu fugiat nulla pariatur."
        , "plain" <> num i <> ": Excepteur sint occaecat cupidatat non proident,"
        , "  sunt in culpa qui officia deserunt mollit anim id est laborum."
        ]

num :: Int -> T.Text
num = T.pack . show

----------------------------------------
-- Types

-- | An entry of 'records'.
data Record = Record
  { name :: T.Text
  , itemId :: Int
  , tags :: [T.Text]
  , description :: T.Text
  , path :: T.Text
  , enabled :: Bool
  , nested :: Nested
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data Nested = Nested
  { x :: Double
  , y :: Int
  , list :: [T.Text]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

-- | An entry of 'flow'.
data FlowRecord = FlowRecord
  { itemId :: Int
  , name :: T.Text
  , values :: [Item]
  , child :: M.Map T.Text T.Text
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

-- | An item of the values of a 'FlowRecord'.
data Item = ItemNumber Double | ItemBool Bool | ItemNull
  deriving stock (Generic)
  deriving anyclass (NFData)

instance FromYAML Record where
  parseYAML = withMapping $ \o ->
    Record
      <$> o .: "name"
      <*> o .: "id"
      <*> o .: "tags"
      <*> o .: "description"
      <*> o .: "path"
      <*> o .: "enabled"
      <*> o .: "nested"

instance H.FromYAML Record where
  parseYAML = H.withMap "Record" $ \o ->
    Record
      <$> o H..: "name"
      <*> o H..: "id"
      <*> o H..: "tags"
      <*> o H..: "description"
      <*> o H..: "path"
      <*> o H..: "enabled"
      <*> o H..: "nested"

instance J.FromJSON Record where
  parseJSON = J.withObject "Record" $ \o ->
    Record
      <$> o J..: "name"
      <*> o J..: "id"
      <*> o J..: "tags"
      <*> o J..: "description"
      <*> o J..: "path"
      <*> o J..: "enabled"
      <*> o J..: "nested"

instance FromYAML Nested where
  parseYAML = withMapping $ \o -> Nested <$> o .: "x" <*> o .: "y" <*> o .: "list"

instance H.FromYAML Nested where
  parseYAML = H.withMap "Nested" $ \o -> Nested <$> o H..: "x" <*> o H..: "y" <*> o H..: "list"

instance J.FromJSON Nested where
  parseJSON = J.withObject "Nested" $ \o -> Nested <$> o J..: "x" <*> o J..: "y" <*> o J..: "list"

instance FromYAML FlowRecord where
  parseYAML = withMapping $ \o ->
    FlowRecord <$> o .: "id" <*> o .: "name" <*> o .: "values" <*> o .: "child"

instance H.FromYAML FlowRecord where
  parseYAML = H.withMap "FlowRecord" $ \o ->
    FlowRecord <$> o H..: "id" <*> o H..: "name" <*> o H..: "values" <*> o H..: "child"

instance J.FromJSON FlowRecord where
  parseJSON = J.withObject "FlowRecord" $ \o ->
    FlowRecord <$> o J..: "id" <*> o J..: "name" <*> o J..: "values" <*> o J..: "child"

instance FromYAML Item where
  parseYAML n = case n.value of
    Int i -> pure $ ItemNumber (fromInteger i)
    Float f -> pure $ ItemNumber (floatToDouble f)
    Bool b -> pure $ ItemBool b
    Null -> pure ItemNull
    _ -> typeMismatch "a number, a boolean or null" n

instance H.FromYAML Item where
  parseYAML = \case
    H.Scalar _ (H.SInt i) -> pure $ ItemNumber (fromInteger i)
    H.Scalar _ (H.SFloat d) -> pure $ ItemNumber d
    H.Scalar _ (H.SBool b) -> pure $ ItemBool b
    H.Scalar _ H.SNull -> pure ItemNull
    n -> H.typeMismatch "a number, a boolean or null" n

instance J.FromJSON Item where
  parseJSON = \case
    J.Number s -> pure $ ItemNumber (Sci.toRealFloat s)
    J.Bool b -> pure $ ItemBool b
    J.Null -> pure ItemNull
    _ -> fail "expected a number, a boolean or null"

-- | HsYAML has no NFData instance for nodes.
forceNode :: H.Node loc -> ()
forceNode = \case
  H.Scalar _ s -> case s of
    H.SNull -> ()
    H.SBool b -> b `seq` ()
    H.SFloat d -> d `seq` ()
    H.SInt i -> i `seq` ()
    H.SStr t -> t `seq` ()
    H.SUnknown tag t -> tag `seq` t `seq` ()
  H.Mapping _ _ m -> foldMap (\(k, v) -> forceNode k `seq` forceNode v) (M.toList m)
  H.Sequence _ _ xs -> foldMap forceNode xs
  H.Anchor _ _ n -> forceNode n
