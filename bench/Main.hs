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
    [ input @[Config] "config" $ config 5000
    , input @[Json] "json" $ json 5000
    , input @(M.Map T.Text T.Text) "text" $ text 2000
    ]

-- | The benchmarks of an input. The type is the result of the benchmarks that
-- decode the input into a Haskell value, and the source of the benchmarks that
-- encode it.
input
  :: forall a
   . ( NFData a
     , FromYaml a
     , H.FromYAML a
     , J.FromJSON a
     , ToYaml a
     , H.ToYAML a
     , J.ToJSON a
     )
  => String
  -> T.Text
  -> Benchmark
input name t = env (pure (bs, bl, value)) $ \ ~(strict, lazy, v) ->
  bgroup
    (name ++ " (" ++ show (BS.length bs `div` 1024) ++ " KiB)")
    [ bgroup
        "yamlet"
        [ bgroup
            "parse"
            [ bench "syntax tree" $ nf S.parseDocuments strict
            , bench "nodes" $ nf (decodeInput >=> decodeNodes) strict
            ]
        , bench "decode" $ nf (either (const Nothing) Just . decode @a) strict
        , bench "encode" $ nf encode v
        ]
    , bgroup
        "HsYAML"
        [ bgroup
            "parse"
            [ bench "events" $ nf HE.parseEvents lazy
            , bench "nodes" $ nf (either (const ()) (foldMap (\(H.Doc n) -> forceNode n)) . H.decodeNode) lazy
            ]
        , bench "decode" $ nf (either (const Nothing) Just . H.decode1Strict @a) strict
        , bench "encode" $ nf H.encode1Strict v
        ]
    , bgroup
        "yaml"
        [ bgroup
            "parse"
            [bench "aeson value" $ nf (either (const Nothing) Just . Y.decodeEither' @J.Value) strict]
        , bench "decode" $ nf (either (const Nothing) Just . Y.decodeEither' @a) strict
        , bench "encode" $ nf Y.encode v
        ]
    ]
  where
    value :: a
    value = either (error . show) id $ decode bs

    bs :: BS.ByteString
    bs = T.encodeUtf8 t

    bl :: BL.ByteString
    bl = BL.fromStrict bs

-- | A block sequence of block mappings, as in a configuration file.
config :: Int -> T.Text
config n = T.concat $ map record [1 .. n]
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
json :: Int -> T.Text
json n = "[" <> T.intercalate ",\n " (map record [1 .. n]) <> "]\n"
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

-- | An entry of 'config'.
data Config = Config
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

-- | An entry of 'json'.
data Json = Json
  { itemId :: Int
  , name :: T.Text
  , values :: [Item]
  , child :: M.Map T.Text T.Text
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

-- | An item of the values of a 'Json'.
data Item = ItemNumber Double | ItemBool Bool | ItemNull
  deriving stock (Generic)
  deriving anyclass (NFData)

instance FromYaml Config where
  parseYaml = withMapping $ \o ->
    Config
      <$> o .: "name"
      <*> o .: "id"
      <*> o .: "tags"
      <*> o .: "description"
      <*> o .: "path"
      <*> o .: "enabled"
      <*> o .: "nested"

instance H.FromYAML Config where
  parseYAML = H.withMap "Config" $ \o ->
    Config
      <$> o H..: "name"
      <*> o H..: "id"
      <*> o H..: "tags"
      <*> o H..: "description"
      <*> o H..: "path"
      <*> o H..: "enabled"
      <*> o H..: "nested"

instance J.FromJSON Config where
  parseJSON = J.withObject "Config" $ \o ->
    Config
      <$> o J..: "name"
      <*> o J..: "id"
      <*> o J..: "tags"
      <*> o J..: "description"
      <*> o J..: "path"
      <*> o J..: "enabled"
      <*> o J..: "nested"

instance FromYaml Nested where
  parseYaml = withMapping $ \o ->
    Nested
      <$> o .: "x"
      <*> o .: "y"
      <*> o .: "list"

instance H.FromYAML Nested where
  parseYAML = H.withMap "Nested" $ \o ->
    Nested
      <$> o H..: "x"
      <*> o H..: "y"
      <*> o H..: "list"

instance J.FromJSON Nested where
  parseJSON = J.withObject "Nested" $ \o ->
    Nested
      <$> o J..: "x"
      <*> o J..: "y"
      <*> o J..: "list"

instance FromYaml Json where
  parseYaml = withMapping $ \o ->
    Json
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "values"
      <*> o .: "child"

instance H.FromYAML Json where
  parseYAML = H.withMap "Json" $ \o ->
    Json
      <$> o H..: "id"
      <*> o H..: "name"
      <*> o H..: "values"
      <*> o H..: "child"

instance J.FromJSON Json where
  parseJSON = J.withObject "Json" $ \o ->
    Json
      <$> o J..: "id"
      <*> o J..: "name"
      <*> o J..: "values"
      <*> o J..: "child"

instance FromYaml Item where
  parseYaml n = case n.value of
    Int i -> pure $ ItemNumber (fromInteger i)
    Float f -> pure $ ItemNumber (floatValueToDouble f)
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

instance ToYaml Config where
  toYaml r =
    mapping
      [ "name" .= r.name
      , "id" .= r.itemId
      , "tags" .= r.tags
      , "description" .= r.description
      , "path" .= r.path
      , "enabled" .= r.enabled
      , "nested" .= r.nested
      ]

instance H.ToYAML Config where
  toYAML r =
    H.mapping
      [ "name" H..= r.name
      , "id" H..= r.itemId
      , "tags" H..= r.tags
      , "description" H..= r.description
      , "path" H..= r.path
      , "enabled" H..= r.enabled
      , "nested" H..= r.nested
      ]

instance J.ToJSON Config where
  toJSON r =
    J.object
      [ "name" J..= r.name
      , "id" J..= r.itemId
      , "tags" J..= r.tags
      , "description" J..= r.description
      , "path" J..= r.path
      , "enabled" J..= r.enabled
      , "nested" J..= r.nested
      ]

instance ToYaml Nested where
  toYaml n =
    mapping
      [ "x" .= n.x
      , "y" .= n.y
      , "list" .= n.list
      ]

instance H.ToYAML Nested where
  toYAML n =
    H.mapping
      [ "x" H..= n.x
      , "y" H..= n.y
      , "list" H..= n.list
      ]

instance J.ToJSON Nested where
  toJSON n =
    J.object
      [ "x" J..= n.x
      , "y" J..= n.y
      , "list" J..= n.list
      ]

instance ToYaml Json where
  toYaml r =
    mapping
      [ "id" .= r.itemId
      , "name" .= r.name
      , "values" .= r.values
      , "child" .= r.child
      ]

instance H.ToYAML Json where
  toYAML r =
    H.mapping
      [ "id" H..= r.itemId
      , "name" H..= r.name
      , "values" H..= r.values
      , "child" H..= r.child
      ]

instance J.ToJSON Json where
  toJSON r =
    J.object
      [ "id" J..= r.itemId
      , "name" J..= r.name
      , "values" J..= r.values
      , "child" J..= r.child
      ]

instance ToYaml Item where
  toYaml = \case
    ItemNumber d -> toYaml d
    ItemBool b -> toYaml b
    ItemNull -> toYaml ()

instance H.ToYAML Item where
  toYAML = \case
    ItemNumber d -> H.toYAML d
    ItemBool b -> H.toYAML b
    ItemNull -> H.Scalar () H.SNull

instance J.ToJSON Item where
  toJSON = \case
    ItemNumber d -> J.toJSON d
    ItemBool b -> J.toJSON b
    ItemNull -> J.Null

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
