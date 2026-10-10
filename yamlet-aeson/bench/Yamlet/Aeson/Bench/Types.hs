-- | The types that the inputs decode into, with written 'FromYaml', 'ToYaml',
-- t'Data.Aeson.FromJSON' and t'Data.Aeson.ToJSON' instances, the same as in
-- the benchmarks of yamlet.
module Yamlet.Aeson.Bench.Types
  ( Config (..)
  , Nested (..)
  , Json (..)
  , Item (..)
  ) where

import Control.DeepSeq
import Data.Aeson qualified as J
import Data.Map.Strict qualified as M
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Yamlet

-- | An entry of 'Yamlet.Aeson.Bench.Inputs.config'.
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

-- | An entry of 'Yamlet.Aeson.Bench.Inputs.json'.
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
      <$> parseField o "name"
      <*> parseField o "id"
      <*> parseField o "tags"
      <*> parseField o "description"
      <*> parseField o "path"
      <*> parseField o "enabled"
      <*> parseField o "nested"

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
      <$> parseField o "x"
      <*> parseField o "y"
      <*> parseField o "list"

instance J.FromJSON Nested where
  parseJSON = J.withObject "Nested" $ \o ->
    Nested
      <$> o J..: "x"
      <*> o J..: "y"
      <*> o J..: "list"

instance FromYaml Json where
  parseYaml = withMapping $ \o ->
    Json
      <$> parseField o "id"
      <*> parseField o "name"
      <*> parseField o "values"
      <*> parseField o "child"

instance J.FromJSON Json where
  parseJSON = J.withObject "Json" $ \o ->
    Json
      <$> o J..: "id"
      <*> o J..: "name"
      <*> o J..: "values"
      <*> o J..: "child"

instance FromYaml Item where
  parseYaml n = case view n of
    IntView i -> pure $ ItemNumber (fromInteger i)
    FloatView f -> pure $ ItemNumber (floatValueToRealFloat f)
    BoolView b -> pure $ ItemBool b
    NullView -> pure ItemNull
    _ -> typeMismatch "a number, a boolean or null" n

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

instance J.ToJSON Item where
  toJSON = \case
    ItemNumber d -> J.toJSON d
    ItemBool b -> J.toJSON b
    ItemNull -> J.Null
