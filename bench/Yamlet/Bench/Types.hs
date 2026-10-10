-- | The types that the inputs decode into, with written instances for each
-- library.
module Yamlet.Bench.Types
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
import Data.YAML qualified as H

import Yamlet

-- | An entry of 'Yamlet.Bench.Inputs.config'.
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

-- | An entry of 'Yamlet.Bench.Inputs.json'.
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
      <$> parseField o "x"
      <*> parseField o "y"
      <*> parseField o "list"

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
      <$> parseField o "id"
      <*> parseField o "name"
      <*> parseField o "values"
      <*> parseField o "child"

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
  parseYaml n = case view n of
    IntView i -> pure $ ItemNumber (fromInteger i)
    FloatView f -> pure $ ItemNumber (floatValueToRealFloat f)
    BoolView b -> pure $ ItemBool b
    NullView -> pure ItemNull
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
