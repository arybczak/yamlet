-- | The types of "Derive.Generic" with written instances.
module Derive.Manual
  ( A (..)
  , B (..)
  , C (..)
  , X (..)
  , F (..)
  , mkX
  , mkF
  ) where

import Control.DeepSeq
import Data.Text qualified as T
import GHC.Generics (Generic)

import Yamlet

data A = A
  { a01 :: T.Text
  , a02 :: Maybe Int
  , a03 :: Int
  , a04 :: T.Text
  , a05 :: Maybe Int
  , a06 :: Int
  , a07 :: T.Text
  , a08 :: Maybe Int
  , a09 :: Int
  , a10 :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

instance ToYaml A where
  toYaml x =
    mapping
      [ "a01" .= x.a01
      , "a02" .= x.a02
      , "a03" .= x.a03
      , "a04" .= x.a04
      , "a05" .= x.a05
      , "a06" .= x.a06
      , "a07" .= x.a07
      , "a08" .= x.a08
      , "a09" .= x.a09
      , "a10" .= x.a10
      ]

instance FromYaml A where
  parseYaml = withMapping $ \o ->
    A
      <$> parseField o "a01"
      <*> parseFieldMaybe o "a02"
      <*> parseField o "a03"
      <*> parseField o "a04"
      <*> parseFieldMaybe o "a05"
      <*> parseField o "a06"
      <*> parseField o "a07"
      <*> parseFieldMaybe o "a08"
      <*> parseField o "a09"
      <*> parseField o "a10"

data B = B
  { b01 :: T.Text
  , b02 :: Maybe Int
  , b03 :: Int
  , b04 :: T.Text
  , b05 :: Maybe Int
  , b06 :: Int
  , b07 :: T.Text
  , b08 :: Maybe Int
  , b09 :: Int
  , b10 :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

instance ToYaml B where
  toYaml x =
    mapping
      [ "b01" .= x.b01
      , "b02" .= x.b02
      , "b03" .= x.b03
      , "b04" .= x.b04
      , "b05" .= x.b05
      , "b06" .= x.b06
      , "b07" .= x.b07
      , "b08" .= x.b08
      , "b09" .= x.b09
      , "b10" .= x.b10
      ]

instance FromYaml B where
  parseYaml = withMapping $ \o ->
    B
      <$> parseField o "b01"
      <*> parseFieldMaybe o "b02"
      <*> parseField o "b03"
      <*> parseField o "b04"
      <*> parseFieldMaybe o "b05"
      <*> parseField o "b06"
      <*> parseField o "b07"
      <*> parseFieldMaybe o "b08"
      <*> parseField o "b09"
      <*> parseField o "b10"

data C = C
  { c01 :: T.Text
  , c02 :: Maybe Int
  , c03 :: Int
  , c04 :: T.Text
  , c05 :: Maybe Int
  , c06 :: Int
  , c07 :: T.Text
  , c08 :: Maybe Int
  , c09 :: Int
  , c10 :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

instance ToYaml C where
  toYaml x =
    mapping
      [ "c01" .= x.c01
      , "c02" .= x.c02
      , "c03" .= x.c03
      , "c04" .= x.c04
      , "c05" .= x.c05
      , "c06" .= x.c06
      , "c07" .= x.c07
      , "c08" .= x.c08
      , "c09" .= x.c09
      , "c10" .= x.c10
      ]

instance FromYaml C where
  parseYaml = withMapping $ \o ->
    C
      <$> parseField o "c01"
      <*> parseFieldMaybe o "c02"
      <*> parseField o "c03"
      <*> parseField o "c04"
      <*> parseFieldMaybe o "c05"
      <*> parseField o "c06"
      <*> parseField o "c07"
      <*> parseFieldMaybe o "c08"
      <*> parseField o "c09"
      <*> parseField o "c10"

-- | A sum of records with the default encoding.
data X = X1 A | X2 B | X3 C
  deriving stock (Generic)
  deriving anyclass (NFData)

instance ToYaml X where
  toYaml = \case
    X1 a -> tagged "X1" a
    X2 b -> tagged "X2" b
    X3 c -> tagged "X3" c
    where
      tagged :: ToYaml a => T.Text -> a -> Node
      tagged t a = mapping ["tag" .= t, "contents" .= a]

instance FromYaml X where
  parseYaml = withMapping $ \o -> do
    tag <- parseField o "tag"
    case tag :: T.Text of
      "X1" -> X1 <$> parseField o "contents"
      "X2" -> X2 <$> parseField o "contents"
      "X3" -> X3 <$> parseField o "contents"
      _ -> fail ("unknown tag " ++ show tag)

-- | The same sum with flat fields.
data F = F1 A | F2 B | F3 C
  deriving stock (Generic)
  deriving anyclass (NFData)

instance ToYaml F where
  toYaml = \case
    F1 a -> tagged "F1" (toYaml a)
    F2 b -> tagged "F2" (toYaml b)
    F3 c -> tagged "F3" (toYaml c)
    where
      tagged :: T.Text -> Node -> Node
      tagged t n = case view n of
        MappingView kvs -> mapping (("tag" .= t) : kvs)
        _ -> mapping ["tag" .= t, "contents" .= n]

instance FromYaml F where
  parseYaml = withMapping $ \o -> do
    tag <- parseField o "tag"
    case tag :: T.Text of
      "F1" -> F1 <$> parseYaml (objectNode o)
      "F2" -> F2 <$> parseYaml (objectNode o)
      "F3" -> F3 <$> parseYaml (objectNode o)
      _ -> fail ("unknown tag " ++ show tag)

mkX :: Int -> X
mkX i = case i `mod` 3 of
  0 -> X1 (mkA i)
  1 -> X2 (mkB i)
  _ -> X3 (mkC i)

mkF :: Int -> F
mkF i = case i `mod` 3 of
  0 -> F1 (mkA i)
  1 -> F2 (mkB i)
  _ -> F3 (mkC i)

mkA :: Int -> A
mkA i =
  A
    (T.pack (show i))
    (if even i then Nothing else Just 2)
    (i + 3)
    (T.pack (show (i * 4)))
    (if even i then Nothing else Just 5)
    (i + 6)
    (T.pack (show (i * 7)))
    (if even i then Nothing else Just 8)
    (i + 9)
    (T.pack (show (i * 10)))

mkB :: Int -> B
mkB i =
  B
    (T.pack (show i))
    (if even i then Nothing else Just 2)
    (i + 3)
    (T.pack (show (i * 4)))
    (if even i then Nothing else Just 5)
    (i + 6)
    (T.pack (show (i * 7)))
    (if even i then Nothing else Just 8)
    (i + 9)
    (T.pack (show (i * 10)))

mkC :: Int -> C
mkC i =
  C
    (T.pack (show i))
    (if even i then Nothing else Just 2)
    (i + 3)
    (T.pack (show (i * 4)))
    (if even i then Nothing else Just 5)
    (i + 6)
    (T.pack (show (i * 7)))
    (if even i then Nothing else Just 8)
    (i + 9)
    (T.pack (show (i * 10)))
