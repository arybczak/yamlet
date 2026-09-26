-- | The benchmark types with generic instances. "Derive.Manual" has the
-- same types with written instances, which give the same YAML.
module Derive.Generic
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
  deriving anyclass (NFData, GenericYaml, FromYaml, ToYaml)

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
  deriving anyclass (NFData, GenericYaml, FromYaml, ToYaml)

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
  deriving anyclass (NFData, GenericYaml, FromYaml, ToYaml)

-- | A sum of records with the default encoding.
data X = X1 A | X2 B | X3 C
  deriving stock (Generic)
  deriving anyclass (NFData, GenericYaml, FromYaml, ToYaml)

-- | The same sum with flat fields.
data F = F1 A | F2 B | F3 C
  deriving stock (Generic)
  deriving anyclass (NFData, FromYaml, ToYaml)

instance GenericYaml F where
  yamlOptions = defaultYamlOptions {flattenFields = True}

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
    (T.pack (show (i * 1)))
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
    (T.pack (show (i * 1)))
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
    (T.pack (show (i * 1)))
    (if even i then Nothing else Just 2)
    (i + 3)
    (T.pack (show (i * 4)))
    (if even i then Nothing else Just 5)
    (i + 6)
    (T.pack (show (i * 7)))
    (if even i then Nothing else Just 8)
    (i + 9)
    (T.pack (show (i * 10)))
