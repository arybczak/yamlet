-- | The benchmark types with generic instances. "Yamlet.Bench.Derive.Manual" has the
-- same types with written instances, which give the same YAML.
module Yamlet.Bench.Derive.Generic
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

import Yamlet
import Yamlet.Bench.Derive.Fields

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
  deriving anyclass (NFData, GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml A

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
  deriving anyclass (NFData, GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml B

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
  deriving anyclass (NFData, GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml C

-- | A sum of records with the default encoding.
data X = X1 A | X2 B | X3 C
  deriving stock (Generic)
  deriving anyclass (NFData, GenericYamlOptions)
  deriving (FromYaml, ToYaml) via GenericYaml X

-- | The same sum with flat fields.
data F = F1 A | F2 B | F3 C
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving (FromYaml, ToYaml) via GenericYaml F

instance GenericYamlOptions F where
  type SumEncoding F = TaggedFlat

mkX :: Int -> X
mkX i = case i `mod` 3 of
  0 -> X1 (fields A i)
  1 -> X2 (fields B i)
  _ -> X3 (fields C i)

mkF :: Int -> F
mkF i = case i `mod` 3 of
  0 -> F1 (fields A i)
  1 -> F2 (fields B i)
  _ -> F3 (fields C i)
