-- | The benchmark types with generic instances. "Yamlet.Bench.Derive.Manual" has the
-- same types with written instances, which give the same YAML.
module Yamlet.Bench.Derive.Generic
  ( A (..)
  , B (..)
  , C (..)
  , X (..)
  , F (..)
  , D (..)
  , P (..)
  , O (..)
  , E (..)
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

-- | A record with a default for the fields of type 'Maybe' and 'Int'.
data D = D
  { d01 :: T.Text
  , d02 :: Maybe Int
  , d03 :: Int
  , d04 :: T.Text
  , d05 :: Maybe Int
  , d06 :: Int
  , d07 :: T.Text
  , d08 :: Maybe Int
  , d09 :: Int
  , d10 :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving (FromYaml, ToYaml) via GenericYaml D

instance GenericYamlOptions D where
  yamlDefault =
    Just
      D
        { d01 = requiredField
        , d02 = Nothing
        , d03 = 0
        , d04 = requiredField
        , d05 = Nothing
        , d06 = 0
        , d07 = requiredField
        , d08 = Nothing
        , d09 = 0
        , d10 = requiredField
        }

-- | 'D' with a type parameter. The default depends on the 'Num' instance of
-- the parameter.
data P a = P
  { p01 :: T.Text
  , p02 :: Maybe a
  , p03 :: a
  , p04 :: T.Text
  , p05 :: Maybe a
  , p06 :: a
  , p07 :: T.Text
  , p08 :: Maybe a
  , p09 :: a
  , p10 :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving via GenericYaml (P a) instance (FromYaml a, Num a) => FromYaml (P a)

deriving via GenericYaml (P a) instance (ToYaml a, Num a) => ToYaml (P a)

instance Num a => GenericYamlOptions (P a) where
  yamlDefault =
    Just
      P
        { p01 = requiredField
        , p02 = Nothing
        , p03 = 0
        , p04 = requiredField
        , p05 = Nothing
        , p06 = 0
        , p07 = requiredField
        , p08 = Nothing
        , p09 = 0
        , p10 = requiredField
        }

-- | A record that leaves out its null fields.
data O = O
  { o01 :: T.Text
  , o02 :: Maybe Int
  , o03 :: Int
  , o04 :: T.Text
  , o05 :: Maybe Int
  , o06 :: Int
  , o07 :: T.Text
  , o08 :: Maybe Int
  , o09 :: Int
  , o10 :: T.Text
  }
  deriving stock (Generic)
  deriving (ToYaml) via GenericYaml O

instance GenericYamlOptions O where
  yamlOptions = defaultYamlOptions {omitNullFields = True}

-- | 'O' with the default of 'D'. The encoder leaves out a null field only if
-- its default is null.
data E = E
  { e01 :: T.Text
  , e02 :: Maybe Int
  , e03 :: Int
  , e04 :: T.Text
  , e05 :: Maybe Int
  , e06 :: Int
  , e07 :: T.Text
  , e08 :: Maybe Int
  , e09 :: Int
  , e10 :: T.Text
  }
  deriving stock (Generic)
  deriving (ToYaml) via GenericYaml E

instance GenericYamlOptions E where
  yamlOptions = defaultYamlOptions {omitNullFields = True}
  yamlDefault =
    Just
      E
        { e01 = requiredField
        , e02 = Nothing
        , e03 = 0
        , e04 = requiredField
        , e05 = Nothing
        , e06 = 0
        , e07 = requiredField
        , e08 = Nothing
        , e09 = 0
        , e10 = requiredField
        }

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
