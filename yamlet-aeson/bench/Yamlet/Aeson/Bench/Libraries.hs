{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The benchmarks that compare the instances of aeson through 'ViaAeson'
-- with the instances of yamlet and with the yaml package.
module Yamlet.Aeson.Bench.Libraries
  ( libraryBenchmarks
  ) where

import Control.DeepSeq
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Yaml qualified as Y
import Test.Tasty.Bench
import Yamlet

import Yamlet.Aeson
import Yamlet.Aeson.Bench.Inputs
import Yamlet.Aeson.Bench.Types

-- | The benchmarks are grouped by the operation, so that the times of the
-- libraries for one operation and input are next to each other.
libraryBenchmarks :: [Benchmark]
libraryBenchmarks =
  [ bgroup
      "decode"
      [ decoding @[Config] "config" configInput
      , decoding @[Json] "json" jsonInput
      ]
  , bgroup
      "encode"
      [ encoding @[Config] "config" configInput
      , encoding @[Json] "json" jsonInput
      ]
  ]
  where
    -- The benchmarks that decode an input into a value of the type.
    decoding
      :: forall a
       . (NFData a, FromYaml a, J.FromJSON a)
      => String
      -> BS.ByteString
      -> Benchmark
    decoding name bs =
      bgroup
        name
        [ bench "yamlet" $ nf (either (error . show) id . decode @a) bs
        , bench "yamlet-aeson" $
            nf (either (error . show) (.value) . decode @(ViaAeson a)) bs
        , bench "yaml" $ nf (either (error . show) id . Y.decodeEither' @a) bs
        ]

    -- The benchmarks that encode the value of an input, decoded as the type.
    encoding
      :: forall a
       . (NFData a, FromYaml a, ToYaml a, J.ToJSON a)
      => String
      -> BS.ByteString
      -> Benchmark
    encoding name bs =
      bgroup
        name
        [ bench "yamlet" $ nf encode value
        , bench "yamlet-aeson" $ nf (encode . ViaAeson) value
        , bench "yaml" $ nf Y.encode value
        ]
      where
        value :: a
        value = either (error . show) id $ decode bs
