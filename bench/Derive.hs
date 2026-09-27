-- | The benchmarks that compare generic instances with written ones.
module Derive
  ( checkDerived
  , derived
  ) where

import Control.DeepSeq
import Control.Monad
import Data.ByteString qualified as BS
import Test.Tasty.Bench

import Derive.Generic qualified as G
import Derive.Manual qualified as M
import Yamlet
import Yamlet.Syntax qualified as S

-- | Fail if the two versions of a type give different YAML.
checkDerived :: IO ()
checkDerived = do
  same "contents" G.mkX M.mkX
  same "flat" G.mkF M.mkF
  where
    same :: (ToYaml g, ToYaml m) => String -> (Int -> g) -> (Int -> m) -> IO ()
    same name mkG mkM =
      unless (encode (map mkG values) == encode (map mkM values)) $
        fail ("the generic and the written instances differ: " ++ name)

-- | The benchmarks of each format, grouped by the operation, so that the
-- times of the two versions are next to each other.
derived :: Benchmark
derived =
  bgroup
    "derive"
    [ format "contents" G.mkX M.mkX
    , format "flat" G.mkF M.mkF
    ]

format
  :: forall g m
   . (NFData g, FromYaml g, ToYaml g, NFData m, FromYaml m, ToYaml m)
  => String
  -> (Int -> g)
  -> (Int -> m)
  -> Benchmark
format name mkG mkM =
  bgroup
    name
    [ bgroup
        "toYaml"
        [ bench "generic" $ nf toYaml gs
        , bench "manual" $ nf toYaml ms
        ]
    , bgroup
        "parseYaml"
        [ bench "generic" $ nf (runParser (parseYaml @[g])) yaml
        , bench "manual" $ nf (runParser (parseYaml @[m])) yaml
        ]
    , bgroup
        "encode"
        [ bench "generic" $ nf encode gs
        , bench "manual" $ nf encode ms
        ]
    , bgroup
        "decode"
        [ bench "generic" $ nf (either (const Nothing) Just . decode @[g]) bs
        , bench "manual" $ nf (either (const Nothing) Just . decode @[m]) bs
        ]
    ]
  where
    gs :: [g]
    gs = map mkG values

    ms :: [m]
    ms = map mkM values

    -- Both versions give the same YAML, see 'checkDerived'.
    yaml :: S.Node
    yaml = toSyntax (toYaml gs)

    bs :: BS.ByteString
    bs = encode gs

values :: [Int]
values = [1 .. 1000]
