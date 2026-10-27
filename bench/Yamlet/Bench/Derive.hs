{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The benchmarks that compare generic instances with written ones.
module Yamlet.Bench.Derive
  ( checkDerived
  , deriveBenchmarks
  ) where

import Control.DeepSeq
import Control.Monad
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Test.Tasty.Bench

import Yamlet
import Yamlet.Bench.Derive.Fields
import Yamlet.Bench.Derive.Generic qualified as G
import Yamlet.Bench.Derive.Manual qualified as M

-- | Fail if the two versions of a type give different YAML, or decode the
-- YAML with missing keys to different values.
checkDerived :: IO ()
checkDerived = do
  same "contents" G.mkX M.mkX
  same "flat" G.mkF M.mkF
  same "record" (fields G.A) (fields M.A)
  same "parameter" (fields (G.P @Int)) (fields (M.P @Int))
  same "omit null" (fields G.O) (fields M.O)
  same "omit default" (fields G.E) (fields M.E)
  sameMissing @G.A @M.A "null" nullInput
  sameMissing @G.D @M.D "default" defaultInput
  sameMissing @(G.P Int) @(M.P Int) "parameter" parameterInput
  where
    same :: (ToYaml g, ToYaml m) => String -> (Int -> g) -> (Int -> m) -> IO ()
    same name mkG mkM =
      unless (encode (map mkG values) == encode (map mkM values)) $
        fail ("the generic and the written instances differ: " ++ name)

    sameMissing
      :: forall g m
       . (FromYaml g, ToYaml g, FromYaml m, ToYaml m)
      => String
      -> Node
      -> IO ()
    sameMissing name yaml =
      unless (encode (parsed @g) == encode (parsed @m)) $
        fail ("the generic and the written instances decode differently: " ++ name)
      where
        parsed :: FromYaml a => [a]
        parsed = either (error . show) id (runParser parseYaml yaml)

-- | The benchmarks of each format, grouped by the operation, so that the
-- times of the two versions are next to each other.
deriveBenchmarks :: Benchmark
deriveBenchmarks =
  bgroup
    "derive"
    [ format "contents" G.mkX M.mkX
    , format "flat" G.mkF M.mkF
    , format "record" (fields G.A) (fields M.A)
    , format "parameter" (fields (G.P @Int)) (fields (M.P @Int))
    , bgroup
        "missing"
        [ missing @G.A @M.A "null" nullInput
        , missing @G.D @M.D "default" defaultInput
        , missing @(G.P Int) @(M.P Int) "parameter" parameterInput
        ]
    , bgroup
        "omit"
        [ omit "null" (fields G.O) (fields M.O)
        , omit "default" (fields G.E) (fields M.E)
        ]
    ]
  where
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
            [ bench "generic" $ nf (either (error . show) id . decode @[g]) bs
            , bench "manual" $ nf (either (error . show) id . decode @[m]) bs
            ]
        ]
      where
        gs :: [g]
        gs = map mkG values

        ms :: [m]
        ms = map mkM values

        -- Both versions give the same YAML, see 'checkDerived'.
        yaml :: Node
        yaml = toYaml gs

        bs :: BS.ByteString
        bs = encode gs

    -- Only the decoders, because the encoders write each key.
    missing
      :: forall g m
       . (NFData g, FromYaml g, NFData m, FromYaml m)
      => String
      -> Node
      -> Benchmark
    missing name yaml =
      bgroup
        name
        [ bgroup
            "parseYaml"
            [ bench "generic" $ nf (runParser (parseYaml @[g])) yaml
            , bench "manual" $ nf (runParser (parseYaml @[m])) yaml
            ]
        , bgroup
            "decode"
            [ bench "generic" $ nf (either (error . show) id . decode @[g]) bs
            , bench "manual" $ nf (either (error . show) id . decode @[m]) bs
            ]
        ]
      where
        bs :: BS.ByteString
        bs = encode yaml

    -- Only the encoders, because leaving out null fields changes only them.
    omit
      :: forall g m
       . (ToYaml g, ToYaml m)
      => String
      -> (Int -> g)
      -> (Int -> m)
      -> Benchmark
    omit name mkG mkM =
      bgroup
        name
        [ bgroup
            "toYaml"
            [ bench "generic" $ nf toYaml gs
            , bench "manual" $ nf toYaml ms
            ]
        , bgroup
            "encode"
            [ bench "generic" $ nf encode gs
            , bench "manual" $ nf encode ms
            ]
        ]
      where
        gs :: [g]
        gs = map mkG values

        ms :: [m]
        ms = map mkM values

values :: [Int]
values = [1 .. 1000]

-- | The records without the keys of the fields of type 'Maybe', which decode
-- as null.
nullInput :: Node
nullInput = withoutKeys ["a02", "a05", "a08"] (toYaml (map (fields G.A) values))

-- | The records without the keys of the fields with a default.
defaultInput :: Node
defaultInput = withoutKeys (defaultKeys 'd') (toYaml (map (fields G.D) values))

-- | 'defaultInput' for the type with a parameter.
parameterInput :: Node
parameterInput =
  withoutKeys (defaultKeys 'p') (toYaml (map (fields (G.P @Int)) values))

-- | The keys of the fields with a default, for the prefix of the field names.
defaultKeys :: Char -> [T.Text]
defaultKeys prefix = [T.pack (prefix : n) | n <- ["02", "03", "05", "06", "08", "09"]]

-- | The sequence of mappings without the keys.
withoutKeys :: [T.Text] -> Node -> Node
withoutKeys keys n = case view n of
  SequenceView items -> toYaml (map dropKeys items)
  _ -> n
  where
    dropKeys :: Node -> Node
    dropKeys item = case view item of
      MappingView kvs -> mapping [kv | kv@(k, _) <- kvs, view k `notElem` map StringView keys]
      _ -> item
