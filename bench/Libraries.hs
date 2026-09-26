{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The benchmarks that compare yamlet with the other libraries.
module Libraries
  ( parsing
  , rendering
  , decoding
  , encoding
  , aesonDecoding
  , aesonEncoding
  ) where

import Control.DeepSeq
import Control.Monad
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.YAML qualified as H
import Data.YAML.Event qualified as HE
import Data.Yaml qualified as Y
import Test.Tasty.Bench

import Yamlet
import Yamlet.Syntax qualified as S

-- | The benchmarks that parse an input into the trees of each library.
parsing :: String -> BS.ByteString -> Benchmark
parsing name bs =
  bgroup
    name
    [ bgroup
        "yamlet"
        [ bench "syntax tree" $ nf S.parseDocuments bs
        , bench "nodes" $ nf (decodeInput >=> decodeNodes) bs
        ]
    , bgroup
        "HsYAML"
        [ bench "events" $ nf HE.parseEvents lazy
        , bench "nodes" $ nf (either (const ()) (foldMap (\(H.Doc n) -> forceNode n)) . H.decodeNode) lazy
        ]
    , bgroup
        "yaml"
        [bench "aeson value" $ nf (either (const Nothing) Just . Y.decodeEither' @J.Value) bs]
    ]
  where
    lazy :: BL.ByteString
    lazy = BL.fromStrict bs

-- | The benchmark that renders the syntax tree of an input.
rendering :: String -> BS.ByteString -> Benchmark
rendering name bs = bgroup name [bench "yamlet" $ nf (S.renderSyntax S.defaultRenderOptions) trees]
  where
    trees :: [S.Document]
    trees = either (error . show) id $ S.parseDocuments bs

-- | The benchmarks that decode an input into a value of the type, and the
-- given benchmarks for other libraries.
decoding
  :: forall a
   . (NFData a, FromYaml a, H.FromYAML a, J.FromJSON a)
  => String
  -> BS.ByteString
  -> [Benchmark]
  -> Benchmark
decoding name bs others =
  bgroup name $
    [ bench "yamlet" $ nf (either (const Nothing) Just . decode @a) bs
    , bench "HsYAML" $ nf (either (const Nothing) Just . H.decode1Strict @a) bs
    , bench "yaml" $ nf (either (const Nothing) Just . Y.decodeEither' @a) bs
    ]
      ++ others

-- | The benchmarks that encode the value of an input, decoded as the type,
-- and the given benchmarks for other libraries.
encoding
  :: forall a
   . (NFData a, FromYaml a, ToYaml a, H.ToYAML a, J.ToJSON a)
  => String
  -> BS.ByteString
  -> [Benchmark]
  -> Benchmark
encoding name bs others =
  bgroup name $
    [ bench "yamlet" $ nf encode value
    , bench "HsYAML" $ nf H.encode1Strict value
    , bench "yaml" $ nf Y.encode value
    ]
      ++ others
  where
    value :: a
    value = either (error . show) id $ decode bs

-- | The benchmark that decodes a JSON input with aeson.
aesonDecoding :: forall a. (NFData a, J.FromJSON a) => BS.ByteString -> Benchmark
aesonDecoding bs = bench "aeson" $ nf (J.decodeStrict' @a) bs

-- | The benchmark that encodes the value of a JSON input as JSON with aeson.
aesonEncoding :: forall a. (NFData a, FromYaml a, J.ToJSON a) => BS.ByteString -> Benchmark
aesonEncoding bs = bench "aeson" $ nf J.encode value
  where
    value :: a
    value = either (error . show) id $ decode bs

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
