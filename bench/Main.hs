module Main (main) where

import Control.Monad
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.YAML qualified as H
import Data.YAML.Event qualified as HE
import Data.Yaml qualified as Y
import Test.Tasty.Bench

import Yamlet
import Yamlet.Syntax qualified as S

main :: IO ()
main =
  defaultMain
    [ input "records" $ records 5000
    , input "flow" $ flow 5000
    , input "text" $ text 2000
    ]

input :: String -> T.Text -> Benchmark
input name t = env (pure (bs, bl)) $ \ ~(strict, lazy) ->
  bgroup
    (name ++ " (" ++ show (BS.length bs `div` 1024) ++ " KiB)")
    [ bench "yamlet (syntax)" $ nf S.parseDocuments strict
    , bench "yamlet (nodes)" $ nf (decodeInput >=> decodeNodes) strict
    , bench "HsYAML (events)" $ nf HE.parseEvents lazy
    , bench "HsYAML (nodes)" $ nf (either (const ()) (foldMap (\(H.Doc n) -> forceNode n)) . H.decodeNode) lazy
    , bench "yaml (libyaml)" $ nf (either (const Nothing) Just . Y.decodeEither' @J.Value) strict
    ]
  where
    bs :: BS.ByteString
    bs = T.encodeUtf8 t

    bl :: BL.ByteString
    bl = BL.fromStrict bs

-- | A block sequence of block mappings, as in a configuration file.
records :: Int -> T.Text
records n = T.concat $ map record [1 .. n]
  where
    record :: Int -> T.Text
    record i =
      T.unlines
        [ "- name: item " <> num i
        , "  id: " <> num i
        , "  tags: [alpha, beta, gamma]"
        , "  description: \"an \\\"escaped\\\" string\\twith a tab\""
        , "  path: /usr/local/share/item-" <> num i
        , "  enabled: true"
        , "  nested:"
        , "    x: 1.5"
        , "    y: -3"
        , "    list:"
        , "      - one"
        , "      - 'two'"
        ]

-- | JSON-like flow collections.
flow :: Int -> T.Text
flow n = "[" <> T.intercalate ",\n " (map record [1 .. n]) <> "]\n"
  where
    record :: Int -> T.Text
    record i =
      T.concat
        [ "{\"id\": "
        , num i
        , ", \"name\": \"item "
        , num i
        , "\", \"values\": [1, 2.5, true, null], \"child\": {\"a\": \"b\"}}"
        ]

-- | Block scalars and multi-line plain scalars.
text :: Int -> T.Text
text n = T.concat $ map entry [1 .. n]
  where
    entry :: Int -> T.Text
    entry i =
      T.unlines
        [ "key" <> num i <> ": |"
        , "  Lorem ipsum dolor sit amet, consectetur adipiscing elit."
        , "  Sed do eiusmod tempor incididunt ut labore et dolore."
        , ""
        , "    Ut enim ad minim veniam, quis nostrud exercitation."
        , "folded" <> num i <> ": >-"
        , "  Duis aute irure dolor in reprehenderit in voluptate velit"
        , "  esse cillum dolore eu fugiat nulla pariatur."
        , "plain" <> num i <> ": Excepteur sint occaecat cupidatat non proident,"
        , "  sunt in culpa qui officia deserunt mollit anim id est laborum."
        ]

num :: Int -> T.Text
num = T.pack . show

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
