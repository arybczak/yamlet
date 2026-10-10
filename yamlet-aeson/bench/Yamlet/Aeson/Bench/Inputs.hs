-- | The generated YAML inputs of the benchmarks, the same as in the
-- benchmarks of yamlet.
module Yamlet.Aeson.Bench.Inputs
  ( configInput
  , jsonInput
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

-- | A block sequence of block mappings, as in a configuration file.
configInput :: BS.ByteString
configInput = T.encodeUtf8 . T.concat $ map record [1 .. 5000]
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
jsonInput :: BS.ByteString
jsonInput = T.encodeUtf8 $ "[" <> T.intercalate ",\n " (map record [1 .. 5000]) <> "]\n"
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

num :: Int -> T.Text
num = T.pack . show
