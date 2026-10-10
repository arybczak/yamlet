-- | The generated YAML inputs of the benchmarks.
module Yamlet.Bench.Inputs
  ( inputs
  , configInput
  , jsonInput
  , textInput
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

-- | The inputs of the benchmarks that do not decode to a type.
inputs :: [(String, BS.ByteString)]
inputs = [("config", configInput), ("json", jsonInput), ("text", textInput)]

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

-- | Block scalars and multi-line plain scalars.
textInput :: BS.ByteString
textInput = T.encodeUtf8 . T.concat $ map entry [1 .. 2000]
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
