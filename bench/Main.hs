module Main (main) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Test.Tasty.Bench

import Derive
import Inputs
import Libraries
import Types

-- | The benchmarks are grouped by the operation, so that the times of the
-- libraries for one operation and input are next to each other.
main :: IO ()
main = do
  mapM_ (uncurry printSize) inputs
  checkDerived
  defaultMain
    [ bgroup "parse" (map (uncurry parsing) inputs)
    , bgroup "render" (map (uncurry rendering) inputs)
    , bgroup
        "decode"
        [ decoding @[Config] "config" configInput []
        , decoding @[Json] "json" jsonInput [aesonDecoding @[Json] jsonInput]
        , decoding @(M.Map T.Text T.Text) "text" textInput []
        ]
    , bgroup
        "encode"
        [ encoding @[Config] "config" configInput []
        , encoding @[Json] "json" jsonInput [aesonEncoding @[Json] jsonInput]
        , encoding @(M.Map T.Text T.Text) "text" textInput []
        ]
    , derived
    ]
  where
    printSize :: String -> BS.ByteString -> IO ()
    printSize name bs = putStrLn $ name ++ ": " ++ show (BS.length bs `div` 1024) ++ " KiB"

    -- The inputs of the benchmarks that do not decode to a type.
    inputs :: [(String, BS.ByteString)]
    inputs = [("config", configInput), ("json", jsonInput), ("text", textInput)]

    configInput :: BS.ByteString
    configInput = T.encodeUtf8 $ config 5000

    jsonInput :: BS.ByteString
    jsonInput = T.encodeUtf8 $ json 5000

    textInput :: BS.ByteString
    textInput = T.encodeUtf8 $ text 2000
