module Main (main) where

import Data.ByteString qualified as BS
import Test.Tasty.Bench

import Yamlet.Aeson.Bench.Inputs
import Yamlet.Aeson.Bench.Libraries

main :: IO ()
main = do
  printSize "config" configInput
  printSize "json" jsonInput
  defaultMain libraryBenchmarks
  where
    printSize :: String -> BS.ByteString -> IO ()
    printSize name bs =
      putStrLn $ name ++ ": " ++ show (BS.length bs `div` 1024) ++ " KiB"
