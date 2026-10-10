module Main (main) where

import Data.ByteString qualified as BS
import Test.Tasty.Bench

import Yamlet.Bench.Derive
import Yamlet.Bench.Inputs
import Yamlet.Bench.Libraries

main :: IO ()
main = do
  mapM_ (uncurry printSize) inputs
  checkDerived
  defaultMain $ libraryBenchmarks ++ [deriveBenchmarks]
  where
    printSize :: String -> BS.ByteString -> IO ()
    printSize name bs =
      putStrLn $ name ++ ": " ++ show (BS.length bs `div` 1024) ++ " KiB"
