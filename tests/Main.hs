module Main (main) where

import Test.Tasty

import DecodeTests
import EncodeTests
import TestSuite

main :: IO ()
main = do
  suite <- testSuiteTests
  defaultMain $ testGroup "yamlet"
    [ decodeTests
    , encodeTests
    , suite
    ]
