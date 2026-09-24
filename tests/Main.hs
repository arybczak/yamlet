module Main (main) where

import Test.Tasty

import DecodeTests
import EncodeTests
import RenderTests
import TestSuite

main :: IO ()
main = do
  suite <- testSuiteTests
  defaultMain $ testGroup "yamlet"
    [ decodeTests
    , encodeTests
    , renderTests
    , suite
    ]
