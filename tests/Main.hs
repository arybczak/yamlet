module Main (main) where

import Test.Tasty

import DecodeTests
import EncodeTests
import GenericTests
import RenderTests
import TestSuite
import TypeErrorTests

main :: IO ()
main = do
  suite <- testSuiteTests
  defaultMain $
    testGroup
      "yamlet"
      [ decodeTests
      , encodeTests
      , genericTests
      , renderTests
      , typeErrorTests
      , suite
      ]
