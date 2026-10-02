module Main (main) where

import Test.Tasty

import DecodeTests
import EncodeTests
import GenericTests
import RenderTests
import RetentionTests
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
      , retentionTests
      , typeErrorTests
      , suite
      ]
