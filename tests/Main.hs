module Main (main) where

import Test.Tasty

import Yamlet.Test.Decode
import Yamlet.Test.Encode
import Yamlet.Test.Generic
import Yamlet.Test.Render
import Yamlet.Test.TypeError
import Yamlet.Test.YamlTestSuite

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
