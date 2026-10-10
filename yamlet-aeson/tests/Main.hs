module Main (main) where

import Test.Tasty

import Yamlet.Aeson.Test.Decode
import Yamlet.Aeson.Test.Encode
import Yamlet.Aeson.Test.RoundTrip

main :: IO ()
main =
  defaultMain $
    testGroup
      "yamlet-aeson"
      [ decodeTests
      , encodeTests
      , roundTripTests
      ]
