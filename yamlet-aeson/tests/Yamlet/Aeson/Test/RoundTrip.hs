module Yamlet.Aeson.Test.RoundTrip
  ( roundTripTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Yamlet

import Yamlet.Aeson ()
import Yamlet.Aeson.Test.Helpers

roundTripTests :: TestTree
roundTripTests = testProperty "round trip" prop_roundTrip

prop_roundTrip :: Property
prop_roundTrip = forAll genValue $ \v -> decodeText (encodeText v) === Right v
