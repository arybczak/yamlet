module Yamlet.Test.Encode (encodeTests) where

import Test.Tasty

import Yamlet.Test.Encode.Comments
import Yamlet.Test.Encode.Properties
import Yamlet.Test.Encode.Values

encodeTests :: TestTree
encodeTests =
  testGroup
    "encode"
    [ valueTests
    , commentTests
    , propertyTests
    ]
