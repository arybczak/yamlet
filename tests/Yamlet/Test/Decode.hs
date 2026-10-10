module Yamlet.Test.Decode (decodeTests) where

import Test.Tasty

import Yamlet.Test.Decode.Errors
import Yamlet.Test.Decode.Input
import Yamlet.Test.Decode.Limits
import Yamlet.Test.Decode.Scalars
import Yamlet.Test.Decode.SyntaxErrors
import Yamlet.Test.Decode.Values

decodeTests :: TestTree
decodeTests =
  testGroup
    "Decode"
    [ scalarTests
    , valueTests
    , inputTests
    , limitTests
    , syntaxErrorTests
    , errorTests
    ]
