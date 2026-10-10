module Yamlet.Test.Render (renderTests) where

import Test.Tasty

import Yamlet.Test.Render.Attachment
import Yamlet.Test.Render.Comments
import Yamlet.Test.Render.Documents
import Yamlet.Test.Render.Properties
import Yamlet.Test.Render.Styles

renderTests :: TestTree
renderTests =
  testGroup
    "Render"
    [ styleTests
    , documentTests
    , attachmentTests
    , commentTests
    , propertyTests
    ]
