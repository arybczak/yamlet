-- | Conversion of Haskell values to nodes.
module Yamlet.Encode
  ( -- * Class
    ToYaml (..)
  , (.=)
  , mapping
  ) where

import Yamlet.Internal.ToYaml
