-- | The helpers that several test modules use.
module Yamlet.Aeson.Test.Helpers
  ( errorsOf
  , genValue
  , Server (..)
  ) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.List.NonEmpty qualified as NE
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Tasty.QuickCheck
import Yamlet

-- | A value with numbers that yamlet reads back, i.e. with an exponent from
-- -1000 to 1000.
genValue :: Gen A.Value
genValue = sized go
  where
    go :: Int -> Gen A.Value
    go n
      | n <= 1 = scalar
      | otherwise =
          oneof
            [ scalar
            , A.Array . V.fromList <$> children go
            , A.object <$> children (\m -> (A..=) . K.fromText <$> text <*> go m)
            ]
      where
        -- The children share the size, so that a value has about as many
        -- nodes as the size.
        children :: (Int -> Gen a) -> Gen [a]
        children gen = do
          k <- choose (0, 5)
          vectorOf k (gen (n `div` (k + 1)))

    scalar :: Gen A.Value
    scalar =
      oneof
        [ pure A.Null
        , A.Bool <$> arbitrary
        , A.Number . fromInteger <$> arbitrary
        , A.Number <$> (Sci.scientific <$> arbitrary <*> choose (-1000, 1000))
        , A.String <$> text
        ]

    text :: Gen T.Text
    text = T.pack <$> arbitrary

-- | The line, the column and the message of each error.
errorsOf :: Either (NE.NonEmpty Error) a -> [(Int, Int, String)]
errorsOf = \case
  Left errs ->
    [(err.location.line, err.location.column, err.message) | err <- NE.toList errs]
  Right _ -> []

data Server = Server {port :: Int, host :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (A.FromJSON)

instance A.ToJSON Server where
  toEncoding = A.genericToEncoding A.defaultOptions
