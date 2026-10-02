-- | The helpers that several test modules use.
module Helpers
  ( errorOf
  , errorsOf
  , roundTrip
  , slow
  , Fortieths
  , Thirds
  ) where

import Data.Fixed
import Data.List.NonEmpty qualified as NE
import Test.Tasty
import Test.Tasty.HUnit

import Yamlet

-- | The time limit of a test on a large input, so that a regression to
-- quadratic time fails the test instead of stalling the suite. No
-- measurement gave the limit. It is far above the run times, which were at
-- most 4.2 s for the test of many keys and below 2.5 s for the others, with
-- GHC 9.10.3.
slow :: TestTree -> TestTree
slow = localOption (mkTimeout 10000000)

-- | The line, the column and the message of the only error.
errorOf :: Either (NE.NonEmpty Error) a -> Maybe (Int, Int, String)
errorOf = \case
  Left (err NE.:| []) -> Just (err.location.line, err.location.column, err.message)
  Left errs -> error $ "expected one error, but got " ++ show (map (.message) (NE.toList errs))
  Right _ -> Nothing

-- | The line, the column and the message of each error.
errorsOf :: Either (NE.NonEmpty Error) a -> [(Int, Int, String)]
errorsOf = \case
  Left errs -> [(err.location.line, err.location.column, err.message) | err <- NE.toList errs]
  Right _ -> []

-- | Encoding a value and decoding the result gives the same value.
roundTrip :: (Eq a, Show a, ToYaml a, FromYaml a) => String -> a -> Assertion
roundTrip preface x = assertEqual preface (Right x) (decodeText (encodeText x))

-- | A resolution of 1/40, which needs three places after the point.
data Fortieths

instance HasResolution Fortieths where
  resolution _ = 40

-- | A resolution of 1/3, which has no exact decimal form.
data Thirds

instance HasResolution Thirds where
  resolution _ = 3
