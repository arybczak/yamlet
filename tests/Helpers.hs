-- | The helpers that several test modules use.
module Helpers
  ( errorPlace
  , errorOf
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
-- measurement gave the limit. A test must stay well below it also on CI,
-- which runs the tests several times slower than a fast local machine, so a
-- slow test gets a smaller input, not a higher limit.
slow :: TestTree -> TestTree
slow = localOption (mkTimeout 10000000)

-- | The line, the column and the message of an error.
errorPlace :: Error -> (Int, Int, String)
errorPlace err = (err.location.line, err.location.column, err.message)

-- | The line, the column and the message of the only error.
errorOf :: Either (NE.NonEmpty Error) a -> Maybe (Int, Int, String)
errorOf = \case
  Left (err NE.:| []) -> Just (errorPlace err)
  Left errs ->
    error $ "expected one error, but got " ++ show (map (.message) (NE.toList errs))
  Right _ -> Nothing

-- | The line, the column and the message of each error.
errorsOf :: Either (NE.NonEmpty Error) a -> [(Int, Int, String)]
errorsOf = \case
  Left errs -> map errorPlace (NE.toList errs)
  Right _ -> []

-- | Encoding a value and decoding the result gives the same value.
roundTrip :: (Eq a, Show a, ToYaml a, FromYaml a) => String -> a -> Assertion
roundTrip preface x =
  assertEqual
    preface
    (Right x)
    (decodeText (encodeText x))

-- | A resolution of 1/40, which needs three places after the point.
data Fortieths

instance HasResolution Fortieths where
  resolution _ = 40

-- | A resolution of 1/3, which has no exact decimal form.
data Thirds

instance HasResolution Thirds where
  resolution _ = 3
