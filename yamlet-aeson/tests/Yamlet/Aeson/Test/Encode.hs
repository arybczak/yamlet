module Yamlet.Aeson.Test.Encode
  ( encodeTests
  ) where

import Control.Exception
import Data.Aeson qualified as A
import Data.Aeson.Encoding qualified as E
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.String
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Yamlet

import Yamlet.Aeson
import Yamlet.Aeson.Test.Helpers

encodeTests :: TestTree
encodeTests =
  testGroup
    "encode"
    [ testCase "order of fields" test_fieldOrder
    , testCase "polymorphic value" test_polymorphic
    , testCase "keys that look like numbers" test_numberKeys
    , testCase "special floats" test_encodeSpecialFloats
    , testCase "zeros" test_zeros
    , testCase "duplicate keys" test_duplicateKeys
    , testCase "invalid encoding" test_invalidEncoding
    , testProperty "value as its encoding" prop_valueAsEncoding
    ]

test_fieldOrder :: Assertion
test_fieldOrder = do
  assertEqual
    "the order of the declaration"
    "- port: 80\n  host: localhost\n"
    (encodeText (ViaAeson [Server 80 "localhost"]))
  assertEqual
    "the order of an aeson object without toEncoding"
    "- host: localhost\n  port: 80\n"
    (encodeText (ViaAeson [Unordered 80 "localhost"]))

test_polymorphic :: Assertion
test_polymorphic =
  assertEqual
    "the output"
    "- 1\n"
    (encodeAny @[Int] [1])
  where
    encodeAny :: A.ToJSON a => a -> T.Text
    encodeAny = encodeText . ViaAeson

test_numberKeys :: Assertion
test_numberKeys = do
  let m = M.fromList @Int @Char [(1, 'a'), (2, 'b')]
  assertEqual
    "the keys in quotes"
    "'1': a\n'2': b\n"
    (encodeText (ViaAeson m))
  assertEqual
    "the keys read back"
    (Right (ViaAeson m))
    (decodeText (encodeText (ViaAeson m)))

test_encodeSpecialFloats :: Assertion
test_encodeSpecialFloats = do
  let ds = [1 / 0, -(1 / 0), 0 / 0, -0.0, 1.0 :: Double]
  assertEqual
    "the values of aeson"
    "- +inf\n- -inf\n- null\n- 0.0\n- 1.0\n"
    (encodeText (ViaAeson ds))
  assertEqual
    "the values read back as from JSON"
    (Right (map show <$> A.decode @[Double] (A.encode ds)))
    $ Just . map show . (.value)
      <$> decodeText @(ViaAeson [Double]) (encodeText (ViaAeson ds))

test_zeros :: Assertion
test_zeros =
  assertEqual
    "a float zero stays a float"
    (Right "- 0.0\n- 0.0\n- 0\n- 1.0\n")
    (encodeText <$> decodeText @A.Value "[0.0, -0.0, 0, 1.0]")

test_duplicateKeys :: Assertion
test_duplicateKeys =
  assertEqual
    "the first key stays"
    "a: 1\nb: 3\n"
    (encodeText (ViaAeson Twice))

test_invalidEncoding :: Assertion
test_invalidEncoding = do
  -- The messages of the lexer of aeson can change between its versions, so
  -- only their start is fixed.
  assertInvalid
    "an incomplete value"
    "Unexpected"
    (Raw "{")
  assertInvalid
    "content after the value"
    "unexpected \"} [2]\" after the value"
    (Raw "{\"a\":1}} [2]")
  assertInvalid
    "an invalid value in a list"
    "Unexpected"
    [Raw "1", Raw "x"]
  assertEqual
    "spaces after the value"
    "a: 1\n"
    (encodeText (ViaAeson (Raw "{\"a\":1} \n")))
  where
    -- The message starts with the prefix of all such errors and the given
    -- text.
    assertInvalid :: A.ToJSON a => String -> String -> a -> Assertion
    assertInvalid preface expected x =
      try (evaluate (encodeText (ViaAeson x))) >>= \case
        Left (ErrorCall msg) ->
          assertBool
            (preface ++ ": the error of the encoding: " ++ msg)
            ((prefix ++ expected) `L.isPrefixOf` msg)
        Right out -> assertFailure (preface ++ ": no error, the output is " ++ show out)
      where
        prefix :: String
        prefix = "Yamlet.Aeson.ViaAeson: the toEncoding is not valid JSON: "

prop_valueAsEncoding :: Property
prop_valueAsEncoding = forAll genValue $ \v -> toYaml (ViaAeson v) === toYaml v

-- | An instance with the default 'A.toEncoding', which goes through
-- 'A.toJSON'.
data Unordered = Unordered {port :: Int, host :: T.Text}
  deriving stock (Generic)
  deriving anyclass (A.ToJSON)

-- | An encoding with the key @a@ twice.
data Twice = Twice

instance A.ToJSON Twice where
  toJSON _ = A.object ["a" A..= (1 :: Int), "b" A..= (3 :: Int)]
  toEncoding _ =
    A.pairs ("a" A..= (1 :: Int) <> "a" A..= (2 :: Int) <> "b" A..= (3 :: Int))

-- | An encoding of the given text.
newtype Raw = Raw String

instance A.ToJSON Raw where
  toJSON (Raw s) = A.toJSON s
  toEncoding (Raw s) = E.unsafeToEncoding (fromString s)
