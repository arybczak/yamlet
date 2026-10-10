module Yamlet.Test.Decode.Scalars
  ( scalarTests
  ) where

import Control.Monad
import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Yamlet
import Yamlet.Schema
import Yamlet.Test.Helpers

scalarTests :: TestTree
scalarTests =
  testGroup
    "scalars"
    [ testCase "core schema" test_coreSchema
    , testProperty "floats" prop_floats
    , testCase "exact floats" test_exactFloats
    , testCase "plain scalars" test_plainSafe
    , testCase "values" test_values
    , testCase "block scalars" test_blockScalars
    , slow $ testCase "time" test_time
    ]

test_coreSchema :: Assertion
test_coreSchema = do
  case decodeText @[Value]
    "[null, ~, '', true, False, 12, -0, 0o17, 0x1f, 1.5, -.inf, .nan, 1e3, +12, .5, a, '1']" of
    Left err -> assertFailure (show err)
    Right ns ->
      assertEqual
        "values"
        [ Null
        , Null
        , String ""
        , Bool True
        , Bool False
        , Int 12
        , Int 0
        , Int 15
        , Int 31
        , Float (Finite 1.5)
        , Float NegativeInfinity
        , Float NaN
        , Float (Finite 1000)
        , Int 12
        , Float (Finite 0.5)
        , String "a"
        , String "1"
        ]
        ns

test_plainSafe :: Assertion
test_plainSafe = do
  assertBool "word with a dash" $ isPlainSafe "dist-newstyle"
  assertBool "colon without a space" $ isPlainSafe "a:b"
  assertBool "flow indicators" $ isPlainSafe "a, [b]"
  assertBool "number" . not $ isPlainSafe "9.10"
  assertBool "boolean" . not $ isPlainSafe "true"
  assertBool "empty" . not $ isPlainSafe ""
  assertBool "colon and a space" . not $ isPlainSafe "a: b"
  assertBool "comment" . not $ isPlainSafe "a #b"
  assertBool "indicator" . not $ isPlainSafe "*a"
  assertBool "line break" . not $ isPlainSafe "a\nb"
  assertBool "string" $ isPlainString "9.10.3"
  assertBool "string with a colon and a space" $ isPlainString "a: b"
  assertBool "string number" . not $ isPlainString "9.10"
  assertBool "string null" . not $ isPlainString "~"

-- | A decimal number resolves to its exact value, and 'withFloat' gives the
-- same double as 'read'.
prop_floats :: Property
prop_floats = forAll genDecimal $ \s ->
  resolvePlain (T.pack s)
    === Float (Finite (read s))
    .&&. decodeText @Double (T.pack s)
      === Right (read s)
  where
    genDecimal :: Gen String
    genDecimal = do
      int <- digits
      frac <- digits
      ex <- oneof [pure "", ("e" ++) . show <$> choose @Int (-30, 30)]
      pure $ int ++ "." ++ frac ++ ex

    digits :: Gen String
    digits = do
      k <- choose (1, 20)
      vectorOf k (elements ['0' .. '9'])

test_exactFloats :: Assertion
test_exactFloats = do
  assertEqual
    "one tenth"
    (Right (Sci.scientific 1 (-1)))
    (decodeText @Sci.Scientific "0.1")
  assertEqual
    "more digits than a double holds"
    (Right (Sci.scientific 12345678901234567890123 (-3)))
    (decodeText @Sci.Scientific "12345678901234567890.123")
  assertEqual
    "integer as a scientific"
    (Right (Sci.scientific 42 0))
    (decodeText @Sci.Scientific "42")
  assertEqual
    "largest exponent"
    (Right (Sci.scientific 99 999))
    (decodeText @Sci.Scientific "9.9e1000")
  assertEqual
    "smallest exponent"
    (Right (Sci.scientific 15 (-1001)))
    (decodeText @Sci.Scientific "1.5e-1000")
  assertEqual
    "large exponent as a double"
    (Right (1 / 0))
    (decodeText @Double "1e1000")
  -- 1 + 2^-24 + 2^-60 is nearest to the float 1 + 2^-23, but the nearest
  -- double is 1 + 2^-24, a tie between two floats that rounds to 1.
  assertEqual
    "float without double rounding"
    (Right (1 + 2 ^^ (-23 :: Int)))
    (decodeText @Float "1.000000059604644776257986737988403547205962240695953369140625")
  let numbers =
        [ "1e1001"
        , "10e1000"
        , "0.1e-1000"
        , "1" <> T.replicate 1001 "0" <> ".0"
        , "1e99999999999999999999"
        , "11e9223372036854775807"
        ]
  forM_ numbers $ \number ->
    assertEqual
      ("exponent beyond the limit in " ++ show number)
      ( Just
          ( 1
          , 2
          , "the exponent of the number is out of the range from -1000 to 1000, quote the value if it is a string, e.g. '"
              ++ T.unpack number
              ++ "'"
          )
      )
      (errorOf (decodeText @Sci.Scientific ("[" <> number <> "]")))
  assertEqual
    "exponent beyond the limit for a string"
    ( Just
        ( 1
        , 9
        , "the exponent of the number is out of the range from -1000 to 1000, quote the value if it is a string, e.g. '61e9540'"
        )
    )
    (errorOf (decodeText @(M.Map T.Text T.Text) "gitsha: 61e9540"))
  assertEqual
    "exponent beyond the limit with a tag"
    (Just (1, 9, "the exponent of the number is out of the range from -1000 to 1000"))
    (errorOf (decodeText @Double "!!float 1e-99999999999999999999"))
  assertEqual
    "exponent beyond the limit in the text, value within it"
    (Right (Sci.scientific 1 997))
    (decodeText @Sci.Scientific "0.0001e1001")
  assertEqual
    "exponent beyond the limit in the schema"
    [Float Infinity, Float (Finite 0), Float (Finite 0)]
    (map resolvePlain ["1e1001", "1e-1001", "0e99999999999999999999"])
  assertEqual
    "zero with an exponent beyond the limit"
    (Right 0)
    (decodeText @Double "0e99999999999999999999")
  assertEqual
    "negative zero"
    (Right [Float NegativeZero, Float NegativeZero, Float (Finite 0), Int 0])
    (decodeText @[Value] "[-0.0, !!float -0, 0.0, -0]")
  assertEqual
    "integer with a float tag"
    (Right 12)
    (decodeText @Double "!!float 12")
  forM_ ["0x10", "0o10"] $ \t ->
    assertEqual
      ("integer in another base with a float tag, " ++ show t)
      (Just (1, 9, "invalid value for the tag !!float"))
      (errorOf (decodeText @Double ("!!float " <> t)))
  assertEqual
    "negative zero as a double"
    (Right True)
    (isNegativeZero <$> decodeText @Double "-0.0")
  assertEqual
    "negative zero as a scientific"
    (Right 0)
    (decodeText @Sci.Scientific "-0.0")
  assertEqual
    "negative and positive zero keys"
    (Right [Float (Finite 0), Float NegativeZero])
    $ (\case Mapping kvs -> map fst kvs; v -> [v])
      <$> decodeText @Value "{0.0: a, -0.0: b}"
  assertEqual
    "infinity as a scientific"
    (Just (1, 1, "expected a finite number"))
    (errorOf (decodeText @Sci.Scientific ".inf"))

-- | Edge cases of block scalars that the specification leaves unclear.
test_blockScalars :: Assertion
test_blockScalars = do
  -- libyaml and the JavaScript package yaml give the same result.
  assertEqual
    "indentation indicator at the top level"
    (Right " a\n")
    (decodeText @T.Text "--- |1\n  a\n")
  assertEqual
    "indentation indicator without a marker"
    (Right " a\n")
    (decodeText @T.Text "|2\n   a\n")
  -- The end of the input ends a last line of spaces, as in the test JEF9/02
  -- of the YAML test suite.
  assertEqual
    "keep with spaces at the end"
    (Right "a\n\n")
    (decodeText @T.Text "|+\n  a\n  ")
  assertEqual
    "keep with an empty line and spaces at the end"
    (Right "a\n\n\n")
    (decodeText @T.Text "|+\n  a\n\n  ")
  assertEqual
    "keep with a line break at the end"
    (Right "a\n\n")
    (decodeText @T.Text "|+\n  a\n  \n")

test_values :: Assertion
test_values = do
  assertEqual
    "mapping"
    (Right (Mapping [(String "a", Sequence [Int 1, Int 2])]))
    (decodeText @Value "a: [1, 2]")
  assertEqual
    "tags"
    ( Right $
        Sequence
          [ Tagged "!point" (Mapping [(String "x", Int 1)])
          , Tagged "!secret" (String "abc")
          , Int 1
          ]
    )
    (decodeText @Value "- !point {x: 1}\n- !secret abc\n- !!int 1\n")

test_time :: Assertion
test_time = do
  assertEqual
    "day"
    (Right (fromGregorian 2026 9 25))
    (decodeText "2026-09-25")
  assertEqual
    "invalid day"
    (Just (1, 1, "expected a date such as 2026-09-25"))
    (errorOf (decodeText @Day "2026-02-30"))
  assertEqual
    "invalid month"
    (Just (1, 1, "expected a month such as 2026-09"))
    (errorOf (decodeText @Month "2026-13"))
  assertEqual
    "uppercase quarter"
    (Right (YearQuarter 2026 Q3))
    (decodeText "2026-Q3")
  assertEqual
    "invalid quarter"
    (Just (1, 1, "expected a quarter such as 2026-q3"))
    (errorOf (decodeText @Quarter "2026-q5"))
  assertEqual
    "day of the week in another case"
    (Right Friday)
    (decodeText "FriDay")
  assertEqual
    "invalid day of the week"
    (Just (1, 1, "expected a day of the week such as monday"))
    (errorOf (decodeText @DayOfWeek "mon"))
  assertEqual
    "unknown key of calendar days"
    (Just (1, 22, "unknown key \"weeks\", expected one of: months, days"))
    (errorOf (decodeText @CalendarDiffDays "{months: 1, days: 2, weeks: 3}"))
  assertEqual
    "short year"
    (Just (1, 1, "expected a date such as 2026-09-25"))
    (errorOf (decodeText @Day "26-09-25"))
  assertEqual
    "time without seconds"
    (Right (TimeOfDay 12 30 0))
    (decodeText "12:30")
  assertEqual
    "time with a fraction"
    (Right (TimeOfDay 12 30 5.25))
    (decodeText "12:30:05.25")
  assertEqual
    "fraction of 13 digits"
    (Just (1, 1, "expected a time such as 12:30:00"))
    (errorOf (decodeText @TimeOfDay "12:30:05.1234567890123"))
  assertEqual
    "end of a day"
    (Right (TimeOfDay 24 0 0))
    (decodeText "24:00")
  assertEqual
    "invalid time"
    (Just (1, 1, "expected a time such as 12:30:00"))
    (errorOf (decodeText @TimeOfDay "24:01"))
  let noon = LocalTime (fromGregorian 2026 9 25) (TimeOfDay 12 30 0)
  assertEqual
    "local time with T"
    (Right noon)
    (decodeText "2026-09-25T12:30:00")
  assertEqual
    "local time with a space"
    (Right noon)
    (decodeText "2026-09-25 12:30")
  let utcNoon = UTCTime (fromGregorian 2026 9 25) (12 * 3600 + 30 * 60)
  assertEqual
    "UTC time"
    (Right utcNoon)
    (decodeText "2026-09-25T12:30:00Z")
  assertEqual
    "UTC time from an offset"
    (Right utcNoon)
    (decodeText "2026-09-25T14:30:00+02:00")
  assertEqual
    "offset without a colon"
    (Right utcNoon)
    (decodeText "2026-09-25T14:30:00+0200")
  assertEqual
    "space before an offset"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25T14:30:00 +02:00"))
  assertEqual
    "offset in hours"
    (Right utcNoon)
    (decodeText "2026-09-25T10:30:00-02")
  assertEqual
    "lowercase separator"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25t12:30:00Z"))
  assertEqual
    "lowercase zone"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25T12:30:00z"))
  assertEqual
    "large offset"
    (Right utcNoon)
    (decodeText "2026-09-26T12:29:00+23:59")
  assertEqual
    "offset beyond a day"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25T12:30:00+24:00"))
  assertEqual
    "time without a time zone"
    (Just (1, 1, "expected a date, a time and a time zone such as 2026-09-25T12:30:00Z"))
    (errorOf (decodeText @UTCTime "2026-09-25T12:30:00"))
  assertEqual
    "zoned time"
    (Right (noon, 120))
    $ (\z -> (zonedTimeToLocalTime z, timeZoneMinutes (zonedTimeZone z)))
      <$> decodeText "2026-09-25T12:30:00+02:00"
  assertEqual
    "duration"
    (Right 1.5)
    (decodeText @NominalDiffTime "1.5")
  assertEqual
    "whole duration"
    (Right 60)
    (decodeText @DiffTime "60")
  assertEqual
    "picosecond"
    (Right (picosecondsToDiffTime 1))
    (decodeText "1e-12")
  assertEqual
    "tiny duration"
    (Right 0)
    (decodeText @DiffTime "1e-1000")
  assertEqual
    "largest duration"
    (Right (10 ^ (1000 :: Int)))
    (decodeText @NominalDiffTime "1e1000")
  assertEqual
    "integer duration beyond the limit of floats"
    (Right (10 ^ (1001 :: Int)))
    (decodeText @NominalDiffTime ("1" <> T.replicate 1001 "0"))
  forM_ [minBound, maxBound - 11, maxBound] $ \ex ->
    assertEqual
      ("duration with the exponent " ++ show ex)
      (Left "the exponent of the number is out of the range from -1000 to 1000")
      . first (snd . NE.head)
      $ runParser
        (parseYaml @NominalDiffTime)
        (toYaml (Float (Finite (Sci.scientific 1 ex))))
  assertEqual
    "zero duration with a large exponent"
    (Right 0)
    $ runParser
      (parseYaml @DiffTime)
      (toYaml (Float (Finite (Sci.scientific 0 maxBound))))
