{-# OPTIONS_HADDOCK not-home #-}

-- | Dates and times in the text formats of aeson, which follow ISO 8601.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Time
  ( -- * Parsing
    parseDay
  , parseMonth
  , parseQuarter
  , parseQuarterOfYear
  , parseTimeOfDay
  , parseLocalTime
  , parseZonedTime
  , parseUTCTime
  , picoseconds

    -- * Formatting
  , formatDay
  , formatMonth
  , formatQuarter
  , formatQuarterOfYear
  , formatTimeOfDay
  , formatLocalTime
  , formatZonedTime
  , formatUTCTime
  ) where

import Control.Monad
import Data.Char
import Data.Fixed
import Data.Maybe
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter

----------------------------------------
-- Parsing

-- | @[+-]YYYY-MM-DD@, with at least four digits in the year.
parseDay :: T.Text -> Maybe Day
parseDay = whole day

-- | @[+-]YYYY-MM@, with at least four digits in the year.
parseMonth :: T.Text -> Maybe Month
parseMonth = whole $ \t -> do
  ((y, m), rest) <- yearMonth t
  mm <- fromYearMonthValid y m
  pure (mm, rest)

-- | @[+-]YYYY-qN@, with at least four digits in the year, e.g. @2026-q3@.
parseQuarter :: T.Text -> Maybe Quarter
parseQuarter = whole $ \t0 -> do
  (y, t1) <- year t0
  t2 <- char '-' t1
  (q, t3) <- quarterOfYear t2
  pure (YearQuarter y q, t3)

-- | @q1@ to @q4@.
parseQuarterOfYear :: T.Text -> Maybe QuarterOfYear
parseQuarterOfYear = whole quarterOfYear

-- | @HH:MM[:SS[.S...]]@.
parseTimeOfDay :: T.Text -> Maybe TimeOfDay
parseTimeOfDay = whole timeOfDay

-- | A date and a time, separated by @T@, @t@ or a space.
parseLocalTime :: T.Text -> Maybe LocalTime
parseLocalTime = whole localTime

-- | A local time and a time zone.
parseZonedTime :: T.Text -> Maybe ZonedTime
parseZonedTime = whole $ \t -> do
  (lt, rest) <- localTime t
  (tz, rest') <- timeZone rest
  pure (ZonedTime lt tz, rest')

-- | A local time and a time zone, converted to UTC.
parseUTCTime :: T.Text -> Maybe UTCTime
parseUTCTime = fmap zonedTimeToUTC . parseZonedTime

-- | The whole number of picoseconds in a number of seconds, rounded down,
-- or 'Nothing' if it is out of range. The result has at most 60 digits, so
-- a huge exponent does not build a huge integer.
picoseconds :: Sci.Scientific -> Maybe Integer
picoseconds s
  | c == 0 = Just 0
  -- The check comes before the computation of k, which can overflow.
  | Sci.base10Exponent s > 48 = Nothing
  | k >= 0 = if k > 60 - digits then Nothing else Just (c * 10 ^ k)
  | -k > digits = Just (if c < 0 then -1 else 0)
  | otherwise = Just (c `div` 10 ^ negate k)
  where
    c :: Integer
    c = Sci.coefficient s

    k :: Int
    k = Sci.base10Exponent s + 12

    digits :: Int
    digits = length (show (abs c))

-- | The value of a parser if it takes the whole text.
whole :: (T.Text -> Maybe (a, T.Text)) -> T.Text -> Maybe a
whole p t = case p t of
  Just (a, rest) | T.null rest -> Just a
  _ -> Nothing

day :: T.Text -> Maybe (Day, T.Text)
day t0 = do
  ((y, m), t1) <- yearMonth t0
  t2 <- char '-' t1
  (d, t3) <- twoDigits t2
  dd <- fromGregorianValid y m d
  pure (dd, t3)

-- | @[+-]YYYY-MM@, the year and the month of a date.
yearMonth :: T.Text -> Maybe ((Integer, Int), T.Text)
yearMonth t0 = do
  (y, t1) <- year t0
  t2 <- char '-' t1
  (m, t3) <- twoDigits t2
  pure ((y, m), t3)

-- | @[+-]YYYY@, with at least four digits.
year :: T.Text -> Maybe (Integer, T.Text)
year t0 = do
  let (sign, t1) = case T.uncons t0 of
        Just ('-', t) -> (negate, t)
        Just ('+', t) -> (id, t)
        _ -> (id, t0)
      (y, t2) = T.span isDigit t1
  -- A longer year is no real date, and its value would be costly to read.
  guard $ T.length y >= 4 && T.length y <= 18
  pure (sign (T.foldl' (\acc c -> acc * 10 + toInteger (digitToInt c)) 0 y), t2)

-- | @q1@ to @q4@, in either case.
quarterOfYear :: T.Text -> Maybe (QuarterOfYear, T.Text)
quarterOfYear t = case T.unpack (T.take 2 t) of
  [q, d] | toLower q == 'q', Just qy <- lookup d [('1', Q1), ('2', Q2), ('3', Q3), ('4', Q4)] -> Just (qy, T.drop 2 t)
  _ -> Nothing

timeOfDay :: T.Text -> Maybe (TimeOfDay, T.Text)
timeOfDay t0 = do
  (h, t1) <- twoDigits t0
  t2 <- char ':' t1
  (m, t3) <- twoDigits t2
  (s, t4) <- case char ':' t3 of
    Just t -> seconds t
    Nothing -> pure (0, t3)
  tod <- makeTimeOfDayValid h m s
  pure (tod, t4)
  where
    seconds :: T.Text -> Maybe (Pico, T.Text)
    seconds t = do
      (s, rest) <- twoDigits t
      let (frac, rest') = case char '.' rest of
            Just r -> T.span isDigit r
            Nothing -> (T.empty, rest)
      guard $ not (T.null frac && T.isPrefixOf "." rest)
      -- Digits after the twelfth do not change a picosecond value.
      let ps = T.foldl' (\acc c -> acc * 10 + toInteger (digitToInt c)) 0 (T.justifyLeft 12 '0' (T.take 12 frac))
      pure (MkFixed (toInteger s * 10 ^ (12 :: Int) + ps), rest')

localTime :: T.Text -> Maybe (LocalTime, T.Text)
localTime t0 = do
  (d, t1) <- day t0
  t2 <- case T.uncons t1 of
    Just (c, t) | c == 'T' || c == 't' || c == ' ' -> Just t
    _ -> Nothing
  (tod, t3) <- timeOfDay t2
  pure (LocalTime d tod, t3)

-- | @Z@, @z@, @+HH:MM@, @+HHMM@ or @+HH@, optionally after one space.
timeZone :: T.Text -> Maybe (TimeZone, T.Text)
timeZone t0 = case T.uncons (fromMaybe t0 (char ' ' t0)) of
  Just (c, t) | c == 'Z' || c == 'z' -> Just (utc, t)
  Just (c, t1) | c == '+' || c == '-' -> do
    (h, t2) <- twoDigits t1
    (m, t3) <- case T.uncons t2 of
      Just (':', t) -> twoDigits t
      Just (d, _) | isDigit d -> twoDigits t2
      _ -> Just (0, t2)
    let offset = (if c == '-' then negate else id) (h * 60 + m)
    guard $ h <= 23 && m <= 59
    pure (minutesToTimeZone offset, t3)
  _ -> Nothing

twoDigits :: T.Text -> Maybe (Int, T.Text)
twoDigits t = case T.unpack (T.take 2 t) of
  [a, b] | isDigit a && isDigit b -> Just (digitToInt a * 10 + digitToInt b, T.drop 2 t)
  _ -> Nothing

char :: Char -> T.Text -> Maybe T.Text
char c t = case T.uncons t of
  Just (c', rest) | c' == c -> Just rest
  _ -> Nothing

----------------------------------------
-- Formatting

-- | @YYYY-MM-DD@, with a sign for a negative year.
formatDay :: Day -> T.Text
formatDay dd =
  let (y, m, d) = toGregorian dd
  in T.concat [formatYear y, "-", pad 2 (toInteger m), "-", pad 2 (toInteger d)]

-- | @YYYY-MM@, with a sign for a negative year.
formatMonth :: Month -> T.Text
formatMonth (YearMonth y m) = formatYear y <> "-" <> pad 2 (toInteger m)

-- | @YYYY-qN@, with a sign for a negative year, e.g. @2026-q3@.
formatQuarter :: Quarter -> T.Text
formatQuarter (YearQuarter y q) = formatYear y <> "-" <> formatQuarterOfYear q

-- | @q1@ to @q4@.
formatQuarterOfYear :: QuarterOfYear -> T.Text
formatQuarterOfYear = \case
  Q1 -> "q1"
  Q2 -> "q2"
  Q3 -> "q3"
  Q4 -> "q4"

-- | A year with at least four digits, and a sign if it is negative.
formatYear :: Integer -> T.Text
formatYear y
  | y < 0 = "-" <> pad 4 (negate y)
  | otherwise = pad 4 y

-- | @HH:MM:SS@, with the fraction of a second if it is not zero.
formatTimeOfDay :: TimeOfDay -> T.Text
formatTimeOfDay (TimeOfDay h m (MkFixed ps)) =
  let (s, frac) = ps `divMod` (10 ^ (12 :: Int))
      fraction
        | frac == 0 = T.empty
        | otherwise = "." <> T.dropWhileEnd (== '0') (pad 12 frac)
  in T.concat [pad 2 (toInteger h), ":", pad 2 (toInteger m), ":", pad 2 s, fraction]

formatLocalTime :: LocalTime -> T.Text
formatLocalTime (LocalTime d tod) = formatDay d <> "T" <> formatTimeOfDay tod

-- | A local time with @Z@ for a zero offset, or @+HH:MM@ for another one.
formatZonedTime :: ZonedTime -> T.Text
formatZonedTime (ZonedTime lt tz) = formatLocalTime lt <> zone
  where
    zone :: T.Text
    zone
      | minutes == 0 = "Z"
      | otherwise =
          let (h, m) = abs minutes `divMod` 60
          in T.concat [if minutes < 0 then "-" else "+", pad 2 (toInteger h), ":", pad 2 (toInteger m)]

    minutes :: Int
    minutes = timeZoneMinutes tz

formatUTCTime :: UTCTime -> T.Text
formatUTCTime = formatZonedTime . utcToZonedTime utc

-- | A number with zeros in front, up to the given number of digits.
pad :: Int -> Integer -> T.Text
pad n x = let s = T.pack (show x) in T.replicate (n - T.length s) "0" <> s
