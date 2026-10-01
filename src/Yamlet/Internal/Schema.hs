{-# OPTIONS_HADDOCK not-home #-}

-- | The core schema of YAML 1.2.2.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Schema
  ( resolvePlain
  , resolveTagged
  , resolvePlainExact
  , resolveTaggedExact
  , isPlainString
  , isPlainSafe
  , isPlainPortable
  , isYaml11Bool
  , isYaml11NonString
  , isYaml11Timestamp
  , maxExponent
  , exponentOutOfRange
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Maybe
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Time.Calendar

import Yamlet.Internal.Emit
import Yamlet.Internal.Utils
import Yamlet.Value

-- | The value of a plain scalar without a tag. Not every such scalar is a
-- string, e.g. @null@, @true@, @12@, @0x1F@ and @1.5e3@ are not. Quoted and
-- block scalars are always strings.
--
-- A float whose exponent in scientific notation is beyond the range from
-- -1000 to 1000, e.g. @1e1001@, @10e1000@ or @1e-1001@, becomes infinity
-- or zero, as a double does. The decoders reject such a number, because its
-- value is not exact.
--
-- >>> map resolvePlain ["", "true", "0x1F", "1.5e3", ".inf", "yes", "9.10.3"]
-- [Null,Bool True,Int 31,Float (Finite 1500.0),Float Infinity,String "yes",String "9.10.3"]
resolvePlain :: T.Text -> Value
resolvePlain = either id id . resolvePlainExact

-- | The value of a scalar with the given resolved tag, e.g.
-- @tag:yaml.org,2002:int@. Return 'Nothing' if the text is not valid for a
-- tag of the core schema. A scalar with another tag is a string.
--
-- A float beyond the limit becomes infinity or zero, as in 'resolvePlain'.
--
-- >>> [resolveTagged floatTag "1", resolveTagged intTag "abc", resolveTagged "!point" "1"]
-- [Just (Float (Finite 1.0)),Nothing,Just (String "1")]
resolveTagged :: T.Text -> T.Text -> Maybe Value
resolveTagged tag t = either id id <$> resolveTaggedExact tag t

-- | The value of a plain scalar, 'Left' if the value is not exact.
resolvePlainExact :: T.Text -> Either Value Value
resolvePlainExact t = case T.uncons t of
  Nothing -> Right Null
  Just (c, _)
    | c == '~' || c == 'n' || c == 'N' -> Right $ if isNull t then Null else String t
    | c == 't' || c == 'T' || c == 'f' || c == 'F' -> Right $ maybe (String t) Bool (readBool t)
    | isDigit c || c == '-' || c == '+' || c == '.' -> case readInt t of
        Just i -> Right (Int i)
        Nothing -> maybe (Right (String t)) (bimap Float Float) (readFloat t)
    | otherwise -> Right (String t)

-- | The value of a scalar with a tag, 'Left' if the value is not exact.
resolveTaggedExact :: T.Text -> T.Text -> Maybe (Either Value Value)
resolveTaggedExact tag t
  | tag == strTag = Just . Right $ String t
  | tag == nullTag = if isNull t then Just (Right Null) else Nothing
  | tag == boolTag = Right . Bool <$> readBool t
  | tag == intTag = Right . Int <$> readInt t
  | tag == floatTag = bimap Float Float <$> readFloat t
  | otherwise = Just . Right $ String t

-- | A plain scalar with the text is a string, e.g. @9.10.3@ is a string, but
-- @9.10@ and @true@ are not. The check ignores the syntax, so e.g. @a: b@
-- passes. For both checks, use 'isPlainSafe'.
--
-- >>> map isPlainString ["9.10.3", "9.10", "true", "a: b"]
-- [True,False,False,True]
isPlainString :: T.Text -> Bool
isPlainString t = case resolvePlain t of
  String _ -> True
  _ -> False

-- | The string reads back as the same string if it is a plain scalar in the
-- block style, as a value or as a key. In a flow collection the characters
-- @,[]{}@ need quotes too, so the check does not apply there.
--
-- >>> map isPlainSafe ["a:b", "a: b", "- a", "a #b", "9.10"]
-- [True,False,False,False,False]
isPlainSafe :: T.Text -> Bool
isPlainSafe t = plainSyntax False t && isPlainString t

-- | As 'isPlainSafe', and common YAML 1.1 parsers also read the plain scalar
-- as a string, e.g. not @yes@ as a boolean or @12:30@ as a number. The
-- encoder writes a string without quotes only if it passes this check.
--
-- >>> map isPlainPortable ["a:b", "yes", "12:30", "2024-01-01", "9.10.3"]
-- [True,False,False,False,True]
isPlainPortable :: T.Text -> Bool
isPlainPortable t = isPlainSafe t && not (isYaml11NonString t)

isNull :: T.Text -> Bool
isNull t = T.null t || t == "~" || t == "null" || t == "Null" || t == "NULL"

readBool :: T.Text -> Maybe Bool
readBool = \case
  "true" -> Just True
  "True" -> Just True
  "TRUE" -> Just True
  "false" -> Just False
  "False" -> Just False
  "FALSE" -> Just False
  _ -> Nothing

-- | A word that YAML 1.1 reads as a boolean, but YAML 1.2 as a string, e.g.
-- @yes@ or @off@.
isYaml11Bool :: T.Text -> Bool
isYaml11Bool t =
  t `elem` ["y", "Y", "yes", "Yes", "YES", "n", "N", "no", "No", "NO", "on", "On", "ON", "off", "Off", "OFF"]

-- | A common YAML 1.1 parser reads a plain scalar with the text as a value
-- that is not a string, e.g. the boolean @yes@, the base-60 number @12:30@ or
-- the date @2024-01-01@. The patterns cover what PyYAML, Ruby's Psych and
-- go-yaml v2, which Kubernetes uses, accept. The YAML 1.1 types themselves
-- are not enough: the parsers accept more, e.g. @1,000@ in Psych and @0X1F@ in
-- go-yaml v2, and the type of floats accepts too much, e.g. @1.2.3@.
isYaml11NonString :: T.Text -> Bool
isYaml11NonString t = case T.uncons t of
  Nothing -> True
  Just (c, _)
    -- A symbol in Psych, which its safe loader rejects.
    | c == ':' -> T.compareLength t 1 == GT
    | isDigit c || c == '-' || c == '+' || c == '.' ->
        matches (alt [int, float, timestamp]) t || matches goNumber (T.filter (/= '_') t)
    | otherwise ->
        t `elem` ["y", "Y", "n", "N", "~", "<<", "="]
          -- Psych ignores the case of these words.
          || (T.compareLength t 5 /= GT && T.toLower t `elem` ["yes", "no", "true", "false", "on", "off", "null"])
  where
    matches :: (T.Text -> [T.Text]) -> T.Text -> Bool
    matches m s = any T.null (m s)

    int :: T.Text -> [T.Text]
    int =
      sign
        >=> alt
          [ str "0b" >=> some (separatorOr (`elem` ['0', '1']))
          , one (== '0') >=> some (separatorOr isOctDigit)
          , one (== '0')
          , nonZero >=> many (separatorOr isDigit)
          , str "0x" >=> some (separatorOr isHexDigit)
          , digit >=> many (underscoreOr isDigit) >=> sexagesimal
          ]

    float :: T.Text -> [T.Text]
    float =
      sign
        >=> alt
          [ digit >=> many (separatorOr isDigit) >=> one (== '.') >=> many (underscoreOr isDigit) >=> opt exponentPart
          , one (== '.') >=> some (underscoreOr isDigit) >=> opt exponentPart
          , one (== '.') >=> exponentPart
          , digit >=> many (underscoreOr isDigit) >=> sexagesimal >=> one (== '.') >=> many (underscoreOr isDigit)
          , one (== '.') >=> caseless "inf"
          , one (== '.') >=> caseless "nan"
          ]

    timestamp :: T.Text -> [T.Text]
    timestamp =
      alt
        [ digits 4 >=> one (== '-') >=> oneOrTwoDigits >=> one (== '-') >=> oneOrTwoDigits
        , opt (one (== '-'))
            >=> digits 4
            >=> one (== '-')
            >=> oneOrTwoDigits
            >=> one (== '-')
            >=> oneOrTwoDigits
            >=> alt [one (`elem` ['T', 't']), some blank]
            >=> oneOrTwoDigits
            >=> one (== ':')
            >=> digits 2
            >=> one (== ':')
            >=> digits 2
            >=> opt (one (== '.') >=> many digit)
            >=> opt (many blank >=> alt [one (== 'Z'), one (`elem` ['+', '-']) >=> oneOrTwoDigits >=> opt (opt (one (== ':')) >=> digits 2)])
        ]

    -- The numbers of go-yaml v2, which removes the underscores first: the
    -- integers of Go, a float whose dot and sign of the exponent are
    -- optional, and a binary integer with its sign after "0b", e.g. 0b-1.
    goNumber :: T.Text -> [T.Text]
    goNumber =
      alt
        [ sign
            >=> alt
              [ one (== '0') >=> one (`elem` ['x', 'X']) >=> some (one isHexDigit)
              , one (== '0') >=> one (`elem` ['o', 'O']) >=> some (one isOctDigit)
              , one (== '0') >=> one (`elem` ['b', 'B']) >=> some (one (`elem` ['0', '1']))
              , alt [one (== '.') >=> some digit, some digit >=> opt (one (== '.') >=> many digit)]
                  >=> opt (one (`elem` ['e', 'E']) >=> opt (one (`elem` ['+', '-'])) >=> some digit)
              ]
        , str "0b" >=> one (`elem` ['+', '-']) >=> some (one (`elem` ['0', '1']))
        ]

    -- Each matcher gives the rests of the text after all its possible
    -- matches, so the patterns backtrack as the regular expressions of the
    -- parsers do and each one matches its regular expression. A parser such as
    -- attoparsec does not backtrack into an optional or repeated part, e.g.
    -- [0-5]?[0-9] would take the 5 of 1:5 and then find no digit.
    one :: (Char -> Bool) -> T.Text -> [T.Text]
    one p s = case T.uncons s of
      Just (x, rest) | p x -> [rest]
      _ -> []

    str :: T.Text -> T.Text -> [T.Text]
    str prefix s = maybe [] pure (textStripPrefix prefix s)

    alt :: [T.Text -> [T.Text]] -> T.Text -> [T.Text]
    alt ms s = concatMap ($ s) ms

    opt :: (T.Text -> [T.Text]) -> T.Text -> [T.Text]
    opt m s = s : m s

    many :: (T.Text -> [T.Text]) -> T.Text -> [T.Text]
    many m s = s : (m s >>= many m)

    some :: (T.Text -> [T.Text]) -> T.Text -> [T.Text]
    some m = m >=> many m

    sign :: T.Text -> [T.Text]
    sign = opt (one (`elem` ['+', '-']))

    digit :: T.Text -> [T.Text]
    digit = one isDigit

    digits :: Int -> T.Text -> [T.Text]
    digits k = foldr (>=>) pure (replicate k digit)

    oneOrTwoDigits :: T.Text -> [T.Text]
    oneOrTwoDigits = digit >=> opt digit

    nonZero :: T.Text -> [T.Text]
    nonZero = one (\x -> isDigit x && x /= '0')

    underscoreOr :: (Char -> Bool) -> T.Text -> [T.Text]
    underscoreOr p = one (\x -> x == '_' || p x)

    -- Psych also allows commas in numbers, e.g. 1,000.
    separatorOr :: (Char -> Bool) -> T.Text -> [T.Text]
    separatorOr p = one (\x -> x == '_' || x == ',' || p x)

    caseless :: T.Text -> T.Text -> [T.Text]
    caseless w s = [rest | let (prefix, rest) = T.splitAt (T.length w) s, T.toLower prefix == w]

    sexagesimal :: T.Text -> [T.Text]
    sexagesimal = some (one (== ':') >=> opt (one (`elem` ['0' .. '5'])) >=> digit)

    exponentPart :: T.Text -> [T.Text]
    exponentPart = one (`elem` ['e', 'E']) >=> one (`elem` ['+', '-']) >=> some digit

    blank :: T.Text -> [T.Text]
    blank = one (`elem` [' ', '\t'])

-- | A common YAML 1.1 parser reads a plain scalar with the text as a
-- timestamp and can build it. PyYAML has the years from 1 to 9999 of Python,
-- and it rejects the hour 24, a leap second and a time zone of 24 hours.
-- Psych reads the hour 24 and a leap second as a later time.
--
-- >>> map isYaml11Timestamp ["2024-01-01", "2024-01-01T12:30:00Z", "0000-01-01", "2016-12-31T23:59:60Z", "12:30"]
-- [True,True,False,False,False]
isYaml11Timestamp :: T.Text -> Bool
isYaml11Timestamp t =
  isYaml11NonString t && case T.splitOn "-" date of
    [y, m, d]
      | T.length y == 4
      , all (\ds -> not (T.null ds) && T.all isDigit ds) [y, m, d] ->
          let year = digitsValue 10 y
          in year >= 1
               && year <= 9999
               && isJust (fromGregorianValid year (number m) (number d))
               && validTime (T.dropWhile isTimeSeparator rest)
    _ -> False
  where
    (date, rest) = T.break isTimeSeparator t

    isTimeSeparator :: Char -> Bool
    isTimeSeparator c = c == 'T' || c == 't' || c == ' ' || c == '\t'

    number :: T.Text -> Int
    number = fromInteger . digitsValue 10

    -- The time, with the hours, the minutes and the seconds of the pattern of
    -- 'isYaml11NonString'.
    validTime :: T.Text -> Bool
    validTime s
      | T.null s = True
      | otherwise = case T.splitOn ":" (T.takeWhile (\c -> isDigit c || c == ':') s) of
          h : _ : sec : _ ->
            number h < 24 && number (T.take 2 sec) < 60 && validZone (T.dropWhile (\c -> isDigit c || c `elem` [':', '.', ' ', '\t']) s)
          _ -> False

    -- The hours of a zone are the digits before the last two, unless the
    -- zone has a colon or at most two digits.
    validZone :: T.Text -> Bool
    validZone z = case T.uncons z of
      Just (c, offset)
        | c == '+' || c == '-' ->
            let hours = T.takeWhile isDigit offset
                h = if T.compareLength hours 2 == GT then T.dropEnd 2 hours else hours
            in number h < 24
      _ -> True

-- | [-+]?[0-9]+, 0o[0-7]+ or 0x[0-9a-fA-F]+.
readInt :: T.Text -> Maybe Integer
readInt t
  | Just ds <- textStripPrefix "0o" t = digits 8 isOctDigit ds
  | Just ds <- textStripPrefix "0x" t = digits 16 isHexDigit ds
  | Just ds <- textStripPrefix "-" t = negate <$> digits 10 isDigit ds
  | Just ds <- textStripPrefix "+" t = digits 10 isDigit ds
  | otherwise = digits 10 isDigit t
  where
    digits :: Integer -> (Char -> Bool) -> T.Text -> Maybe Integer
    digits radix valid ds
      | not (T.null ds) && T.all valid ds = Just $ digitsValue radix ds
      | otherwise = Nothing

-- | The value of the digits in the radix. A multiplication for each digit
-- takes quadratic time in the number of digits, so the halves of a long text
-- are read apart.
digitsValue :: Integer -> T.Text -> Integer
digitsValue radix t0 = go (T.length t0) t0
  where
    go :: Int -> T.Text -> Integer
    go n t
      | n <= maxFoldDigits = T.foldl' (\acc d -> acc * radix + toInteger (digitToInt d)) 0 t
      | otherwise =
          let k = n `div` 2
              (hi, lo) = T.splitAt (n - k) t
          in go (n - k) hi * radix ^ k + go k lo

    -- Up to about 20 digits, one fold is faster than a split, measured with
    -- GHC 9.10.3 for numbers from 60 to 100000 digits.
    maxFoldDigits :: Int
    maxFoldDigits = 20

-- | The decimal digits times a power of 10, with the exponent that the text
-- of the float has. A value beyond the limit of 'maxExponent' gives infinity
-- or zero, which are not exact.
--
-- The coefficient has no trailing zeros. The comparison of two
-- t'Data.Scientific.Scientific' values removes them one digit at a time, which
-- takes quadratic time in their number.
decimal :: T.Text -> Integer -> Either FloatValue FloatValue
decimal ds0 e0
  | c == 0 = Right (Finite 0)
  | abs leading > maxExponent = Left (if leading > 0 then Infinity else Finite 0)
  | otherwise = Right $ Finite (Sci.scientific c (fromInteger e))
  where
    ds :: T.Text
    ds = T.dropWhileEnd (== '0') ds0

    e :: Integer
    e = e0 + toInteger (T.length ds0 - T.length ds)

    -- The exponent of the first digit that is not zero.
    leading :: Integer
    leading = e + toInteger (T.length (T.dropWhile (== '0') ds)) - 1

    c :: Integer
    c = digitsValue 10 ds

-- | The limit of the exponent of a float in scientific notation, i.e. the
-- exponent of its first digit that is not zero. The limit applies to the
-- value, not to the text, so every value that the decoder gives reads back
-- after the encoder writes it.
--
-- A t'Data.Scientific.Scientific' keeps the exponent apart from the
-- coefficient, but its conversion to an 'Integer', e.g. with 'truncate',
-- computes every digit. With this limit, the integer has at most 1001
-- digits. Without a limit, a short input such as @1e999999999@ gives an
-- integer of about 400 MiB. The limit covers the whole range of 'Double',
-- from about 5e-324 to 1.8e308.
maxExponent :: Integer
maxExponent = 1000

-- | The error for a number beyond 'maxExponent'.
exponentOutOfRange :: String
exponentOutOfRange =
  "the exponent of the number is out of the range from "
    ++ show (negate maxExponent)
    ++ " to "
    ++ show maxExponent

negateFloat :: FloatValue -> FloatValue
negateFloat = \case
  Finite s
    | s == 0 -> NegativeZero
    | otherwise -> Finite (negate s)
  NegativeZero -> Finite 0
  Infinity -> NegativeInfinity
  NegativeInfinity -> Infinity
  NaN -> NaN

-- | [-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?, [-+]?\.inf or \.nan
-- in one of three capitalizations. The value is 'Left' if it is not exact.
readFloat :: T.Text -> Maybe (Either FloatValue FloatValue)
readFloat t0 = case t0 of
  ".nan" -> Just (Right NaN)
  ".NaN" -> Just (Right NaN)
  ".NAN" -> Just (Right NaN)
  _ -> case T.uncons t0 of
    Just ('-', t) -> bimap negateFloat negateFloat <$> unsigned t
    Just ('+', t) -> unsigned t
    _ -> unsigned t0
  where
    unsigned :: T.Text -> Maybe (Either FloatValue FloatValue)
    unsigned t
      | t == ".inf" || t == ".Inf" || t == ".INF" = Just (Right Infinity)
      | otherwise =
          let (int, rest) = T.span isDigit t
              (frac, rest') = case T.uncons rest of
                Just ('.', r) -> T.span isDigit r
                _ -> ("", rest)
              hasDot = textIsPrefixOf "." rest
          in if
               | T.null int && T.null frac -> Nothing
               | not (T.null int) || hasDot -> do
                   ex <- exponent_ rest'
                   Just $ decimal (int <> frac) (ex - toInteger (T.length frac))
               | otherwise -> Nothing

    exponent_ :: T.Text -> Maybe Integer
    exponent_ t = case T.uncons t of
      Nothing -> Just 0
      Just (e, r)
        | e == 'e' || e == 'E' ->
            let (sign, ds) = case T.uncons r of
                  Just ('-', d) -> (negate, d)
                  Just ('+', d) -> (id, d)
                  _ -> (id, r)
            in if not (T.null ds) && T.all isDigit ds
                 then Just . sign $ digitsValue 10 ds
                 else Nothing
        | otherwise -> Nothing
