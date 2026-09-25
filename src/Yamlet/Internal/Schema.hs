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
  , isYaml11Bool
  ) where

import Data.Bifunctor
import Data.Char
import Data.Scientific qualified as Sci
import Data.Text qualified as T

import Yamlet.Internal.Emit
import Yamlet.Internal.Utils
import Yamlet.Node

-- | The value of a plain scalar without a tag, e.g. @null@, @true@, @12@,
-- @0x1F@ and @1.5e3@ are not strings. Quoted and block scalars are always
-- strings.
--
-- A number with an exponent beyond the range from -1000 to 1000 in
-- scientific notation, e.g. @1e1001@ or @0.1e-1000@, becomes infinity or
-- zero, as a double does. The decoders reject such a number, because its
-- value is not exact.
resolvePlain :: T.Text -> Value
resolvePlain = either id id . resolvePlainExact

-- | The value of a scalar with the given resolved tag, e.g.
-- @tag:yaml.org,2002:int@. Return 'Nothing' if the text is not valid for a tag of
-- the core schema. A scalar with another tag is a string.
--
-- A number with an exponent beyond the range from -1000 to 1000 becomes
-- infinity or zero, as in 'resolvePlain'.
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
isPlainString :: T.Text -> Bool
isPlainString t = case resolvePlain t of
  String _ -> True
  _ -> False

-- | The string reads back as the same string if it is a plain scalar in the
-- block style, as a value or as a key. In a flow collection the characters
-- @,[]{}@ need quotes too, so the check does not apply there.
isPlainSafe :: T.Text -> Bool
isPlainSafe t = plainSyntax False t && isPlainString t

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
-- yes or off.
isYaml11Bool :: T.Text -> Bool
isYaml11Bool t =
  t `elem` ["y", "Y", "yes", "Yes", "YES", "n", "N", "no", "No", "NO", "on", "On", "ON", "off", "Off", "OFF"]

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
      | n <= 40 = T.foldl' (\acc d -> acc * radix + toInteger (digitToInt d)) 0 t
      | otherwise =
          let k = n `div` 2
              (hi, lo) = T.splitAt (n - k) t
          in go (n - k) hi * radix ^ k + go k lo

-- | The decimal digits times a power of 10. A value beyond the limit of
-- 'maxExponent' gives infinity or zero, which are not exact.
--
-- The coefficient has no trailing zeros. The comparison of two
-- 'Sci.Scientific' values removes them one digit at a time, which takes
-- quadratic time in their number.
decimal :: T.Text -> Integer -> Either FloatValue FloatValue
decimal ds0 e0
  | c == 0 = Right (Finite 0)
  | leading > maxExponent = Left Infinity
  | leading < negate maxExponent = Left (Finite 0)
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

-- | The limit of the exponent of the first digit of a float. A
-- 'Sci.Scientific' keeps the exponent apart from the coefficient, but its
-- conversion to an 'Integer', e.g. with 'truncate', computes every digit.
-- With this limit, the integer has at most 1001 digits, about 420 bytes.
-- Without a limit, a short input such as @1e999999999@ gives an integer of
-- about 400 MiB. The limit covers the whole range of 'Double', from about
-- 5e-324 to 1.8e308.
maxExponent :: Integer
maxExponent = 1000

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
