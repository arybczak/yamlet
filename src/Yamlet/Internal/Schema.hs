{-# OPTIONS_HADDOCK not-home #-}
-- | The core schema of YAML 1.2.2.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Schema
  ( resolvePlain
  , resolveTagged
  ) where

import Control.Applicative
import Data.Char
import Data.Text qualified as T

import Yamlet.Node

-- | The value of a plain scalar without a tag.
resolvePlain :: T.Text -> Value
resolvePlain t = case T.uncons t of
  Nothing -> Null
  Just (c, _)
    | c == '~' || c == 'n' || c == 'N' -> if isNull t then Null else String t
    | c == 't' || c == 'T' || c == 'f' || c == 'F' -> maybe (String t) Bool (readBool t)
    | isDigit c || c == '-' || c == '+' || c == '.' ->
        maybe (String t) id $ (Int <$> readInt t) <|> (Float <$> readFloat t)
    | otherwise -> String t

-- | The value of a scalar with a tag of the core schema. Return 'Nothing' if
-- the text is not valid for the tag.
resolveTagged :: T.Text -> T.Text -> Maybe Value
resolveTagged tag t
  | tag == strTag = Just (String t)
  | tag == nullTag = if isNull t then Just Null else Nothing
  | tag == boolTag = Bool <$> readBool t
  | tag == intTag = Int <$> readInt t
  | tag == floatTag = Float <$> (readFloat t <|> fromInteger <$> readInt t)
  | otherwise = Just (String t)

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

-- | [-+]?[0-9]+, 0o[0-7]+ or 0x[0-9a-fA-F]+.
readInt :: T.Text -> Maybe Integer
readInt t
  | Just ds <- T.stripPrefix "0o" t = digits 8 isOctDigit ds
  | Just ds <- T.stripPrefix "0x" t = digits 16 isHexDigit ds
  | Just ds <- T.stripPrefix "-" t = negate <$> digits 10 isDigit ds
  | Just ds <- T.stripPrefix "+" t = digits 10 isDigit ds
  | otherwise = digits 10 isDigit t
  where
    digits :: Integer -> (Char -> Bool) -> T.Text -> Maybe Integer
    digits radix valid ds
      | not (T.null ds) && T.all valid ds =
          Just $ T.foldl' (\acc d -> acc * radix + toInteger (digitToInt d)) 0 ds
      | otherwise = Nothing

-- | [-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?, [-+]?\.inf or \.nan
-- in one of three capitalizations.
readFloat :: T.Text -> Maybe Double
readFloat t0 = case t0 of
  ".nan" -> Just nan
  ".NaN" -> Just nan
  ".NAN" -> Just nan
  _ -> case T.uncons t0 of
    Just ('-', t) -> negate <$> unsigned t
    Just ('+', t) -> unsigned t
    _ -> unsigned t0
  where
    nan :: Double
    nan = 0 / 0

    unsigned :: T.Text -> Maybe Double
    unsigned t
      | t == ".inf" || t == ".Inf" || t == ".INF" = Just (1 / 0)
      | otherwise =
          let (int, rest) = T.span isDigit t
              (frac, rest') = case T.uncons rest of
                Just ('.', r) -> T.span isDigit r
                _ -> ("", rest)
              hasDot = T.isPrefixOf "." rest
          in if | T.null int && T.null frac -> Nothing
                | not (T.null int) || hasDot -> do
                    ex <- exponent_ rest'
                    Just $ decimal (int <> frac) (ex - toInteger (T.length frac))
                | otherwise -> Nothing

    -- The value of digits times a power of 10. The multiplication or the
    -- division is exact and correctly rounded if both operands fit in the
    -- mantissa of a double.
    decimal :: T.Text -> Integer -> Double
    decimal ds e
      | T.length (T.dropWhile (== '0') ds) <= 15 && abs e <= 22 =
          let m = fromIntegral (T.foldl' (\acc d -> acc * 10 + digitToInt d) 0 ds)
          in if e >= 0 then m * 10 ^ e else m / 10 ^ negate e
      | otherwise = read $ (if T.null ds then "0" else T.unpack ds) ++ "e" ++ show e

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
                 then Just . sign $ T.foldl' (\acc d -> acc * 10 + toInteger (digitToInt d)) 0 ds
                 else Nothing
        | otherwise -> Nothing
