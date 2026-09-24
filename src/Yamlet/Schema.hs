-- | The core schema of YAML 1.2.2: the rules that give a scalar its value.
--
-- A decoder applies these rules to every scalar. A program that writes YAML
-- can use them to check how a plain scalar reads back, e.g. @9.10@ is a
-- number, not a string.
module Yamlet.Schema
  ( resolvePlain
  , resolveTagged
  , isPlainString
  , isPlainSafe
  ) where

import Control.Applicative
import Data.Char
import Data.Scientific qualified as Sci
import Data.Text qualified as T

import Yamlet.Internal.Emit
import Yamlet.Node

-- | The value of a plain scalar without a tag, e.g. @null@, @true@, @12@,
-- @0x1F@ and @1.5e3@ are not strings. Quoted and block scalars are always
-- strings.
resolvePlain :: T.Text -> Value
resolvePlain t = case T.uncons t of
  Nothing -> Null
  Just (c, _)
    | c == '~' || c == 'n' || c == 'N' -> if isNull t then Null else String t
    | c == 't' || c == 'T' || c == 'f' || c == 'F' -> maybe (String t) Bool (readBool t)
    | isDigit c || c == '-' || c == '+' || c == '.' ->
        maybe (String t) id $ (Int <$> readInt t) <|> (Float <$> readFloat t)
    | otherwise -> String t

-- | The value of a scalar with the given resolved tag, e.g.
-- @tag:yaml.org,2002:int@. Return 'Nothing' if the text is not valid for a tag of
-- the core schema. A scalar with another tag is a string.
resolveTagged :: T.Text -> T.Text -> Maybe Value
resolveTagged tag t
  | tag == strTag = Just (String t)
  | tag == nullTag = if isNull t then Just Null else Nothing
  | tag == boolTag = Bool <$> readBool t
  | tag == intTag = Int <$> readInt t
  | tag == floatTag = Float <$> (readFloat t <|> (\i -> Finite (Sci.scientific i 0)) <$> readInt t)
  | otherwise = Just (String t)

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
readFloat :: T.Text -> Maybe FloatValue
readFloat t0 = case t0 of
  ".nan" -> Just NaN
  ".NaN" -> Just NaN
  ".NAN" -> Just NaN
  _ -> case T.uncons t0 of
    Just ('-', t) -> negateFloat <$> unsigned t
    Just ('+', t) -> unsigned t
    _ -> unsigned t0
  where
    negateFloat :: FloatValue -> FloatValue
    negateFloat = \case
      Finite s -> Finite (negate s)
      Infinity -> NegativeInfinity
      NegativeInfinity -> Infinity
      NaN -> NaN

    unsigned :: T.Text -> Maybe FloatValue
    unsigned t
      | t == ".inf" || t == ".Inf" || t == ".INF" = Just Infinity
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

    -- The digits times a power of 10. An exponent out of the range of Int
    -- gives infinity or zero.
    decimal :: T.Text -> Integer -> FloatValue
    decimal ds e
      | c == 0 = Finite 0
      | e > toInteger (maxBound @Int) = Infinity
      | e < toInteger (minBound @Int) = Finite 0
      | otherwise = Finite (Sci.scientific c (fromInteger e))
      where
        c :: Integer
        c = T.foldl' (\acc d -> acc * 10 + toInteger (digitToInt d)) 0 ds

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
