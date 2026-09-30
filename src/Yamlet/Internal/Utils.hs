{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Helpers for the other modules.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Utils
  ( textStripPrefix
  , textIsPrefixOf
  , maxImplicitKeyLength
  , maxVersion
  , coreTagPrefix
  , picoDecimals
  , decimalPlaces
  , isHighSurrogate
  , isLowSurrogate
  , fromSurrogates
  , isScalarValue
  , xEscapeDigits
  , uEscapeDigits
  , bigUEscapeDigits
  , percentDigits
  ) where

import Data.Char
import Data.Fixed
import Data.Proxy
import Data.Text qualified as T
import Math.NumberTheory.Logarithms

#if !MIN_VERSION_text(2,1,4)
import Data.Text.Internal qualified as T
#endif

-- | 'Data.Text.stripPrefix'. Before text 2.1.4, 'Data.Text.stripPrefix'
-- compares the texts as streams of characters and allocates for each
-- character. For these versions this is the code of text 2.1.4, which
-- compares the UTF-8 bytes.
textStripPrefix :: T.Text -> T.Text -> Maybe T.Text
#if MIN_VERSION_text(2,1,4)
textStripPrefix = T.stripPrefix
#else
textStripPrefix p@(T.Text _arr _off plen) t@(T.Text arr off len)
  | textIsPrefixOf p t = Just $! T.text arr (off + plen) (len - plen)
  | otherwise = Nothing
#endif

-- | 'Data.Text.isPrefixOf', with the code of text 2.1.4 as in
-- 'textStripPrefix'.
textIsPrefixOf :: T.Text -> T.Text -> Bool
#if MIN_VERSION_text(2,1,4)
textIsPrefixOf = T.isPrefixOf
#else
textIsPrefixOf a@(T.Text _aArr _aOff aLen) b@(T.Text bArr bOff bLen) =
  d >= 0 && a == b'
  where
    d :: Int
    d = bLen - aLen

    b' :: T.Text
    b'
      | d == 0 = b
      | otherwise = T.Text bArr bOff aLen
#endif

-- | The largest number of characters of an implicit key, from the YAML 1.2.2
-- specification.
maxImplicitKeyLength :: Int
maxImplicitKeyLength = 1024

-- | The largest number in a version of a @%YAML@ directive.
--
-- Without a limit, the largest number depends on the size of Int, which
-- differs between architectures. The limit is far above any version of YAML,
-- and a number below it times 10 fits in 32 bits.
maxVersion :: Int
maxVersion = 1000000

-- | The prefix of the tags of the core schema, and of the @!!@ handle.
coreTagPrefix :: T.Text
coreTagPrefix = "tag:yaml.org,2002:"

-- | The number of decimal places of 'Pico', the resolution of the durations of
-- the time library.
picoDecimals :: Int
picoDecimals = integerLog10 (resolution (Proxy @E12))

-- | The number of decimal places of 1/n, or 'Nothing' if 1/n has no finite
-- decimal form. It has one if n is 2^a * 5^b, and then it needs max a b
-- places.
decimalPlaces :: Integer -> Maybe Int
decimalPlaces n = if rest == 1 then Just (max twos fives) else Nothing
  where
    twos, fives :: Int
    afterTwos, rest :: Integer
    (twos, afterTwos) = factors 2 n
    (fives, rest) = factors 5 afterTwos

    -- The number of factors p of x, and x without them.
    factors :: Integer -> Integer -> (Int, Integer)
    factors p = go 0
      where
        go :: Int -> Integer -> (Int, Integer)
        go i x = case x `quotRem` p of
          (q, 0) | x /= 0 -> go (i + 1) q
          _ -> (i, x)

-- | The first code unit of a surrogate pair of UTF-16.
isHighSurrogate :: Int -> Bool
isHighSurrogate u = u >= 0xD800 && u <= 0xDBFF

-- | The second code unit of a surrogate pair of UTF-16.
isLowSurrogate :: Int -> Bool
isLowSurrogate u = u >= 0xDC00 && u <= 0xDFFF

-- | The code point of a surrogate pair, by the formula of UTF-16.
fromSurrogates :: Int -> Int -> Int
fromSurrogates hi lo = 0x10000 + (hi - 0xD800) * 0x400 + (lo - 0xDC00)

-- | A code point that a character can have: in the range of Unicode, and not a
-- surrogate.
isScalarValue :: Int -> Bool
isScalarValue c = c >= 0 && c <= ord maxBound && not (isHighSurrogate c || isLowSurrogate c)

-- | The number of hex digits of the @\\x@, @\\u@ and @\\U@ escapes of a
-- double-quoted scalar.
xEscapeDigits, uEscapeDigits, bigUEscapeDigits :: Int
xEscapeDigits = 2
uEscapeDigits = 4
bigUEscapeDigits = 8

-- | The number of hex digits of a @%XX@ escape in a tag.
percentDigits :: Int
percentDigits = 2
