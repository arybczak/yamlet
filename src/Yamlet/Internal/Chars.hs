{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | The bytes of UTF-8 encoded YAML and the classes of characters that the
-- parser, the renderer and the error messages share.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Chars
  ( -- * Characters
    pattern TAB
  , pattern LF
  , pattern CR
  , pattern SPACE
  , pattern EXCL
  , pattern DQUOTE
  , pattern HASH
  , pattern PERCENT
  , pattern AMP
  , pattern SQUOTE
  , pattern STAR
  , pattern PLUS
  , pattern COMMA
  , pattern MINUS
  , pattern DOT
  , pattern DIGIT_0
  , pattern DIGIT_1
  , pattern DIGIT_9
  , pattern COLON
  , pattern LESS
  , pattern GREATER
  , pattern QUESTION
  , pattern AT
  , pattern UPPER_A
  , pattern UPPER_F
  , pattern UPPER_Z
  , pattern LBRACKET
  , pattern BACKSLASH
  , pattern RBRACKET
  , pattern GRAVE
  , pattern LOWER_A
  , pattern LOWER_F
  , pattern LOWER_U
  , pattern LOWER_Z
  , pattern LBRACE
  , pattern PIPE
  , pattern RBRACE
  , pattern DEL
  , isWhite
  , isBreak
  , isAsciiByte
  , asciiChar
  , isCharStart
  , isNsChar
  , isFlowIndicator
  , isIndicator
  , isDecDigit
  , isHexDigit'
  , hexValue
  , isWordChar
  , isUriChar
  , isTagChar
  , isAnchorChar
  , bomLength
  , isBomIn
  ) where

import Data.Bits
import Data.Char
import Data.Text.Array qualified as A
import Data.Word

pattern
  TAB
  , LF
  , CR
  , SPACE
  , EXCL
  , DQUOTE
  , HASH
  , PERCENT
  , AMP
  , SQUOTE
  , STAR
  , PLUS
  , COMMA
  , MINUS
  , DOT
  , DIGIT_0
  , DIGIT_1
  , DIGIT_9
  , COLON
  , LESS
  , GREATER
  , QUESTION
  , AT
  , UPPER_A
  , UPPER_F
  , UPPER_Z
  , LBRACKET
  , BACKSLASH
  , RBRACKET
  , GRAVE
  , LOWER_A
  , LOWER_F
  , LOWER_U
  , LOWER_Z
  , LBRACE
  , PIPE
  , RBRACE
  , DEL
    :: Word8
pattern TAB = 0x09
pattern LF = 0x0A
pattern CR = 0x0D
pattern SPACE = 0x20
pattern EXCL = 0x21
pattern DQUOTE = 0x22
pattern HASH = 0x23
pattern PERCENT = 0x25
pattern AMP = 0x26
pattern SQUOTE = 0x27
pattern STAR = 0x2A
pattern PLUS = 0x2B
pattern COMMA = 0x2C
pattern MINUS = 0x2D
pattern DOT = 0x2E
pattern DIGIT_0 = 0x30
pattern DIGIT_1 = 0x31
pattern DIGIT_9 = 0x39
pattern COLON = 0x3A
pattern LESS = 0x3C
pattern GREATER = 0x3E
pattern QUESTION = 0x3F
pattern AT = 0x40
pattern UPPER_A = 0x41
pattern UPPER_F = 0x46
pattern UPPER_Z = 0x5A
pattern LBRACKET = 0x5B
pattern BACKSLASH = 0x5C
pattern RBRACKET = 0x5D
pattern GRAVE = 0x60
pattern LOWER_A = 0x61
pattern LOWER_F = 0x66
pattern LOWER_U = 0x75
pattern LOWER_Z = 0x7A
pattern LBRACE = 0x7B
pattern PIPE = 0x7C
pattern RBRACE = 0x7D
pattern DEL = 0x7F

isWhite :: Word8 -> Bool
isWhite w = w == SPACE || w == TAB

isBreak :: Word8 -> Bool
isBreak w = w == LF || w == CR

isAsciiByte :: Word8 -> Bool
isAsciiByte w = w < 0x80

-- | A predicate on bytes for a character, e.g. 'isFlowIndicator' for the
-- emitter. A character beyond ASCII does not satisfy it.
asciiChar :: (Word8 -> Bool) -> Char -> Bool
asciiChar p c = isAscii c && p (fromIntegral (ord c))

-- | The byte starts a character in UTF-8, i.e. it is not a continuation byte.
isCharStart :: Word8 -> Bool
isCharStart w = isAsciiByte w || w >= 0xC0

-- | ns-char. Every byte of a multibyte character counts, because the input
-- contains printable characters only, except in quoted scalars, which the
-- parser checks after it parses the stream.
isNsChar :: Word8 -> Bool
isNsChar w = w > SPACE && w /= DEL

isFlowIndicator :: Word8 -> Bool
isFlowIndicator w =
  w == COMMA
    || w == LBRACKET
    || w == RBRACKET
    || w == LBRACE
    || w == RBRACE

isIndicator :: Word8 -> Bool
isIndicator w = isAsciiByte w && testBit indicators (fromIntegral w)
  where
    indicators :: Integer
    indicators = foldr @[] (\c acc -> setBit acc (ord c)) 0 "-?:,[]{}#&*!|>'\"%@`"

isDecDigit :: Word8 -> Bool
isDecDigit w = w >= DIGIT_0 && w <= DIGIT_9

isHexDigit' :: Word8 -> Bool
isHexDigit' w = isDecDigit w || (w >= UPPER_A && w <= UPPER_F) || (w >= LOWER_A && w <= LOWER_F)

hexValue :: Word8 -> Int
hexValue w
  | w <= DIGIT_9 = fromIntegral (w - DIGIT_0)
  | w <= UPPER_F = fromIntegral (w - UPPER_A) + 10
  | otherwise = fromIntegral (w - LOWER_A) + 10

isWordChar :: Word8 -> Bool
isWordChar w =
  isDecDigit w
    || (w >= UPPER_A && w <= UPPER_Z)
    || (w >= LOWER_A && w <= LOWER_Z)
    || w == MINUS

-- | ns-uri-char without the escaped characters.
isUriChar :: Word8 -> Bool
isUriChar w = isWordChar w || w `elem` extra
  where
    extra :: [Word8]
    extra = map (fromIntegral . ord) "#;/?:@&=+$,_.!~*'()[]"

-- | ns-tag-char without the escaped characters.
isTagChar :: Word8 -> Bool
isTagChar w = isUriChar w && w /= EXCL && not (isFlowIndicator w)

isAnchorChar :: Word8 -> Bool
isAnchorChar w = isNsChar w && not (isFlowIndicator w)

-- | The number of bytes of a byte order mark, U+FEFF in UTF-8.
bomLength :: Int
bomLength = 3

-- | A byte order mark at the index of the array, before the end index.
isBomIn :: A.Array -> Int -> Int -> Bool
isBomIn arr end i =
  i + bomLength <= end
    && A.unsafeIndex arr i == 0xEF
    && A.unsafeIndex arr (i + 1) == 0xBB
    && A.unsafeIndex arr (i + 2) == 0xBF
