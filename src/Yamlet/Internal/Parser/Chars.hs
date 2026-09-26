{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | The bytes of the input that the parser and its error messages look at.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Parser.Chars
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
  , isBom
  , skipBoms

    -- * Scanning
  , skipSpaces
  , skipWhites
  , breakEnd
  , isStartOfLine
  , isMarker
  , fitsKey
  ) where

import Data.Bits
import Data.Char
import Data.Text.Array qualified as A
import Data.Word

import Yamlet.Internal.Parser.Monad
import Yamlet.Internal.Utils

----------------------------------------
-- Characters

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

-- | The byte starts a character in UTF-8, i.e. it is not a continuation byte.
isCharStart :: Word8 -> Bool
isCharStart w = isAsciiByte w || w >= 0xC0

-- | ns-char. Every byte of a multibyte character counts, because the input
-- contains printable characters only.
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
    indicators = foldr (\c acc -> setBit acc (ord c)) 0 ("-?:,[]{}#&*!|>'\"%@`" :: String)

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

isBom :: Env -> Int -> Bool
isBom e = isBomIn e.array e.end

skipBoms :: Env -> Int -> Int
skipBoms e i = if isBom e i then skipBoms e (i + bomLength) else i

----------------------------------------
-- Scanning

skipSpaces :: Env -> Int -> Int
skipSpaces e i = if byteAt e i == SPACE then skipSpaces e (i + 1) else i

skipWhites :: Env -> Int -> Int
skipWhites e i = if isWhite (byteAt e i) then skipWhites e (i + 1) else i

-- | The index after the line break at the index.
breakEnd :: Env -> Int -> Int
breakEnd e i
  | byteAt e i == CR && byteAt e (i + 1) == LF = i + 2
  | otherwise = i + 1

isStartOfLine :: Env -> Int -> Bool
isStartOfLine e i
  | i <= e.base = True
  | isBreak (byteBefore e i) = True
  | i - bomLength >= e.base && isBom e (i - bomLength) = isStartOfLine e (i - bomLength)
  | otherwise = False

-- | A @---@ or @...@ marker at the start of a line.
isMarker :: Env -> Int -> Bool
isMarker e i =
  let w = byteAt e i
  in (w == MINUS || w == DOT)
       && byteAt e (i + 1) == w
       && byteAt e (i + 2) == w
       && (let w3 = byteAt e (i + 3) in w3 == 0 || isWhite w3 || isBreak w3)
       && isStartOfLine e i

-- | The input between the indices fits in an implicit key.
fitsKey :: Env -> Int -> Int -> Bool
fitsKey e p q =
  q - p <= maxImplicitKeyLength
    || (q - p <= maxImplicitKeyLength * maxCharBytes && countChars <= maxImplicitKeyLength)
  where
    -- The longest UTF-8 encoding of a character.
    maxCharBytes :: Int
    maxCharBytes = 4

    countChars :: Int
    countChars =
      length
        [() | x <- [p .. q - 1], isCharStart (byteAt e x)]
