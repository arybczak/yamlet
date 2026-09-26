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
  , pattern COMMA
  , pattern MINUS
  , pattern DOT
  , pattern COLON
  , pattern LESS
  , pattern GREATER
  , pattern QUESTION
  , pattern AT
  , pattern LBRACKET
  , pattern BACKSLASH
  , pattern RBRACKET
  , pattern GRAVE
  , pattern LBRACE
  , pattern PIPE
  , pattern RBRACE
  , pattern STAR
  , isWhite
  , isBreak
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
  , COMMA
  , MINUS
  , DOT
  , COLON
  , LESS
  , GREATER
  , QUESTION
  , AT
  , LBRACKET
  , BACKSLASH
  , RBRACKET
  , GRAVE
  , LBRACE
  , PIPE
  , RBRACE
  , STAR
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
pattern COMMA = 0x2C
pattern MINUS = 0x2D
pattern DOT = 0x2E
pattern COLON = 0x3A
pattern LESS = 0x3C
pattern GREATER = 0x3E
pattern QUESTION = 0x3F
pattern AT = 0x40
pattern LBRACKET = 0x5B
pattern BACKSLASH = 0x5C
pattern RBRACKET = 0x5D
pattern GRAVE = 0x60
pattern LBRACE = 0x7B
pattern PIPE = 0x7C
pattern RBRACE = 0x7D

isWhite :: Word8 -> Bool
isWhite w = w == SPACE || w == TAB

isBreak :: Word8 -> Bool
isBreak w = w == LF || w == CR

-- | ns-char. Every byte of a multibyte character counts, because the input
-- contains printable characters only.
isNsChar :: Word8 -> Bool
isNsChar w = w > SPACE && w /= 0x7F

isFlowIndicator :: Word8 -> Bool
isFlowIndicator w =
  w == COMMA
    || w == LBRACKET
    || w == RBRACKET
    || w == LBRACE
    || w == RBRACE

isIndicator :: Word8 -> Bool
isIndicator w = w < 0x80 && testBit indicators (fromIntegral w)
  where
    indicators :: Integer
    indicators = foldr (\c acc -> setBit acc (ord c)) 0 ("-?:,[]{}#&*!|>'\"%@`" :: String)

isDecDigit :: Word8 -> Bool
isDecDigit w = w >= 0x30 && w <= 0x39

isHexDigit' :: Word8 -> Bool
isHexDigit' w = isDecDigit w || (w >= 0x41 && w <= 0x46) || (w >= 0x61 && w <= 0x66)

hexValue :: Word8 -> Int
hexValue w
  | w <= 0x39 = fromIntegral w - 0x30
  | w <= 0x46 = fromIntegral w - 0x37
  | otherwise = fromIntegral w - 0x57

isWordChar :: Word8 -> Bool
isWordChar w =
  isDecDigit w
    || (w >= 0x41 && w <= 0x5A)
    || (w >= 0x61 && w <= 0x7A)
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

isBom :: Env -> Int -> Bool
isBom e i = byteAt e i == 0xEF && byteAt e (i + 1) == 0xBB && byteAt e (i + 2) == 0xBF

skipBoms :: Env -> Int -> Int
skipBoms e i = if isBom e i then skipBoms e (i + 3) else i

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
  | i - 3 >= e.base
      && A.unsafeIndex e.array (i - 3) == 0xEF
      && A.unsafeIndex e.array (i - 2) == 0xBB
      && A.unsafeIndex e.array (i - 1) == 0xBF =
      isStartOfLine e (i - 3)
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
        [() | x <- [p .. q - 1], let w = byteAt e x, w < 0x80 || w >= 0xC0]
