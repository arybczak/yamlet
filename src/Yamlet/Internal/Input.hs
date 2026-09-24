{-# OPTIONS_HADDOCK not-home #-}

-- | Detection of the encoding of the input.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Input
  ( decodeInput
  ) where

import Data.Bits
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.Unsafe qualified as T

import Yamlet.Error
import Yamlet.Internal.Syntax

-- | Decode the bytes of a stream to text. The encoding is UTF-8, UTF-16 or
-- UTF-32, detected as the YAML specification describes.
decodeInput :: BS.ByteString -> Either Error T.Text
decodeInput bs = case map (BS.indexMaybe bs) [0 .. 3] of
  [Just 0, Just 0, Just 0xFE, Just 0xFF] -> utf32 T.decodeUtf32BEWith bigEndian (BS.drop 4 bs)
  [Just 0xFF, Just 0xFE, Just 0, Just 0] -> utf32 T.decodeUtf32LEWith littleEndian (BS.drop 4 bs)
  [Just 0xFE, Just 0xFF, _, _] -> utf16 T.decodeUtf16BEWith bigEndian (BS.drop 2 bs)
  [Just 0xFF, Just 0xFE, _, _] -> utf16 T.decodeUtf16LEWith littleEndian (BS.drop 2 bs)
  [Just 0, Just 0, Just 0, Just _] -> utf32 T.decodeUtf32BEWith bigEndian bs
  [Just x, Just 0, Just 0, Just 0] | x /= 0 -> utf32 T.decodeUtf32LEWith littleEndian bs
  Just 0 : Just _ : _ -> utf16 T.decodeUtf16BEWith bigEndian bs
  Just x : Just 0 : _ | x /= 0 -> utf16 T.decodeUtf16LEWith littleEndian bs
  _ -> utf8
  where
    utf32
      :: (T.OnDecodeError -> BS.ByteString -> T.Text)
      -> (Int -> BS.ByteString -> Int -> Int)
      -> BS.ByteString
      -> Either Error T.Text
    utf32 decodeWith unit input = checked "invalid UTF-32" decodeWith input (go 0)
      where
        go :: Int -> Int
        go i
          | i + 4 > BS.length input = i
          | c <= 0x10FFFF && not (isSurrogate c) = go (i + 4)
          | otherwise = i
          where
            c :: Int
            c = unit 4 input i

    utf16
      :: (T.OnDecodeError -> BS.ByteString -> T.Text)
      -> (Int -> BS.ByteString -> Int -> Int)
      -> BS.ByteString
      -> Either Error T.Text
    utf16 decodeWith unit input = checked "invalid UTF-16" decodeWith input (go 0)
      where
        go :: Int -> Int
        go i
          | i + 2 > BS.length input = i
          | u >= 0xD800 && u <= 0xDBFF =
              if i + 4 <= BS.length input && isLow (unit 2 input (i + 2)) then go (i + 4) else i
          | isLow u = i
          | otherwise = go (i + 2)
          where
            u :: Int
            u = unit 2 input i

        isLow :: Int -> Bool
        isLow u = u >= 0xDC00 && u <= 0xDFFF

    isSurrogate :: Int -> Bool
    isSurrogate c = c >= 0xD800 && c <= 0xDFFF

    -- The unsigned integer of the given number of bytes at the index.
    bigEndian, littleEndian :: Int -> BS.ByteString -> Int -> Int
    bigEndian k input i = foldl (\acc j -> acc * 256 + fromIntegral (BS.index input (i + j))) 0 [0 .. k - 1]
    littleEndian k input i = foldl (\acc j -> acc * 256 + fromIntegral (BS.index input (i + j))) 0 [k - 1, k - 2 .. 0]

    -- The text, or an error at the end of the valid prefix of the given length.
    checked
      :: String
      -> (T.OnDecodeError -> BS.ByteString -> T.Text)
      -> BS.ByteString
      -> Int
      -> Either Error T.Text
    checked msg decodeWith input valid
      | valid == BS.length input = Right $ decodeWith T.lenientDecode input
      | otherwise = Left $ errorAt prefix (Offset (T.lengthWord8 prefix)) msg
      where
        prefix :: T.Text
        prefix = decodeWith T.lenientDecode (BS.take valid input)

    utf8 :: Either Error T.Text
    utf8 = case T.decodeUtf8' bs of
      Right t -> Right t
      Left _ ->
        let valid = validPrefix 0
            prefix = T.decodeUtf8 (BS.take valid bs)
        in Left $ errorAt prefix (Offset valid) "invalid UTF-8"

    -- The length of the longest valid UTF-8 prefix.
    validPrefix :: Int -> Int
    validPrefix i
      | i >= BS.length bs = i
      | otherwise =
          let w = BS.index bs i
              k
                | w < 0x80 = 1
                | w .&. 0xE0 == 0xC0 = 2
                | w .&. 0xF0 == 0xE0 = 3
                | w .&. 0xF8 == 0xF0 = 4
                | otherwise = 0
          in if k > 0
               && i + k <= BS.length bs
               && either (const False) (const True) (T.decodeUtf8' (BS.take k (BS.drop i bs)))
               then validPrefix (i + k)
               else i
