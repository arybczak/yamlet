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
import Data.ByteString.Unsafe qualified as BS
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
  [Just 0, Just 0, Just 0xFE, Just 0xFF] -> utf32 T.decodeUtf32BEWith be32 (BS.drop 4 bs)
  [Just 0xFF, Just 0xFE, Just 0, Just 0] -> utf32 T.decodeUtf32LEWith le32 (BS.drop 4 bs)
  [Just 0xFE, Just 0xFF, _, _] -> utf16 T.decodeUtf16BEWith be16 (BS.drop 2 bs)
  [Just 0xFF, Just 0xFE, _, _] -> utf16 T.decodeUtf16LEWith le16 (BS.drop 2 bs)
  [Just 0, Just 0, Just 0, Just _] -> utf32 T.decodeUtf32BEWith be32 bs
  [Just x, Just 0, Just 0, Just 0] | x /= 0 -> utf32 T.decodeUtf32LEWith le32 bs
  Just 0 : Just _ : _ -> utf16 T.decodeUtf16BEWith be16 bs
  Just x : Just 0 : _ | x /= 0 -> utf16 T.decodeUtf16LEWith le16 bs
  _ -> utf8
  where
    -- A copy for each reading function makes the loops fast.
    {-# INLINE utf32 #-}
    {-# INLINE utf16 #-}

    utf32
      :: (T.OnDecodeError -> BS.ByteString -> T.Text)
      -> (BS.ByteString -> Int -> Int)
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
            c = unit input i

    utf16
      :: (T.OnDecodeError -> BS.ByteString -> T.Text)
      -> (BS.ByteString -> Int -> Int)
      -> BS.ByteString
      -> Either Error T.Text
    utf16 decodeWith unit input = checked "invalid UTF-16" decodeWith input (go 0)
      where
        go :: Int -> Int
        go i
          | i + 2 > BS.length input = i
          | u >= 0xD800 && u <= 0xDBFF =
              if i + 4 <= BS.length input && isLow (unit input (i + 2)) then go (i + 4) else i
          | isLow u = i
          | otherwise = go (i + 2)
          where
            u :: Int
            u = unit input i

        isLow :: Int -> Bool
        isLow u = u >= 0xDC00 && u <= 0xDFFF

    isSurrogate :: Int -> Bool
    isSurrogate c = c >= 0xD800 && c <= 0xDFFF

    -- The code unit at the index. The callers make sure that its bytes are in
    -- the input.
    be16, le16, be32, le32 :: BS.ByteString -> Int -> Int
    be16 input i = byte input i `shiftL` 8 .|. byte input (i + 1)
    le16 input i = byte input (i + 1) `shiftL` 8 .|. byte input i
    be32 input i = be16 input i `shiftL` 16 .|. be16 input (i + 2)
    le32 input i = le16 input (i + 2) `shiftL` 16 .|. le16 input i

    byte :: BS.ByteString -> Int -> Int
    byte input i = fromIntegral (BS.unsafeIndex input i)

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
        -- The length of the longest valid prefix.
        let valid = fst (T.validateUtf8Chunk bs)
            prefix = T.decodeUtf8 (BS.take valid bs)
        in Left $ errorAt prefix (Offset valid) "invalid UTF-8"
