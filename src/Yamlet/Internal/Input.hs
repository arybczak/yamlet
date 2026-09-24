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

import Yamlet.Error
import Yamlet.Internal.Syntax

-- | Decode the bytes of a stream to text. The encoding is UTF-8, UTF-16 or
-- UTF-32, detected as the YAML specification describes.
decodeInput :: BS.ByteString -> Either Error T.Text
decodeInput bs = case map (BS.indexMaybe bs) [0 .. 3] of
  [Just 0, Just 0, Just 0xFE, Just 0xFF] -> Right $ lenient T.decodeUtf32BEWith (BS.drop 4 bs)
  [Just 0xFF, Just 0xFE, Just 0, Just 0] -> Right $ lenient T.decodeUtf32LEWith (BS.drop 4 bs)
  [Just 0xFE, Just 0xFF, _, _] -> Right $ lenient T.decodeUtf16BEWith (BS.drop 2 bs)
  [Just 0xFF, Just 0xFE, _, _] -> Right $ lenient T.decodeUtf16LEWith (BS.drop 2 bs)
  [Just 0, Just 0, Just 0, Just _] -> Right $ lenient T.decodeUtf32BEWith bs
  [Just x, Just 0, Just 0, Just 0] | x /= 0 -> Right $ lenient T.decodeUtf32LEWith bs
  Just 0 : Just _ : _ -> Right $ lenient T.decodeUtf16BEWith bs
  Just x : Just 0 : _ | x /= 0 -> Right $ lenient T.decodeUtf16LEWith bs
  _ -> utf8
  where
    lenient :: (T.OnDecodeError -> BS.ByteString -> T.Text) -> BS.ByteString -> T.Text
    lenient f = f T.lenientDecode

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
