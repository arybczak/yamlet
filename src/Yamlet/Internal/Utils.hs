{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Helpers for the other modules.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Yamlet.Internal.Utils
  ( textStripPrefix
  , textIsPrefixOf
  , readBoundedInt
  , maxImplicitKeyLength
  ) where

import Control.Monad
import Data.Char
import Data.Text qualified as T
#if !MIN_VERSION_text(2,1,4)
import Data.Text.Internal qualified as T
#endif

-- | 'T.stripPrefix'. Before text 2.1.4, 'T.stripPrefix' compares the texts as
-- streams of characters and allocates for each character. For these versions
-- this is the code of text 2.1.4, which compares the UTF-8 bytes.
textStripPrefix :: T.Text -> T.Text -> Maybe T.Text
#if MIN_VERSION_text(2,1,4)
textStripPrefix = T.stripPrefix
#else
textStripPrefix p@(T.Text _arr _off plen) t@(T.Text arr off len)
  | textIsPrefixOf p t = Just $! T.text arr (off + plen) (len - plen)
  | otherwise = Nothing
#endif

-- | 'T.isPrefixOf', with the code of text 2.1.4 as in 'textStripPrefix'.
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

-- | The value of decimal digits, or 'Nothing' if the text is empty, has a
-- character that is not a digit or is beyond the range of 'Int'.
readBoundedInt :: T.Text -> Maybe Int
readBoundedInt t
  | T.null t = Nothing
  | otherwise = T.foldl' step (Just 0) t
  where
    step :: Maybe Int -> Char -> Maybe Int
    step acc c = do
      n <- acc
      guard (isDigit c)
      let d = digitToInt c
      guard (n <= (maxBound - d) `quot` 10)
      pure (n * 10 + d)

-- | The largest number of characters of an implicit key, from the YAML 1.2.2
-- specification.
maxImplicitKeyLength :: Int
maxImplicitKeyLength = 1024
