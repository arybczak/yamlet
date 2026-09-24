-- | A YAML 1.2.2 library.
--
-- Decode a configuration file:
--
-- @
-- data Config = Config
--   { name :: Text
--   , paths :: [FilePath]
--   }
--
-- instance FromYAML Config where
--   parseYAML = withMapping $ \\o -> do
--     rejectUnknownKeys ["name", "paths"] o
--     Config \<$> o .: "name" \<*> o .:? "paths" .!= []
--
-- main :: IO ()
-- main = do
--   input <- BS.readFile "config.yaml"
--   case decode input of
--     Left err -> putStr $ prettyError "config.yaml" err
--     Right config -> ...
-- @
module Yamlet
  ( -- * Decoding
    decode
  , decodeAll
  , decodeText
  , decodeAllText
  , decodeNodes
  , decodeInput

    -- * Encoding
  , encode
  , encodeAll
  , encodeText
  , encodeAllText

    -- * Nodes
  , module Yamlet.Node

    -- * Conversion from nodes
  , module Yamlet.Decode

    -- * Conversion to nodes
  , ToYAML(..)
  , (.=)
  , mapping

    -- * Errors
  , module Yamlet.Error
  ) where

import Data.Bits
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T

import Yamlet.Decode
import Yamlet.Encode
import Yamlet.Error
import Yamlet.Internal.Compose
import Yamlet.Internal.Parser
import Yamlet.Node

-- | Decode a stream with one document. An empty stream is null.
decode :: FromYAML a => BS.ByteString -> Either Error a
decode bs = decodeInput bs >>= decodeText

-- | Decode every document of a stream.
decodeAll :: FromYAML a => BS.ByteString -> Either Error [a]
decodeAll bs = decodeInput bs >>= decodeAllText

-- | Decode a stream with one document. An empty stream is null.
decodeText :: FromYAML a => T.Text -> Either Error a
decodeText input = decodeNodes input >>= \case
  [] -> convert input (Node (Offset 0) nullTag Null)
  [n] -> convert input n
  _ : n : _ -> Left $ errorAt input n.offset "expected a single document"

-- | Decode every document of a stream.
decodeAllText :: FromYAML a => T.Text -> Either Error [a]
decodeAllText input = decodeNodes input >>= mapM (convert input)

-- | Parse a stream into the root nodes of its documents.
decodeNodes :: T.Text -> Either Error [Node]
decodeNodes input = parseStream input >>= mapM (compose input)

convert :: FromYAML a => T.Text -> Node -> Either Error a
convert input n = case runParser parseYAML n of
  Right a -> Right a
  Left (off, msg) -> Left $ errorAt input off msg

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
              k | w < 0x80 = 1
                | w .&. 0xE0 == 0xC0 = 2
                | w .&. 0xF0 == 0xE0 = 3
                | w .&. 0xF8 == 0xF0 = 4
                | otherwise = 0
          in if k > 0 && i + k <= BS.length bs
                && either (const False) (const True) (T.decodeUtf8' (BS.take k (BS.drop i bs)))
               then validPrefix (i + k)
               else i

-- | Encode a value as a document.
encode :: ToYAML a => a -> BS.ByteString
encode = T.encodeUtf8 . encodeText

-- | Encode values as a stream of documents.
encodeAll :: ToYAML a => [a] -> BS.ByteString
encodeAll = T.encodeUtf8 . encodeAllText

encodeText :: ToYAML a => a -> T.Text
encodeText a = renderDocuments [toYAML a]

encodeAllText :: ToYAML a => [a] -> T.Text
encodeAllText = renderDocuments . map toYAML
