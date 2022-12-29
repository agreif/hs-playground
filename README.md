# hs-playground

Collection of small independent Haskell topics, programming exercises and ideas.

# Concepts

## ByteString, Text, String, OverloadedStrings

- Text types capture a unicode representation of character data
  Data.Text
  pack :: String -> Text
  unpack :: Text -> String
  encodeUtf8 :: Text -> ByteString
  decodeUtf8 :: ByteString -> Text

- String is a list of the Char type

- ByteString is more low-level, storing its information at the "byte" level.
  List of Word8 - an 8-bit (1 byte) unsigned integer
  Data.ByteString
  pack :: [Word8] -> ByteString
  unpack :: ByteString -> [Word8]

- Text implements the IsString class
  >> import qualified Data.Text as T
  >> :set -XOverloadedStrings
  >> let t = "Hello" :: T.Text
