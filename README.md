# hs-playground

Collection of small independent Haskell topics, programming exercises and ideas.

# Some Concepts


## ByteString, Text, String, OverloadedStrings

- Text types capture a unicode representation of character data
```
  Data.Text
  pack :: String -> Text
  unpack :: Text -> String
  encodeUtf8 :: Text -> ByteString
  decodeUtf8 :: ByteString -> Text
```
- String is a list of the Char type

- ByteString is more low-level, storing its information at the "byte" level.
```
  List of Word8 - an 8-bit (1 byte) unsigned integer
  Data.ByteString
  pack :: [Word8] -> ByteString
  unpack :: ByteString -> [Word8]
```
- Text implements the IsString class
```
  >> import qualified Data.Text as T
  >> :set -XOverloadedStrings
  >> let t = "Hello" :: T.Text

  >> import qualified Data.Text as T
  >> import qualified Data.Text.Lazy as TL
  >> let t1 = T.pack "Hello"
  >> let t2 = TL.pack "World"
  >> let t3 = TL.fromStrict t1
  >> let t4 = TL.toStrict t2
```

## Functional Fippers

"A zipper is a technique of representing an aggregate data structure so that it is convenient for writing programs that traverse the structure arbitrarily and update its contents" (from Wikipedia)

excerpt from LYAH:

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Crumb a
     = LeftCrumb a (Tree a)
     | RightCrumb a (Tree a)
     deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

...
```


## Return type polymorphism
from [https://eli.thegreenplace.net/2018/return-type-polymorphism-in-haskell/](https://eli.thegreenplace.net/2018/return-type-polymorphism-in-haskell/)

- Parametric polymorphism is possible when we can define a certain operation to work similarly on any type
```haskell
length :: [a] -> Int
```

- Ad-hoc polymorphism is achieved by using typeclasses
```haskell
instance Ord Person where
  ...
```
