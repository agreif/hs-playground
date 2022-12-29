# hs-playground

My Collection of some Haskell topics, programming exercises and ideas.

# Some Concepts


## ByteString, Text, String, OverloadedStrings
From [https://mmhaskell.com/blog/2022/2/17/taking-a-byte-out-of-strings](https://mmhaskell.com/blog/2022/2/17/taking-a-byte-out-of-strings)

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

## Functional Zippers

"A zipper is a technique of representing an aggregate data structure so that it is convenient for writing programs that traverse the structure arbitrarily and update its contents" (from Wikipedia)

From [http://learnyouahaskell.com/zippers](http://learnyouahaskell.com/zippers):

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
From [https://eli.thegreenplace.net/2018/return-type-polymorphism-in-haskell/](https://eli.thegreenplace.net/2018/return-type-polymorphism-in-haskell/)

- Parametric polymorphism is possible when we can define a certain operation to work similarly on any type
```haskell
length :: [a] -> Int
```

- Ad-hoc polymorphism is achieved by using typeclasses
```haskell
instance Ord Person where
  ...
```

- Return-type polymophism
```haskell
read :: Read a => String -> a
> read "1"

<interactive>:46:1:
    No instance for (Read a0) arising from a use of `read'
    The type variable `a0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    <...>

> read "1" :: Int
1

> read "1" :: Double
1.0
```

type inference with return-type polymophism:
```haskell
> putStrLn (take (read "2") (read "\"haskell\""))
ha
```


## Mutability and State
ways to achieve mutable state in Haskell, let’s take a look at them:
- IORef
mutable reference to a type. An IORef must always contain a value of a given type, it is impossible to create it empty
```haskell
data IORef a

newIORef    :: a -> IO (IORef a)
readIORef   :: IORef a -> IO a
writeIORef  :: IORef a -> a -> IO ()
modifyIORef :: IORef a -> (a -> a) -> IO ()
```

- STRef in the ST monad
ability to escape from the ST monad with the `runST :: ST s a -> a` function, making the computation pure
```haskell
data STRef s a

newSTRef    :: a -> ST s (STRef s a)
readSTRef   :: STRef s a -> ST s a
writeSTRef  :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
```
```haskell
import Control.Monad.ST
import Data.STRef

magic :: Int -> Int                <- pure!
magic x = runST $ do
    ref <- newSTRef x

    modifySTRef ref (+1)

    readSTRef ref
```

- MVar
- TVar in Software Transactional Memory (STM)



