# hs-playground

My Collection of some Haskell topics, programming exercises and ideas.

# Some Concepts


## ByteString, Text, String, OverloadedStrings

from [https://mmhaskell.com/blog/2022/2/17/taking-a-byte-out-of-strings](https://mmhaskell.com/blog/2022/2/17/taking-a-byte-out-of-strings)

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

from [http://learnyouahaskell.com/zippers](http://learnyouahaskell.com/zippers):

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

from [https://smunix.github.io/blog.jakuba.net/2014/07/20/mutable-state-in-haskell.html](https://smunix.github.io/blog.jakuba.net/2014/07/20/mutable-state-in-haskell.html)

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
While an IORef must always have a value, MVar can be empty.

if we try to do takeMVar from an empty MVar, it will block the thread until someone else puts a value into the MVar. The same thing happens when you try to putMVar into an MVar that already has a value, it will block until someone takes that value out. they can be be used to serve as synchronization primitives for communication between threads

```haskell
newMVar :: a -> IO (MVar a)
newEmptyMVar :: IO (MVar a)
takeMVar :: MVar a -> IO a
```
```haskell
main :: IO ()
main = do
    a <- newEmptyMVar
    putMVar a "hello"
    takeMVar a >>= print
```

- TVar in Software Transactional Memory (STM)
The way that STM works is that it builds up a log of actions that are to be performed atomically.

```haskell
data TVar a

newTVar    :: a -> STM (TVar a)
readTVar   :: TVar a -> STM a
writeTVar  :: TVar a -> a -> STM ()
modifyTVar :: TVar a -> (a -> a) -> STM ()
```

to run the actual STM transaction we must use the function `atomically :: STM a -> IO a`

```haskell
atomicReadWrite :: IO ()
atomicReadWrite = do
    var <- newTVarIO (0 :: Int)

    atomically $ do
        value <- readTVar var
        writeTVar var (value + 1)

    readTVarIO var >>= print
```


## Applicatives vs. Monads

from [https://kseo.github.io/posts/2014-01-26-swtiching-from-monads-to-applicative-functors.html](https://kseo.github.io/posts/2014-01-26-swtiching-from-monads-to-applicative-functors.html)
from [https://wiki.haskell.org/Applicative_functor#Some_advantages_of_applicative_functors](https://wiki.haskell.org/Applicative_functor#Some_advantages_of_applicative_functors)

- Code that uses only the Applicative interface is more general than code that uses the Monad interface, because there are more applicative functors than monads.
- Programming with Applicative has a more applicative/functional feel
- Applicative functors do not need special transformers because they can be combined in a generic way

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b   Prelude
ap    :: Monad m       => m (a -> b) -> m a -> m b   Control.Monad

pure   :: Applicative f => a -> f a                  Prelude
return :: Monad m       => a -> m a                  Prelude

(*>) :: Applicative f        => f a -> f b -> f b    Prelude
(>>) :: forall a b . Monad m => m a -> m b -> m b    Prelude

traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
mapM     :: (Traversable t, Monad m)       => (a -> m b) -> t a -> m (t b)

sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence  :: (Traversable t, Monad m)       => t (m a) -> m (t a)

liftA :: Applicative f => (a -> b) -> f a -> f b     Control.Applicative
liftM :: Monad m       => (a1 -> r) -> m a1 -> m r   Control.Monad

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c      Control.Applicative
liftM2 :: Monad m       => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r  Control.Monad

-- for Alternative vs MonadPlus:

empty :: Alternative f => f a            Control.Applicative
mzero :: MonadPlus m   => m a            Control.Monad

(<|>) :: Alternative f => f a -> f a -> f a     Control.Applicative
mplus :: MonadPlus m   => m a -> m a -> m a     Control.Monad
```
