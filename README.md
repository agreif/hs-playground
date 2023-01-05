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
(<*>) :: Applicative f => f (a -> b) -> f a -> f b     -- Prelude
ap    :: Monad m       => m (a -> b) -> m a -> m b     -- Control.Monad

pure   :: Applicative f => a -> f a                    -- Prelude
return :: Monad m       => a -> m a                    -- Prelude

(*>) :: Applicative f        => f a -> f b -> f b      -- Prelude
(>>) :: forall a b . Monad m => m a -> m b -> m b      -- Prelude

traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
mapM     :: (Traversable t, Monad m)       => (a -> m b) -> t a -> m (t b)

sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence  :: (Traversable t, Monad m)       => t (m a) -> m (t a)

liftA :: Applicative f => (a -> b) -> f a -> f b       -- Control.Applicative
liftM :: Monad m       => (a1 -> r) -> m a1 -> m r     -- Control.Monad

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c      -- Control.Applicative
liftM2 :: Monad m       => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r  -- Control.Monad

-- for Alternative vs MonadPlus:

empty :: Alternative f => f a                          -- Control.Applicative
mzero :: MonadPlus m   => m a                          -- Control.Monad

(<|>) :: Alternative f => f a -> f a -> f a            -- Control.Applicative
mplus :: MonadPlus m   => m a -> m a -> m a            -- Control.Monad
```


## Language Extensions

- [`DeriveAnyClass`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/derive_any_class.html)
- DeriveGeneric
- [GeneralizedNewtypeDeriving](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html)
- [StandaloneDeriving](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/standalone_deriving.html?highlight=standalonederiving#extension-StandaloneDeriving)
- [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html)
- [DerivingStrategies](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_strategies.html)


## NonEmpty

These packages implement non-empty lists:

- [NonEmpty](http://hackage.haskell.org/package/NonEmpty)
- [NonEmptyList](http://hackage.haskell.org/package/NonEmptyList)
- [Cardinality](http://hackage.haskell.org/package/Cardinality)
- [non-empty](http://hackage.haskell.org/package/non-empty)
- [semigroups](http://hackage.haskell.org/package/semigroups)
- [mono-traversable](http://hackage.haskell.org/package/mono-traversable)


## Encoding invariants at the type level

[Encoding invariants using types](https://www.dcc.fc.up.pt/~pbv/aulas/tapf/handouts/gadts.html)
[Type Invariants for Haskell](https://www.iro.umontreal.ca/~monnier/lemmas.pdf)
[Eliminating Bugs with Dependent Haskell](https://dl.acm.org/doi/pdf/10.1145/3406088.3409020)

## Custom Preludes

[protolude](https://www.stephendiehl.com/posts/protolude.html)
[universum](https://hackage.haskell.org/package/universum)
[relude](https://hackage.haskell.org/package/relude)

- Avoid all partial functions
- Use more efficient string representations
- ...


## OverloadedLists

[OverloadedLists](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_lists.html)
This extension allows programmers to use the list notation for construction of structures like: Set, Map, IntMap, Vector, Text and Array

IsList class


## Data.Word and Data.Int

[Unsigned integer types - Data.Word](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Word.html)
[Signed integer types - Data.Int](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Int.html)


## Why to not use lists

[When You Should Use Lists in Haskell](https://www.imn.htwk-leipzig.de/~waldmann/etc/untutorial/list-or-not-list/)
[Lists are not ...](https://wiki.haskell.org/Haskell_programming_tips#Lists_are_not_good_for_everything)


## Foldable and Traversable

[Data.Foldable](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Foldable.html)
The Foldable class generalises some common Data.List functions to structures that can be reduced to a summary value one element at a time.

[Data.Traversable](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Traversable.html)

traverse turns things inside a Traversable into a Traversable of things "inside" an Applicative

Every Traversable structure is both a Functor and Foldable because it is possible to implement the requisite instances in terms of traverse by using fmapDefault for fmap and foldMapDefault for foldMap.


## Semigroup and Foldable

[Data.Semigroup](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Semigroup.html)

A type a is a Semigroup if it provides an associative function (<>) that lets you combine any two values of type a into one.

`Min`
`Max`
`First`
`Last`
`Dual` The dual of a Monoid, obtained by swapping the arguments of mappend.
`All` Boolean monoid under conjunction (&&).
`Any` Boolean monoid under disjunction (||).
`Sum` Monoid under addition.
`Product` Monoid under multiplication.


[Data.Monoid](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Monoid.html)

A type a is a Monoid if it provides an associative function (<>) that lets you combine any two values of type a into one, and a neutral element (mempty) such that

`a <> mempty == mempty <> a == a`

A Monoid is a Semigroup with the added requirement of a neutral element. Thus any Monoid is a Semigroup,

`Dual` The dual of a Monoid, obtained by swapping the arguments of mappend
`All` Boolean monoid under conjunction (&&).
`Any` Boolean monoid under disjunction (||).
`Sum` Monoid under addition.
`Product` Monoid under multiplication.
`First` Maybe monoid returning the leftmost non-Nothing value.
`Last` Maybe monoid returning the rightmost non-Nothing value.
`Alt` Monoid under <|>.
`Ap` This data type witnesses the lifting of a Monoid into an Applicative pointwise.


## LambdaCase

[LambdaCase](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lambda_case.html)

```haskell
\case
  p1 -> e1
  pN -> eN

which is equivalent to

\freshName -> case freshName of
                p1 -> e1
		pN -> eN
```


## MultiWayIf

[MultiWayIf](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/multiway_if.html)

```haskell
if | guard1 -> expr1
   | ...
   | guardN -> exprN

which is roughly equivalent to

case () of
  _ | guard1 -> expr1
  ...
  _ | guardN -> exprN

```


## TupleSections

[TupleSections](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/tuple_sections.html)

```haskell
(, True)

is alternative for

\x -> (x, True)

(, "I", , , "Love", , 1337)

which translates to

\a b c d -> (a, "I", b, c, "Love", d, 1337)


## BlockArguments

[BlockArguments](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html)

Allow do expressions, lambda expressions, etc. to be directly used as a function argument.

```haskell
when (x > 0) do
  print x
  exitFailure

will be parsed as:

when (x > 0) (do
  print x
  exitFailure)
```


## NumericUnderscores

```haskell
-- decimal
million    = 1_000_000

-- float
pi       = 3.141_592_653_589_793

-- function
isUnderMillion = (< 1_000_000)
```


## ScopedTypeVariables

[ScopedTypeVariables](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/scoped_type_variables.html)

GHC supports lexically scoped type variables, without which some type signatures are simply impossible to write. For example:

```haskell
f :: forall a. [a] -> [a]
f xs = ys ++ ys
     where
       ys :: [a]
       ys = reverse xs
```

### Declaration type signatures

[Declaration type signatures](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/scoped_type_variables.html#decl-type-sigs)

A declaration type signature that has explicit quantification (using forall) brings into scope the explicitly-quantified type variables, in the definition of the named function. For example:

```haskell
f :: forall a. [a] -> [a]
f (x:xs) = xs ++ [ x :: a ]
```

The “forall a” brings “a” into scope in the definition of “f”.


## TypeApplications

[TypeApplications](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html)

Allow the use of type application syntax.

The TypeApplications extension allows you to use visible type application in expressions. Here is an example: `show (read @Int "5")`. The `@Int` is the visible type application; it specifies the value of the type variable in read’s type.
