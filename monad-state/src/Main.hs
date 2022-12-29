module Main (main) where

main :: IO ()
main = do
  putStrLn "hello world"

type ProgState = Int

foo :: StTr ()
foo = do
  let a = runStTr (bar 1) 1
  return ()

bar :: Int -> StTr String
bar i = do
  pure ""

--

-- State Transformer
newtype StTr a = StTr {runStTr :: ProgState -> (a, ProgState)}

instance Functor StTr where
  -- fmap :: (a -> b) -> ST a -> ST a
  fmap f stx =
    StTr
      ( \s ->
          let (x, s') = runStTr stx s
           in (f x, s')
      )

instance Applicative StTr where
  -- pure :: a -> ST a
  pure x = StTr (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    StTr
      ( \s ->
          let (f, s') = runStTr stf s
              (x, s'') = runStTr stx s'
           in (f x, s'')
      )

instance Monad StTr where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  stx >>= f =
    StTr
      ( \s ->
          let (x, s') = runStTr stx s
           in runStTr (f x) s'
      )
