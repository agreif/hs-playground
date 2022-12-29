module Main (main) where

main :: IO ()
main = do
  putStrLn "hello world"

reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount funcCount list = (funcCount + 1, reverse list)

appendReversedWithCount :: Int -> [a] -> [a] -> (Int, [a])
appendReversedWithCount funcCount list1 list2 =
  let (funcCount', revList1) = reverseWithCount funcCount list1
      (funcCount'', revList2) = reverseWithCount funcCount' list2
   in (funcCount'' + 1, revList1 ++ revList2)

append3ReversedWithCount :: Int -> [a] -> [a] -> [a] -> (Int, [a])
append3ReversedWithCount funcCount list1 list2 list3 =
  let (funcCount', revList1) = reverseWithCount funcCount list1
      (funcCount'', revList2) = reverseWithCount funcCount' list2
      (funcCount''', revList3) = reverseWithCount funcCount'' list3
   in (funcCount''' + 1, revList1 ++ revList2 ++ revList3)

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (\s -> (s, ()))

modify :: (s -> s) -> State s ()
-- modify f = get >>= (\s -> put (f s))
modify f = do
  s <- get
  put (f s)

reverseWithCount'' :: [a] -> State Int [a]
reverseWithCount'' list = do
  modify (+ 1)
  return $ reverse list

appendReversedWithCount'' :: [a] -> [a] -> State Int [a]
appendReversedWithCount'' list1 list2 = do
  l1 <- reverseWithCount'' list1
  l2 <- reverseWithCount'' list2
  modify (+ 1)
  return $ l1 ++ l2

append3ReversedWithCount'' :: [a] -> [a] -> [a] -> State Int [a]
append3ReversedWithCount'' list1 list2 list3 = do
  l1 <- reverseWithCount'' list1
  l2 <- reverseWithCount'' list2
  l3 <- reverseWithCount'' list3
  modify (+ 1)
  return $ l1 ++ l2 ++ l3

data State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> state s b
  fmap f (State stateFn) =
    State
      ( \s ->
          let (s', result) = stateFn s
           in (s', f result)
      )

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure x = State (\s -> (s, x))

  -- <*> :: State s (a -> b) -> State s a -> State s b
  (State stateFx) <*> (State stateX) =
    State
      ( \s ->
          let (s', fx) = stateFx s
              (s'', x) = stateX s'
           in (s'', fx x)
      )

instance Monad (State s) where
  -- >>= :: State s a -> (a -> State s b) -> State s b
  (State stateX) >>= f =
    State
      ( \s ->
          let (s', x) = stateX s
           in runState (f x) s'
      )
--
--
-- reverseWithCount' :: [a] -> State Int [a]
-- reverseWithCount' list = State (\s -> (s + 1, reverse list))
--
-- appendReversedWithCount' :: [a] -> [a] -> State Int [a]
-- appendReversedWithCount' list1 list2 =
--   reverseWithCount' list1
--     >>= ( \revList1 ->
--             reverseWithCount' list2
--               >>= ( \revList2 ->
--                       State (\s -> (s + 1, revList1 ++ revList2))
--                   )
--         )

-- append3ReversedWithCount' :: [a] -> [a] -> [a] -> State Int [a]
-- append3ReversedWithCount' list1 list2 list3 =
--   reverseWithCount' list1
--     >>= ( \revList1 ->
--             reverseWithCount' list2
--               >>= ( \revList2 ->
--                       reverseWithCount' list3
--                         >>= ( \revList3 ->
--                                 State (\s -> (s + 1, revList1 ++ revList2 ++ revList3))
--                             )
--                   )
--         )
