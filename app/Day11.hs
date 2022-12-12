{-# LANGUAGE RankNTypes #-}

module Day11  where
import Utils
import Lens.Family2.State.Strict
import Data.List (sort)
import Lens.Family2.Stock (ix)
import Control.Monad (guard, forM_)
import Lens.Family2
import Debug.Trace
import Control.Monad.Trans.State.Strict (State(..), get, modify, get)




element :: Int -> Lens' [a] a
element i k l = fmap (exchange i) (k (l !! i))
  where exchange i x = top ++ (x:(tail rest))
        (top, rest) = splitAt i l

top :: Lens' [a] a
top k l = fmap (:tail l) (k (head l))

peek :: Int -> State MonkeyState Integer
peek i = head . (!! i) . stacks <$> get

pop :: Int -> State MonkeyState Integer
pop i = do
  n <- peek i
  zoom (stacksLens . element i) (modify tail)
  return n

queue :: Int -> Integer -> State MonkeyState ()
queue j n = zoom (stacksLens . element j) (modify (++ [n]))

alterStack :: Int -> (Integer -> Integer) -> State MonkeyState ()
alterStack i f = zoom (stacksLens . element i . top) (modify f)

transfer :: Int -> Int -> State MonkeyState ()
transfer i j = pop i >>= queue j

bump :: Int -> State MonkeyState ()
bump i = zoom (transfersLens . element i) (modify (+1))

type Transfer = (Int, Int)
data Monkey = Monkey {
  index :: Int,
  operation :: Integer -> Integer,
  test :: Integer -> Int
                     }

data MonkeyState = MonkeyState {
  stacks :: [[Integer]],
  transfers :: [Integer]
                 } deriving (Show)
stacksLens :: Lens' MonkeyState [[Integer]]
stacksLens f m = fmap (\x -> m {stacks = x}) (f $ stacks m)

transfersLens :: Lens' MonkeyState [Integer]
transfersLens f m = fmap (\x -> m {transfers = x}) (f $ transfers m)

monkey :: Monkey -> State MonkeyState ()
monkey m = do
  t <- (!! index m) . stacks <$> get
  if null t then return ()
    else do
    alterStack (index m) ((`mod` 9699690). operation m)
    st <- get
    n <- peek (index m)
    transfer (index m) (test m $ n)
    bump (index m)
    monkey m
  
  
    
monkeys :: [Monkey] -> State MonkeyState ()
monkeys = flip forM_ monkey

rounds :: Int -> [Monkey] -> State MonkeyState ()
rounds i l = monkeys (take (length l * i) $ cycle l)

inspections :: Int -> [Transfer] -> Int
inspections i = countAll ((== i). fst)


testMonkeys = [
  Monkey {
      index = 0,
      operation = \x -> x * 7,
      test = \x -> if x `mod` 3 == 0 then 3 else 7
               },
    Monkey {
      index = 1,
      operation = \x -> x + 5,
      test = \x -> if x `mod` 11 == 0 then 6 else 4
           },
    Monkey {
      index = 2,
      operation = \x -> x * x,
      test = \x -> if x `mod` 7 == 0then 0 else 7
           },
    Monkey {
      index = 3,
      operation = \x -> x + 4,
      test = \x -> if x `mod` 2 == 0 then 5 else 1
           },
    Monkey {
      index = 4,
      operation = \x -> x * 17,
      test = \x -> if x `mod` 19 == 0 then 2 else 6
      },
    Monkey {
      index = 5,
      operation = \x -> x + 7,
      test = \x -> if x `mod` 5 == 0 then 1 else 4
                 },
    Monkey {
      index = 6,
      operation = \x -> x + 6,
      test = \x -> if x `mod` 17 == 0 then 2 else 0
           },
    Monkey {
      index = 7,
      operation = \x -> x + 3,
      test = \x -> if x `mod` 13 == 0 then 3 else 5
           }
              ]
    
        

starting = MonkeyState {
  stacks = [[56, 56, 92, 65, 71, 61, 79],
            [61, 85],
            [54, 96, 82, 78, 69],
            [57, 59, 65, 95],
            [62, 67, 80],
            [91],
            [79, 83, 64, 52, 77, 56, 63, 92],
            [50, 97, 76, 96, 80, 56]
           ],
           transfers = replicate 8 0
                       
                       }
  
  
  

day11 :: IO ()
day11 = mempty
