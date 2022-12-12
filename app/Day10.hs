module Day10 (day10)  where

import Utils
import Parse
import Data.Functor
import Data.List (singleton)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data Instruction = Noop | Add Integer deriving (Show, Eq)



instruction =  concat <$> sepEndBy (choice [add, noop]) newline
  where add = fmap (Noop:) $ string "addx " *> (singleton . Add <$> number)
        noop = string "noop" $> [Noop]

signal :: [Instruction] -> [Integer]
signal = scanl go 1
  where go x Noop = x
        go x (Add n) = x + n


signalStrength :: [Integer] -> Integer
signalStrength l =  sum $ map strength [20,60..220]
  where strength i = fromIntegral i * (l !! (i - 1))
        
render :: [Integer] -> String
render = zipWith (\i x -> if abs (x - (i `mod` 40)) <= 1 then '#' else '.') [0..]

day10 :: IO ()
day10 = interact (either show (unlines . init . chunksOf 40 . render . signal) . parse instruction "")
