module Main where

import System.Environment (getArgs)
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day10
import Day11
import Day12
-- import Day13
import Day14

-- Unimportant parsing code START

-- -- reads in one number

-- TODO: where did day 9 go?
days :: [IO ()]
days = [day1, day2, day3, day4, day5, day6, day7, day8, mempty, day10, day11, day12, mempty, day14]

main :: IO ()
main = do
  args <- getArgs
  days !! (read (head args) - 1)
