module Day1 (day1) where

import Utils
import Parse
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.List

-- An elf has a list of energy counts, separated by newlines
elf :: Parser [Integer]
elf = sepBy1Try digits newline

-- the total shopping list is a bunch of individual shopping lists separated by blank lines
elves :: Parser [[Integer]]
elves = sepBy1Try elf (newline >> newline)
-- The actual thing doing all the solving here is the (sum . take 3 . reverse . sort .  map sum) bit. Here it is in parts:
-- map sum :: Takes the list of lists returned by elves and sums each individual elves calories.
-- sort :: Sorts the calorie counts from low to high
-- reverse :: To swap the order
-- take 3 :: take the top 3 items and
-- sum :: sum them up
-- All this needs to be wrapped in an fmap call because parse elves "" has type Either ParseError [[Integer]], and fmap lets you apply a function to such a type if it succeeds (and otherwise does nothing).
-- e.g. fmap (1+) (Right 3) = Right 4
-- but fmap (1+) (Left 2) = Left 2
-- The fmap call is implemented by the Either type, as it is an instance of the Functor typeclass.
day1 :: IO ()
day1 = interact (showEither . fmap (sum . take 3 . reverse . sort . map sum) . parse elves "")
