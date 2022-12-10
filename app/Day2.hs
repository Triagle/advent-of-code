module Day2 (day2) where

import Utils
import Parse
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.List

data Move = Rock | Paper | Scissors deriving (Show, Eq)

data Outcome = Win | Loss | Draw deriving (Show, Eq)

move :: Parser Move
move = choice [char 'A' >> pure Rock, char 'B' >> pure Paper, char 'C' >> pure Scissors]

outcome :: Parser Outcome
outcome = choice [char 'X' >> pure Loss, char 'Y' >> pure Draw, char 'Z' >> pure Win]

-- A game is of the form "move outcome", we return that as the tuple (move, outcome)
game :: Parser (Move, Outcome)
game = (,) <$> (move <* space) <*> outcome

-- Likewise a strategy is a number of games separated by newlines
strategy :: Parser [(Move, Outcome)]
strategy = sepBy1Try game newline

-- Treating the moves as integers would let you write this in a cleaner way, but it's not very "Haskelly" to use integers to stand in as types...
toPlay :: Move -> Outcome -> Move
toPlay x Draw = x
toPlay Rock Win = Paper
toPlay Scissors Win = Rock
toPlay Paper Win = Scissors
toPlay Rock Loss = Scissors
toPlay Scissors Loss = Paper
toPlay Paper Loss = Rock

-- ...however here I decided to do that anyway :P
win :: Move -> Move -> Integer
win Scissors Rock = 1
win Rock Paper = 1
win Paper Scissors = 1
win x y
  | x == y = 0
  | otherwise = -1

-- I try to write Haskell functions like I would write a mathematical function, using the format 'f(x, y, ...) = ..., where x = ..., y = ...'.
-- Gets across the idea that Haskell functions should be thought of as like equations.
score :: Move -> Outcome -> Integer
score p1 outcome = shapeScore p2 + outcomeScore p1 p2
  where
    p2 = toPlay p1 outcome
    shapeScore Rock = 1
    shapeScore Paper = 2
    shapeScore Scissors = 3
    outcomeScore p1 p2 = 3 + win p1 p2 * 3


day2 :: IO ()
day2 = interact (showEither . fmap (sum . map (uncurry score)) . parse strategy "")
