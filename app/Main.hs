module Main where

import Data.Bifunctor (first)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Functor
import Data.List (sort, transpose)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Sq
import qualified Data.Set as S
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

-- Unimportant parsing code START

-- reads in one number
digits :: (Read a, Integral a) => Parser a
digits = read <$> many1 digit

line :: Parser ()
line = void $ manyTill anyChar newline

-- At least one of p, separated by sep, don't consume input if sep fails.
sepBy1Try :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1Try p sep = do
  x <- p
  xs <- many (try $ sep *> p)
  return (x : xs)

-- Unimportant parsing code END

-- An elf has a list of energy counts, separated by newlines
elf :: Parser [Integer]
elf = sepBy1Try digits newline

-- the total shopping list is a bunch of individual shopping lists separated by blank lines
elves :: Parser [[Integer]]
elves = sepBy1Try elf (newline >> newline)

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
day1 = getContents >>= printEither . fmap (sum . take 3 . reverse . sort . map sum) . parse elves ""

-- This one is even simpler than day 1, simply read in the data, map the rounds to scores and then sum them.
-- The only interesting thing is the use of the uncurry function. This take a function that has type f :: a -> b -> c
-- and turns it into a function on pairs f :: (a, b) -> c
-- e.g. if f x y = x + y then
-- f (2, 3) fails because f expects a number and got a tuple, but
-- uncurry f $ (2, 3) succeeds because uncurry f is a function that accepts a pair of numbers.
-- you can implement uncurry youself like so
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry f (x, y) = f x y
day2 :: IO ()
day2 = getContents >>= printEither . fmap (sum . map (uncurry score)) . parse strategy ""

priority :: Char -> Int
priority c
  | c >= 'A' && c < 'a' = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

rucksackPriority :: [Char] -> Int
rucksackPriority = priority . head . S.toList . uncurry intersect . half
  where
    half l = splitAt (length l `div` 2) l
    intersect l r = S.fromList l `S.intersection` S.fromList r

groupPriority :: Sq.Seq String -> Int
groupPriority = priority . head . S.toList . foldr1 S.intersection . fmap S.fromList

day3 :: IO ()
day3 = getContents >>= print . sum . fmap groupPriority . Sq.chunksOf 3 . Sq.fromList . lines

blank :: Parser (Maybe Char)
blank = count 3 space $> Nothing

crate :: Parser (Maybe Char)
crate = between (char '[') (char ']') (Just <$> anyChar)

row :: Parser [Maybe Char]
row = sepBy1 (choice [blank, crate]) (char ' ')

rows :: Parser [[Maybe Char]]
rows = sepEndBy row newline

convertToStacks :: [[Maybe Char]] -> [[Char]]
convertToStacks = map catMaybes . transpose

type Quantity = Int

type From = Int

type To = Int

data Op = Op Quantity From To deriving (Show)

stackMove :: Parser Op
stackMove = Op <$> (string "move " *> digits) <*> (string " from " *> fmap toIndex digits) <*> (string " to " *> fmap toIndex digits)

performMove :: Sq.Seq [a] -> Op -> Sq.Seq [a]
performMove stacks (Op q from to) = Sq.adjust (top ++) to . Sq.update from rest $ stacks
  where
    (top, rest) = splitAt q (stacks `Sq.index` from)

data Input = Input (Sq.Seq [Char]) [Op] deriving (Show)

input :: Parser Input
input = Input <$> fmap (Sq.fromList . convertToStacks) rows <*> sepBy1Try stackMove newline

day5 :: IO ()
day5 = getContents >>= printEither . fmap (concatMap headOrEmpty . toList . \(Input stacks ops) -> foldl performMove stacks ops) . parse input ""
  where
    headOrEmpty [] = "*"
    headOrEmpty (x : _) = [x]

-- This is not relevant to solving any of the problems, it just handles IO and selecting which problem to solve.
days :: [IO ()]
days = [day1, day2, day3, mempty, day5]

toIndex :: Int -> Int
toIndex x = x - 1

printEither :: (Show a, Show b) => Either a b -> IO ()
printEither (Left a) = print a
printEither (Right b) = print b

main :: IO ()
main = getArgs >>= ((days !!) . toIndex . read . head)
