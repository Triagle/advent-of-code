module Day5 (day5) where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.List (transpose)
import Utils
import Parse
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Functor (($>))
import qualified Data.Sequence as Sq
import qualified Data.Set as S

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
  where toIndex x = x - 1

performMove :: Sq.Seq [a] -> Op -> Sq.Seq [a]
performMove stacks (Op q from to) = Sq.adjust (top ++) to . Sq.update from rest $ stacks
  where
    (top, rest) = splitAt q (stacks `Sq.index` from)

data Input = Input (Sq.Seq [Char]) [Op] deriving (Show)

input :: Parser Input
input = Input <$> fmap (Sq.fromList . convertToStacks) rows <*> sepBy1Try stackMove newline

day5 :: IO ()
day5 = interact (showEither . fmap (concatMap headOrEmpty . toList . \(Input stacks ops) -> foldl performMove stacks ops) . parse input "")
  where
    headOrEmpty [] = "*"
    headOrEmpty (x : _) = [x]
