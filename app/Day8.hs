module Day8 (day8) where

import Text.Parsec
import Data.List
import Data.Bifunctor
import qualified Data.Set as S
import Utils
import Parse
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String


norm :: (Integer, Integer) -> Integer
norm (x1, x2) = max (abs x1) (abs x2)

sub :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
(x1, y1) `sub` (x2, y2) = (x1 - x2, y1 - y2)

add :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
(x1, y1) `add` (x2, y2) = (x1 + x2, y1 + y2)

direction :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
direction x y = bimap signum signum $ y `sub` x

distance :: (Integer, Integer) -> (Integer, Integer) -> Integer
distance x y = norm $ x `sub` y

chase :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
x `chase` y = x `add` dir
  where
    dir = if distance x y <= 1 then (0, 0) else direction x y

data Move = U | L | D | R deriving (Show, Eq, Enum)

up :: Parser [Move]
up = string "U " *> fmap (`replicate` U) digits

down :: Parser [Move]
down = string "D " *> fmap (`replicate` D) digits

left :: Parser [Move]
left = string "L " *> fmap (`replicate` L) digits

right :: Parser [Move]
right = string "R " *> fmap (`replicate` R) digits

moves :: Parser [Move]
moves = concat <$> sepEndBy (choice [up, down, left, right]) newline

chaseRope :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
chaseRope rope head = take 9 $ scanr chase head rope

processMoves :: [Move] -> [[(Integer, Integer)]]
processMoves = scanl chaseRope [(0, 0)] . tail . scanl go (0, 0)
  where
    go (x, y) U = (x, y + 1)
    go (x, y) D = (x, y - 1)
    go (x, y) L = (x - 1, y)
    go (x, y) R = (x + 1, y)


day8 :: IO ()
day8 = interact (showEither . fmap (length . S.fromList . map head . processMoves) . parse moves "")
