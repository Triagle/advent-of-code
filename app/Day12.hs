module Day12 where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Data.Char (ord)
import Data.Bifunctor 
import Data.List (elemIndex, find)
import Data.Tuple (swap)
import Data.Array
import Text.Parsec
import Utils
import Text.Parsec.Char
import Text.Parsec.String
import Data.Graph.Inductive.PatriciaTree
import Data.Functor (($>))


data Tile = Start | End | Point Int deriving (Show, Eq)

elevation :: Tile -> Int
elevation Start = 0
elevation End = 25
elevation (Point x) = x

charToElevation :: Char -> Int
charToElevation c = ord c - ord 'a'

tile :: Parser Tile
tile = choice [char 'S' $> Start, char 'E' $> End, Point . charToElevation <$> alphaNum]

row :: Parser [Tile]
row = many1 tile

type Grid = Array (Int, Int) Tile

grid :: Parser Grid
grid = listTo2DArray <$> sepEndBy row newline 






listTo2DArray :: [[a]] -> Array (Int, Int) a
listTo2DArray xss =
  array ((0, 0), (length xss - 1, length (head xss) - 1)) $
    concatMap (\(i, xs) -> zip [(i, j) | j <- [0..]] xs) (zip [0..] xss) 

nodeToCoordinate :: Grid -> Int -> (Int, Int)
nodeToCoordinate grid i = i `quotRem` r
  where r = (1 +) . snd . snd . bounds $ grid

coordinateToNode :: Grid -> (Int, Int) -> Int
coordinateToNode grid (x, y) = x * r + y
  where r = (1 +) . snd . snd . bounds $ grid
  

neighbours :: Grid -> Int -> [LEdge ()]
neighbours grid n = map (\c -> (n, coordinateToNode grid c, ())) $ filter (\(u, v) -> lx <= u && u <= ux && ly <= v && v <= uy && near (x, y) (u, v)) neighbourCoords
  where
    ((lx, ly), (ux, uy)) = bounds grid
    (x, y) = nodeToCoordinate grid n
    neighbourCoords = [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]
    near (x, y) (u, v) = elevation (grid ! (x, y)) - elevation (grid ! (u, v))  <= 1

graph :: Grid -> Gr Tile ()
graph positions = mkGraph nodes edges
  where 
        nodes = zip [0..] (elems positions)
        edges = concatMap (neighbours positions) [0..length nodes - 1]

findStart :: Grid -> Maybe Int
findStart =  elemIndex Start . elems

findEnd :: Grid -> Maybe Int
findEnd =  elemIndex End . elems
        
findLowElevationPath :: Grid -> Int -> Maybe (LPath ())
findLowElevationPath grid j =  find ((== 0) . elevation . (grid !) . nodeToCoordinate grid . fst . head . unLPath) $ lbft j (graph grid)
        
day12 :: IO ()
day12 = interact (\x -> showEither $ do
                     gr <- first show $  parse grid "" x
                     j <- maybe (Left "Could not find end") Right (findEnd gr)
                     p <- maybe (Left "Could not find path") Right $ findLowElevationPath gr j
                     pure (length (unLPath p) - 1))
