module Day14 where

import Data.List (find, intercalate, maximumBy)
import Data.Maybe (fromMaybe)
import Utils
import Parse
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Text.Parsec
import Debug.Trace
import Text.Parsec.String
import Text.Parsec.Char

type Point = (Int, Int)

type Path = [Point]

coordinate :: Parser Point
coordinate = (,) <$> digits <*> (char ',' *> digits)

path :: Parser Path
path = concat . segments <$> sepBy1 coordinate sep
  where sep = string " -> "
        segments p = zipWith segment p (tail p)
        segment (x1, y1) (x2, y2)
          | x1 == x2 = [(x1, yi) | yi <- [min y1 y2..max y1 y2]]
          | otherwise = [(xi, y1) | xi <- [min x1 x2..max x1 x2]]
        

-- Sparse grid better for performance here I think
data Grid = Grid {
  state :: M.Map Point (),
  maxY :: Int,
  occupied :: M.Map Int Int -- lowest occupied position 
  } deriving (Show, Eq)


createGrid :: [Path] -> M.Map Point ()
createGrid = mconcat . map gridPath
  where gridPath  = M.fromList . map (\x -> (x, ()))
        
createOccupied :: [Path] -> M.Map Int Int
createOccupied = foldr (\(x, y) m -> M.insertWith max x y m) mempty . concat
  

grid :: Parser Grid
grid = do
  paths <- sepEndBy path newline
  return Grid {
    state = createGrid paths,
    maxY = maximum . map snd $ concat paths,
    occupied = createOccupied paths
              } 

free :: Grid -> Point -> Bool
free grid (x, y) = maybe True (y >) $ M.lookup x (occupied grid)


pOccupied :: Grid -> Point -> Bool
pOccupied grid point@(_, y) =  y >= maxY grid + 2 || point `M.member` state grid

move :: Grid -> Point -> Point
move grid p@(x, y) = fromMaybe p $ find (not . pOccupied grid) [(x + dx, y + 1) | dx <- [0, -1, 1]]


restingPlace :: Grid -> Point -> Point
restingPlace grid point@(x, y)
  | next == point = point
  | otherwise = restingPlace grid next
  where next = move grid point

startingLocation :: Point
startingLocation = (500, 0)

addSand :: Grid -> Grid
addSand grid = grid { state = M.insert p () (state grid) }
  where p = restingPlace grid startingLocation

numStates :: Grid -> Int
numStates grid = numStates' grid 0
  where numStates' grid i
          | pOccupied grid startingLocation = i
          | otherwise = numStates' next (i + 1)
          where next = addSand grid

render :: Point -> Point -> Grid -> Grid -> String
render (x1, y1) (x2, y2) start grid = unlines $ map (\y -> map (\x -> if pOccupied start (x, y) then '#' else if pOccupied grid (x, y) then 'o' else '.') [min x1 x2..max x1 x2]) [min y1 y2 .. max y1 y2]


day14 :: IO ()
day14 = interact (either show (show . numStates) . parse grid "")
  
  

