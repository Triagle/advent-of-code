module Utils (window, difference, countAll, showEither, chunksOf) where
import Data.List

window :: Int -> [a] -> [[a]]
window n = map (take n) . tails

difference :: Num a => [a] -> [a]
difference l = zipWith (-) (tail l) l


countAll :: (a -> Bool) -> [a] -> Int
countAll p = length . filter p

showEither :: (Show a, Show b) => Either a b -> String
showEither = either show show

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  -- split the list into groups of n elements
  let (chunk, rest) = splitAt n xs
  -- if the list is not empty, recursively split the rest of the list
  -- and add the current group to the result
  in if not (null xs)
     then chunk : chunksOf n rest
     -- if the list is empty, return an empty list
     else []
