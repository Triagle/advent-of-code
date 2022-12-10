module Utils (window, difference, countAll, showEither) where
import Data.List

window :: Int -> [a] -> [[a]]
window n = map (take n) . tails

difference :: Num a => [a] -> [a]
difference l = zipWith (-) (tail l) l

countAll :: (a -> Bool) -> [a] -> Int
countAll p = length . filter p

showEither :: (Show a, Show b) => Either a b -> String
showEither = either show show
