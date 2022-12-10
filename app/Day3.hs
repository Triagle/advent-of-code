module Day3 (day3) where
import Data.Sequence as Sq
import Data.Char (ord)
import Data.Set as S

priority :: Char -> Int
priority c
  | c >= 'A' && c < 'a' = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

groupPriority :: Sq.Seq String -> Int
groupPriority = priority . head . S.toList . foldr1 S.intersection . fmap S.fromList

day3 :: IO ()
day3 = interact (show . sum . fmap groupPriority . Sq.chunksOf 3 . Sq.fromList . lines)
