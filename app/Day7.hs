module Day7 (day7) where
import Utils
import Parse
import Text.Parsec
import Data.Tree
import Control.Monad
import Data.Foldable
import Text.Parsec.String


cd :: Parser ()
cd = string "$ cd " >> notFollowedBy (string "..") >> many (noneOf "\n") >> newlineOrEof

dir :: Parser Integer
dir = string "dir " >> many (noneOf "\n") >> newlineOrEof >> pure 0

file :: Parser Integer
file = digits <* (many (noneOf "\n") >> newlineOrEof)

ls :: Parser ()
ls = void $ string "$ ls" >> newlineOrEof

backtrack :: Parser ()
backtrack = string "$ cd .." >> newlineOrEof

directory :: Parser (Tree Integer)
directory = do
  cd
  ls
  fileSize <- sum <$> many (choice [file, dir])
  children <- many (try directory)
  backtrack <|> eof
  return $ Node (fileSize + (sum . map rootLabel) children) children

dirToDelete :: Tree Integer -> Integer
dirToDelete tree = minimum . filter (>= requiredSpace) . toList $ tree
  where total = rootLabel tree
        requiredSpace = 30000000 - (70000000 - total)

day7 :: IO ()
day7 = interact (showEither . fmap dirToDelete . parse directory "")
