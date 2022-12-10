module Parse where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Control.Monad


newlineOrEof :: Parser ()
newlineOrEof = void newline <|> eof

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
