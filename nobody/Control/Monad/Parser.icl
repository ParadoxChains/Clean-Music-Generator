implementation module Control.Monad.Parser

import StdEnv
import StdMaybe
import Control.Monad

:: Parser a = Parser ([Char] -> Maybe (!a, ![Char]))

runParser :: !(Parser a) ![Char] -> Maybe (!a, ![Char])
runParser (Parser p) s = p s

instance Monad Parser where
  pure a = Parser \s. pure (a, s)
  (>>=) (Parser p) f = Parser \s0.
    p s0 >>= \(a, s1).
    runParser (f a) s1

parse :: !(Parser a) ![Char] -> Maybe a
parse (Parser p) s = fst <$> p s

fail :: Parser a
fail = Parser \_. Nothing

(<|>) infixl 3 :: !(Parser a) (Parser a) -> Parser a
(<|>) (Parser p) (Parser q) = Parser \s0. case p s0 of
  Nothing -> q s0
  m       -> m

eof :: Parser ()
eof = Parser \s. if (isEmpty s) (Just ((), s)) Nothing

anyChar :: Parser Char
anyChar = Parser \s. case s of
  []     -> Nothing
  [c:cs] -> Just (c, cs)

char :: !Char -> Parser Char
char c0 = Parser \s. case s of
  [c:cs] | c0 == c -> Just (c, cs)
  _ -> Nothing

string :: !String -> Parser String
string s = s <$ mapM_ char (fromString s)

takeP :: !Int -> Parser [Char]
takeP n = replicateM n anyChar
