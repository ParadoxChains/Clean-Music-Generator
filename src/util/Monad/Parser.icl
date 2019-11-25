implementation module util.Monad.Parser

import StdEnv
import util.Monad
import util.Monad.Result
import util.Byte

:: Parser a = Parser ([Char] -> Result (!a, ![Char]))

runParser :: !(Parser a) ![Char] -> Result (!a, ![Char])
runParser (Parser p) s = p s

instance Monad Parser where
  pure a = Parser \s. pure (a, s)
  (>>=) (Parser p) f = Parser \s0.
    p s0 >>= \(a, s1).
    runParser (f a) s1

parse :: !(Parser a) ![Char] -> Result a
parse (Parser p) s = fst <$> p s

fail :: String -> Parser a
fail s = Parser \_. Err s

(<|>) infixl 3 :: !(Parser a) (Parser a) -> Parser a
(<|>) (Parser p) (Parser q) = Parser \s0. case p s0 of
  Err _ -> q s0
  m     -> m

eof :: Parser ()
eof = Parser \s. if (isEmpty s) (Ok ((), s)) (Err "Expected eof")

anyChar :: Parser Char
anyChar = Parser \s. case s of
  []     -> Err "Unexpected eof"
  [c:cs] -> Ok (c, cs)

char :: !Char -> Parser Char
char c0 = Parser \s. case s of
  [c:cs] | c0 == c -> Ok (c, cs)
  _ -> Err ("Expected " +++ toString c0)

string :: !String -> Parser String
string s = s <$ mapM_ char (fromString s)

takeP :: !Int -> Parser [Char]
takeP n = replicateM n anyChar

uintBE :: !Int -> Parser Int
uintBE n = bytesToUintBE <$> takeP n

uintLE :: !Int -> Parser Int
uintLE n = bytesToUintLE <$> takeP n
