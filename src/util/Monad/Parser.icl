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

fail :: !String -> Parser a
fail e = Parser \_. Err e

(<?>) infix 0 :: !(Parser a) String -> Parser a
(<?>) (Parser p) e = Parser \s. case p s of
  Err _ -> Err e
  r     -> r

(<|>) infixl 3 :: !(Parser a) (Parser a) -> Parser a
(<|>) (Parser p) (Parser q) = Parser \s. case p s of
  Err e0 -> case q s of
    Err e1 -> Err (e0 +++ "\n" +++ e1)
    r      -> r
  r     -> r

eof :: Parser ()
eof = Parser \s. case s of
  []     -> Ok ((), s)
  [c:cs] -> err (toString c) "eof"

anyChar :: Parser Char
anyChar = Parser \s. case s of
  []     -> err "eof" "char"
  [c:cs] -> Ok (c, cs)

char :: !Char -> Parser Char
char c0 = Parser \s. case s of
  [] -> err "eof" (toString c0)
  [c:cs]
    | c0 == c -> Ok (c, cs)
    -> err (toString c) (toString c0)

string :: !String -> Parser String
string s = s <$ mapM_ char (fromString s)

takeP :: !Int -> Parser [Char]
takeP n = replicateM n anyChar

uintBE :: !Int -> Parser Int
uintBE n = bytesToUintBE <$> takeP n

uintLE :: !Int -> Parser Int
uintLE n = bytesToUintLE <$> takeP n

err :: !String !String -> Result a
err u e = Err ("Unexpected " +++ u +++ ", expected " +++ e)
