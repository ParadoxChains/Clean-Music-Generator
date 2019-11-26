implementation module Util.Monad.Parser

import StdEnv
import Util.Monad
import Util.Monad.Result
import Util.Byte

:: Parser a = Parser ((!Int, ![Char]) -> Result (!a, (!Int, ![Char])))

runParser :: !(Parser a) (!Int, ![Char]) -> Result (!a, (!Int, ![Char]))
runParser (Parser p) s = p s

instance Monad Parser where
  pure a = Parser \s. pure (a, s)
  (>>=) (Parser p) f = Parser \s0.
    p s0 >>= \(a, s1).
    runParser (f a) s1

parse :: !(Parser a) ![Char] -> Result a
parse (Parser p) s = fst <$> p (0, s)

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
eof = Parser \(i, s). case s of
  []     -> Ok ((), (i, s))
  [c:cs] -> err i (toString c) "eof"

anyChar :: Parser Char
anyChar = Parser \(i, s). case s of
  []     -> err i "eof" "char"
  [c:cs] -> Ok (c, (i + 1, cs))

char :: !Char -> Parser Char
char c0 = Parser \(i, s). case s of
  [] -> err i "eof" (toString c0)
  [c:cs]
    | c0 == c -> Ok (c, (i + 1, cs))
    -> err i (toString c) (toString c0)

string :: !String -> Parser String
string s = s <$ mapM_ char (fromString s) <?> "Expected " +++ s

takeP :: !Int -> Parser [Char]
takeP n = replicateM n anyChar

uintBE :: !Int -> Parser Int
uintBE n = bytesToUintBE <$> takeP n

uintLE :: !Int -> Parser Int
uintLE n = bytesToUintLE <$> takeP n

err :: !Int !String !String -> Result a
err i u e = Err
  (toString i +++ ": unexpected " +++ u +++ ", expected " +++ e)
