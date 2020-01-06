implementation module Util.Monad.Parser

import StdEnv
import Util.Monad
import Util.Monad.Result
import Util.Byte

:: State =
  { pos  :: !Int
  , rest :: ![Char]
  }

:: Parser a = Parser (State -> Result (!a, !State))

runParser :: !(Parser a) !State -> Result (!a, !State)
runParser (Parser p) s = p s

instance Monad Parser where
  pure a = Parser \s. pure (a, s)
  (>>=) (Parser p) f = Parser \s0.
    p s0 >>= \(a, s1).
    runParser (f a) s1

parse :: !(Parser a) ![Char] -> Result a
parse (Parser p) cs = fst <$> p { pos = 0, rest = cs }

fail :: !String -> Parser a
fail e = Parser \s. Err (toString s.pos +++ ": " +++ e)

(<|>) infixl 3 :: !(Parser a) (Parser a) -> Parser a
(<|>) (Parser p) (Parser q) = Parser \s. case p s of
  Err e0 -> case q s of
    Err e1 -> Err (e0 +++ "\n" +++ e1)
    r      -> r
  r      -> r

(<?>) infix 0 :: !(Parser a) String -> Parser a
(<?>) (Parser p) e = Parser \s. case p s of
  Err _ -> Err (toString s.pos +++ ": " +++ e)
  r     -> r

optional :: !(Parser a) -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing

err :: !String !String -> Parser a
err u e = fail ("unexpected " +++ u +++ ", expecting " +++ e)

get :: Parser State
get = Parser \s. pure (s, s)

put :: State -> Parser ()
put s = Parser \_. pure ((), s)

eof :: Parser ()
eof = get >>= \s. case s.rest of
  []    -> pure ()
  [c:_] -> err (toString c) "eof"

anyChar :: Parser Char
anyChar = get >>= \s. case s.rest of
  []     -> err "eof" "any char"
  [c:cs] -> put { pos = s.pos + 1, rest = cs } >>> pure c

char :: !Char -> Parser Char
char c0 = get >>= \s. case s.rest of
  []     -> err "eof" (toString c0)
  [c:cs] | c0 == c -> put { pos = s.pos + 1, rest = cs } >>> pure c
                   -> err (toString c) (toString c0)

string :: !String -> Parser String
string s = s <$ mapM_ char (fromString s) <?> "expecting " +++ s

takeP :: !Int -> Parser [Char]
takeP n = replicateM n anyChar

uintBE :: !Int -> Parser Int
uintBE n = bytesToUintBE <$> takeP n

uintLE :: !Int -> Parser Int
uintLE n = bytesToUintLE <$> takeP n

intBE :: !Int -> Parser Int
intBE n = bytesToIntBE <$> takeP n

intLE :: !Int -> Parser Int
intLE n = bytesToIntLE <$> takeP n
