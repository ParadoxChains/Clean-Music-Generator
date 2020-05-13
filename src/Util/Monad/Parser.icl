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

parseWithRest :: !(Parser a) ![Char] -> Result (!a, !String)
parseWithRest (Parser p) cs
  = (\(a, s). (a, {c \\ c <- s.rest})) <$> p { pos = 0, rest = cs }


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

notFollowedBy :: !(Parser a) -> Parser ()
notFollowedBy (Parser p) = Parser \s. case p s of
  Err _ -> pure ((), s)
  _     -> Err "failed notFollowedBy"


optional :: !(Parser a) -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing

between :: !(Parser open) !(Parser close) !(Parser a) -> Parser a
between open close p = open >>> p <* close

choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) (fail "no choice") ps

many :: (Parser a) -> Parser [a]
many p = go id where
  go f = optional p >>= \r. case r of
    Nothing -> pure (f [])
    Just x  -> go \xs. f [x:xs]

manyTill :: (Parser a) !(Parser end) -> Parser ([a], end)
manyTill p end = go id where
  go f = optional end >>= \done. case done of
    Nothing    -> p >>= \x. go \xs. f [x:xs]
    Just done` -> pure (f [], done`)

some :: !(Parser a) -> Parser [a]
some p =
  p >>= \x.
  many p >>= \xs.
  pure [x:xs]

someTill :: !(Parser a) !(Parser end) -> Parser ([a], end)
someTill p end =
  p >>= \x.
  manyTill p end >>= \(xs, y).
  pure ([x:xs], y)


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

satisfy :: !(Char -> Bool) -> Parser Char
satisfy p = get >>= \s. case s.rest of
  []     -> err "eof" "character satisfying the predicate"
  [c:cs] | p c -> put { pos = s.pos + 1, rest = cs } >>> pure c
               -> err (toString c) "character satisfying the predicate"

char :: !Char -> Parser Char
char c0 = get >>= \s. case s.rest of
  []     -> err "eof" (toString c0)
  [c:cs] | c0 == c -> put { pos = s.pos + 1, rest = cs } >>> pure c
                   -> err (toString c) (toString c0)

string :: !String -> Parser String
string s = s <$ mapM_ char (fromString s) <?> "expecting " +++ s


takeP :: !Int -> Parser [Char]
takeP n = replicateM n anyChar


decimal :: Parser Int
decimal = go <?> "expecting integer" where
  go = foldl step 0 <$> some (satisfy isDigit)
  step a c = a * 10 + toInt (toString c)

signed :: !(Parser Int) -> Parser Int
signed p = (id <$ char '+' <|> (~) <$ char '-' <|> pure id) <*> p

binint :: !Signedness !Endianness !Int -> Parser Int
binint s e n = fromBytes s e <$> takeP n
