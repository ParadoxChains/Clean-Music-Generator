definition module Control.Monad.Parser

import StdMaybe
import Control.Monad

:: Parser a

instance Monad Parser

parse :: !(Parser a) ![Char] -> Maybe a

fail :: Parser a
(<|>) infixl 3 :: !(Parser a) (Parser a) -> Parser a

eof     :: Parser ()
anyChar :: Parser Char
char    :: !Char   -> Parser Char
string  :: !String -> Parser String

takeP :: !Int -> Parser [Char]
