definition module Util.Monad.Parser

import StdMaybe
import Util.Byte
import Util.Monad
import Util.Monad.Result

:: Parser a

instance Monad Parser

parse :: !(Parser a) ![Char] -> Result a

fail :: !String -> Parser a
(<|>) infixl 3 :: !(Parser a) (Parser a) -> Parser a
(<?>) infix  0 :: !(Parser a) String     -> Parser a

optional :: !(Parser a) -> Parser (Maybe a)

eof     :: Parser ()
anyChar :: Parser Char
char    :: !Char   -> Parser Char
string  :: !String -> Parser String

takeP :: !Int -> Parser [Char]

int :: !Signedness !Endianness !Int -> Parser Int
