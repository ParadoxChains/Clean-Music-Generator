definition module Util.Monad.Parser

import StdMaybe
import Util.Byte
import Util.Monad
import Util.Monad.Result

:: Parser a

instance Monad Parser

// Runs the parser to get the result.
parse :: !(Parser a) ![Char] -> Result a

// Runs the parser to get the result and the remaining character.
// Use this only for debugging.
parseWithRest :: !(Parser a) ![Char] -> Result (!a, !String)


// Stop parsing and report an error.
fail :: !String -> Parser a

// The parser p <|> q first applies p.
// If it succeeds, the value of p is returned.
// If p fails, parser q is tried.
(<|>) infixl 3 :: !(Parser a) (Parser a) -> Parser a

// The parser p <?> str behaves as parser p,
// but whenever the parser p fails, it replaces the error with str.
(<?>) infix 0 :: !(Parser a) String -> Parser a

// If p in lookAhead p succeeds (either consuming input or not)
// the whole parser behaves like p succeeded without consuming anything
// (parser state is not updated as well).
// If p fails, lookAhead has no effect,
// i.e. it will fail consuming input if p fails consuming input.
lookAhead :: !(Parser a) -> Parser a

// notFollowedBy p only succeeds when the parser p fails.
notFollowedBy :: !(Parser a) -> Parser ()


// optional p tries to apply the parser p.
// It will parse p or Nothing.
// It only fails if p fails after consuming input.
// On success result of p is returned inside of Just,
// on failure Nothing is returned.
optional :: !(Parser a) -> Parser (Maybe a)

// between open close p parses open, followed by p and close.
// Returns the value returned by p.
between :: !(Parser open) !(Parser close) !(Parser a) -> Parser a

// choice ps tries to apply the parsers in the list ps in order,
// until one of them succeeds.
// Returns the value of the succeeding parser.
choice :: [Parser a] -> Parser a

// many p applies the parser p zero or more times
// and returns a list of the values returned by p.
many :: (Parser a) -> Parser [a]

// manyTill p end applies parser p zero or more times
// until parser end succeeds.
// Returns the list of values returned by p and the end result.
manyTill :: (Parser a) !(Parser end) -> Parser ([a], end)

// some p applies the parser p one or more times
// and returns a list of the values returned by p.
some :: !(Parser a) -> Parser [a]

// someTill p end works similarly to manyTill p end,
// but p should succeed at least once.
someTill :: !(Parser a) !(Parser end) -> Parser ([a], end)


// This parser only succeeds at the end of input.
eof :: Parser ()

// Parse and return a single character.
anyChar :: Parser Char

// The parser satisfy f succeeds for any character
// for which the supplied function f returns True.
satisfy :: !(Char -> Bool) -> Parser Char

// char c only matches the single specific character c.
char :: !Char -> Parser Char

// string str only matches the string str.
string :: !String -> Parser String


// takeP n parses n characters.
takeP :: !Int -> Parser [Char]


// Parse an integer in decimal representation.
decimal :: Parser Int

// signed space p parser parses an optional sign character (“+” or “-”),
// then it runs parser p which should return a number.
// Sign of the number is changed
// according to the previously parsed sign character.
signed :: !(Parser Int) -> Parser Int

// binint s e b parses an integer from binary data
// with s signedness, e endianness and b bytes.
binint :: !Signedness !Endianness !Int -> Parser Int
