implementation module Input.MusicXML.Parse

import StdEnv
import Util.Monad
import Util.TypeDefs
import Util.Monad.Parser

// eliminate useless whitespace or newline or tab
dropWhiteSpace :: Parser ()
dropWhiteSpace = ()<$ many (satisfy isSpace)

// skips everything until it reaches a '>'
skip :: Parser ()
skip = ()<$ manyTill anyChar (char '>')

// jump useless information that same for each MusicXML file
skipHeader :: Parser ()
skipHeader = skip >>> skip

// matching the correct characters of tag name -- English letters or dash
parseTagName :: Parser [Char]
parseTagName = some (satisfy (\x = (isAlpha x) ||  (x == '-')))

parseBeginTag :: Parser [Char]
parseBeginTag = char '<' >>> parseTagName >>= \x = char '>' >>> pure x

parseEndTag :: Parser [Char]
parseEndTag = char '<' >>> char '/' >>> parseTagName >>= \x = char '>' >>> pure x

