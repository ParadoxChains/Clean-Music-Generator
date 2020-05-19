implementation module Input.MusicXML.Parse

import StdEnv
import Util.Monad
import Util.TypeDefs
import Util.Monad.Parser

// eliminating useless whitespace or newline or tab
dropWhiteSpace :: Parser ()
dropWhiteSpace = ()<$ many (satisfy isSpace)

// skipping everything until it reaches a '>'
skip :: Parser ()
skip = ()<$ manyTill anyChar (char '>')

// jumpimg useless information that same for each MusicXML file
skipHeader :: Parser ()
skipHeader = skip >>> skip

// matching the correct characters of tag name -- English letters or dash
parseTagName :: Parser String
parseTagName = some (satisfy (\x = (isAlpha x) ||  (x == '-'))) >>= \x = pure (toString x)

parseBeginTag :: Parser String
parseBeginTag = char '<' >>> 
	parseTagName >>= \x = 
	skip  >>> 
	pure x

parseEndTag :: Parser String
parseEndTag = char '<' >>> char '/' >>> parseTagName >>= \x = char '>' >>> pure x

// parsing information between open tag and close tag
parseInfo :: Parser XML
parseInfo = some (satisfy (\x = (isAlpha x) || (isDigit x) 
					|| (isSpace x) || (x == '-'))) >>= \x = pure (Text (toString x))

// parsing one or more elements recursively
parseElement :: Parser XML
parseElement = 
	parseBeginTag >>= \x = 
	dropWhiteSpace >>>
	many parseXML >>= \y = 
	parseEndTag >>> 
	dropWhiteSpace >>>
	pure (Element x y)
					
parseXML :: Parser XML
parseXML = parseElement <|> parseInfo

parseFile :: Parser XML
parseFile = skipHeader >>> dropWhiteSpace >>> parseXML

//test :: Parser String
//test = skipHeader >>> dropWhiteSpace >>> parseFile

