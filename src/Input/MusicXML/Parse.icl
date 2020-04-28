implementation module Input.MusicXML.Parse

import StdEnv
import Util.TypeDefs
import Util.Monad.Parser

// eliminate useless whitespace or newline or tab
dropWhiteSpace :: Parser ()
dropWhiteSpace = ()<$ many (satisfy isSpace)

// jump useless information that same for each MusicXML file
skip :: Parser ()
//skip = ()<$ manyTill anyChar (char '>')
skip = manyTill anyChar (char '>')