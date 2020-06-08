implementation module Input.MusicXML.Parse

import StdEnv
import Util.Monad
import Util.TypeDefs
import Util.Monad.Parser

// eliminating useless whitespace or newline or tab
dropWhiteSpace :: Parser ()
dropWhiteSpace = ()<$ (many (()<$(satisfy isSpace) <|> skipComment)) 

// skipping everything until it reaches a '>'
skip :: Parser ()
skip = ()<$ manyTill anyChar (char '>')

// jumpimg useless information that same for each MusicXML file
skipHeader :: Parser ()
skipHeader = skip >>> skip

skipComment :: Parser ()
skipComment = ()<$ (string "<!-" >>>  manyTill anyChar (string "-->"))

// matching the correct characters of tag name -- English letters or dash
parseTagName :: Parser String
parseTagName = some (satisfy (\x = (isAlpha x) ||  (x == '-'))) >>= \x = pure (toString x)

//parseBeginTag :: Parser (String,ElementAttribute)
/*
parseBeginTag = 
	char '<' >>> 
	parseTagName >>= \x = 
	many (char ' ' >>> parseAttribute) >>= \y =
	char '>' >>>
	pure (x,y)
*/
parseBeginTag :: Parser String
parseBeginTag = 
	char '<' >>> 
	parseTagName >>= \x = 
	skip  >>> 
	pure x

parseEndTag :: Parser String
parseEndTag = between (string "</") (char '>') parseTagName

parseSelfClosingTag :: Parser XML
parseSelfClosingTag = 
	char '<' >>> 
	parseTagName >>= \x = 
	manyTill (satisfy \x = x <> '>') (char '/') >>>
	char '>' >>>
	dropWhiteSpace >>> 
	pure (Element x [])

parseAttribute :: Parser ElementAttribute
parseAttribute = 
	parseTagName >>= \x = 
	char '=' >>>
	char '"' >>>
	manyTill anyChar (char '"') >>= \z =
	pure({name = x, value = toString(fst z)})
	
// parsing information between open tag and close tag
parseInfo :: Parser XML
parseInfo = some (satisfy \x = x <> '<') >>= \x = pure (Text (toString x))

// parsing one or more elements recursively
parseElement :: Parser XML
parseElement = 
	parseBeginTag >>= \x = 
	dropWhiteSpace >>>
	many parseXML >>= \y = 
	parseEndTag >>= \z = 
	case x == z of 
		True -> dropWhiteSpace >>> pure (Element x y)
		False -> fail ("No matching close tag for " +++ x)

parseXML :: Parser XML
parseXML = parseSelfClosingTag <|> parseElement <|> parseInfo

parseFile :: Parser XML
parseFile = skipHeader >>> dropWhiteSpace >>> parseXML

getNote :: XML -> Note
getNote (Element "note" x) 
		= {
	   		pitch = case (getElements "pitch" x) of
	   				[(Element "pitch" l)]
	   				 	 -> {step = case (getElement "step" l) of
	   				 	 			(Element "step" [(Text t1)]) -> t1.[0],
	   				 	 	 alter = case (getElements "alter" l) of
	   				 	 	 		[(Element "alter" [(Text t2)])] -> toInt(t2)
	   				 	 	 		[] -> 1,
	   				 	 	 octave = case (getElement "octave" l) of
	   				 	 	 		(Element "octave" [(Text t3)]) -> toInt(t3)}
	   				[] -> {step = 'C', alter = 1, octave = 4},
	   		duration = case (getElement "duration" x) of 
	   				(Element "duration" [(Text t3)]) -> toInt(t3),
	   		type = case (getElement "type" x) of 
	   				(Element "type" [(Text t4)]) 
	   					->	case t4 of 
	   						"whole" -> Whole
	   						"quarter" -> Quarter
	   						_ -> Other_Note
	     }

getNotes :: [XML] -> [Note]	
getNotes l = map getNote (getElements "note" l)

getAttribute :: XML -> Attributes
getAttribute (Element "attributes" x) 
			 = {
				divisions = case (getElements "divisions" x) of
							[(Element "divisions" [(Text t1)])] -> toInt(t1)
							[] -> 1,
				key = case (getElement "key" x) of 
					  (Element "key" [(Element "fifths" [(Text t2)]):x]) -> 
						{fifths = toInt(t2), 
					   	 mode = case x of
					   			 [(Element "mode" [(Text "Major")]):_] -> Major
					   			 [(Element "mode" [(Text "Minor")]):_] -> Minor
					   			 [(Element "mode" [(Text _)]):_] -> Other_Mode
					   			 _ -> Major},
				time = case (getElements "time" x) of
						[(Element "time" 
						 [(Element "beats" [(Text t3)]),
				 	 	  (Element "beat-type" [(Text t4)]):_])] -> 
				 	 	 	{barVal = toInt(t3), 
							 noteVal = toInt(t4)}
						[] -> 
				 	 	 	{barVal = 4, 
							 noteVal = 4}
			  	} 
getAttribute (Element name l) = abort name

getAttributes :: [XML] -> [Attributes]	
getAttributes l = map getAttribute (getElements "attributes" l)
			
getMeasure :: XML -> Measure
getMeasure (Element "measure" l) = 
	{attributes = getAttributes(getElements "attributes" l),
	 notes = getNotes(getElements "note" l)}
	 
getMeasures :: [XML] -> [Measure]
getMeasures l = map getMeasure (getElements "measure" l)

getElement :: String [XML] -> XML
getElement s l 
	# [x:xs] = getElements s l
	= x
	
getElements :: String [XML] -> [XML]
getElements s xs = [(Element name l)\\(Element name l)<-xs | name == s]

getPart :: XML -> [Measure]
getPart (Element "part" l) = getMeasures l

getParts :: [XML] -> [Measure]
getParts l = flatten(map getPart (getElements "part" l))

getRoot :: XML -> [Measure]
getRoot (Element "score-partwise" l) = getParts l




