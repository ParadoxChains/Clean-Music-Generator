definition module Input.MusicXML.Parse

import Util.TypeDefs
import Util.Monad.Parser

:: Step :== Char
:: Line :== Int
:: Fifths :== Int
:: Octave :== Int
:: Divisions :== Int

:: Note_type = Whole | Quarter | Other_Note
:: Mode = Major | Minor | Other_Mode

// Text String: information of a tag; Element String: what kind of element is this
:: XML = Text String | Element String [XML] 

:: Key = 
	{
		fifths :: Fifths,
		mode :: Mode
	}

:: Pitch = 
	{
		step :: Step,
		octave :: Octave
	}
	
:: Attributes = 
	{
		divisions :: Divisions,
		key :: Key,
		time :: TimeSignature
	}
		
:: Note = 
	{
		pitch :: Pitch,
		duration :: Duration,
		type :: Note_type
	}
	
:: Measure = 
	{
		attributes :: [Attributes],
		notes :: [Note]
	}
	
:: MusicXML :== [Measure]

dropWhiteSpace :: Parser ()

skip :: Parser ()

skipHeader :: Parser ()

parseTagName :: Parser String

parseBeginTag :: Parser String

parseEndTag :: Parser String

parseSelfClosingTag :: Parser XML

parseInfo :: Parser XML

parseElement :: Parser XML

getNote :: XML -> Note

getNotes :: [XML] -> [Note]	

getAttribute :: XML -> Attributes

getAttributes :: [XML] -> [Attributes]

getMeasure :: XML -> Measure

getMeasures :: [XML] -> [Measure]

getPart :: XML -> [Measure]

getParts :: [XML] -> [Measure]

getRoot :: XML -> [Measure]

parseXML :: Parser XML

parseFile :: Parser XML
