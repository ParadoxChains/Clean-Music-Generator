definition module Input.MusicXML.Parse

import Util.TypeDefs
import Util.Monad.Parser
:: Sign :== Char
:: Step :== Char

:: Line :== Int
:: Beats :== Int
:: Fifths :== Int
:: Octave :== Int
:: Divisions :== Int
:: Beat_type :== Int
//<score-partwise>: made up of parts, where each part is made up of measures. 
//<score-timewise>: made up of measures, where each measure is made up of parts. 
:: File_type = Partwise | Timewise
:: Note_type = Whole | Quarter | Other_note
:: Mode = Major | Minor | Other_mode
:: Measure_numbering = None | System | Other_mn

:: Key = 
	{
		fifths :: Fifths,
		mode :: Mode
	}
	
:: XMLTime = 
	{
		beats :: Beats,
		beat_type :: Beat_type
	}
	
:: Clef = 
	{
		sign :: Sign,
		line :: Line
	}
	
:: Print = 
	{
		measure_numbering :: Measure_numbering
	}
	
:: Attributes = 
	{
		divisions :: Divisions,
		key :: Key,
		xmltime :: XMLTime,
		clef :: Clef
	}
	
:: Pitch = 
	{
		step :: Step,
		octave :: Octave
	}
	
:: Note = 
	{
		pitch :: Pitch,
		duration :: Duration,
		note_type :: Note_type
	}
	
:: Measure = 
	{
		print :: Print,
		attributes :: [Attributes],
		notes :: [Note]
	}
	
:: MusicXML =
 	{
 		file_type :: File_type,
 		measures :: [Measure]
 	}	

dropWhiteSpace :: Parser ()

skip :: Parser ()

skipHeader :: Parser ()

parseTagName :: Parser [Char]

parseBeginTag :: Parser [Char]

parseEndTag :: Parser [Char]