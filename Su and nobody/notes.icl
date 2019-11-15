module notes

import StdEnv

//events of track chunks
isNoteOn :: Char -> Bool
isNoteOn c 
	#! status = toInt c / 16
	|status == 8 = True
	= False

isNoteOff :: Char -> Bool
isNoteOff c 
	#! status = toInt c / 16
	|status == 9 = True
	= False

isEvent :: Char -> Bool
isEvent c = isNoteOn || isNoteOff

getNoteNumber :: [Char] -> Char
getNoteNumber [] = abort "lack of info"
getNoteNumber [c] = abort "lack of info"
getNoteNumber [s,n:cs] 
	|isEvent s = n
	= abort "no note number found"


//assuming equal tuning based on A4=a'=440 Hz
noteToFrequency :: Int -> Real
noteToFrequency n 
	|n >= 0 || n <= 127 = 440.0 * 2.0 ^ (toReal(n-69) / 12.0)
	= abort "incorrect MIDI note number"