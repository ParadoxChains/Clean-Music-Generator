implementation module chunks

import StdEnv

//midi is consists of chunks there are two types of chunks there

//check if it is header chunk
isHeader :: [Char] -> Bool
isHeader l
	|length l < 4 = False 
	#! type = toString (take 4 l)
	|type == "MThd" = True
	= False

//read the format info in header
calcFormat :: [Char] -> Int
calcFormat l = byteToInt l

//read the division info in header
calcDivision :: [Char] -> Int
calcDivision l = byteToInt l

//check if it is track chunk
isTrack :: [Char] -> Bool
isTrack l
	|length l < 4 = False 
	#! type = toString (take 4 l)
	|type == "MTrk" = True
	= False

deltaByteToInt :: [Char] -> Int
deltaByteToInt [] = 0
deltaByteToInt [c:cs]
	#! len = length cs
	= toInt c * 2 ^ (7*len) + deltaByteToInt cs

deltaTimeList :: [Char] -> [Char]
deltaTimeList [] = []
deltaTimeList [c:cs]
	#! d = toInt c
	|d < 128 = [c]
	= [c:deltaTimeList cs]

//read delta time of an event
deltaTime :: [Char] -> (Int,Int)
deltaTime l = 
	#! result = deltaByteToInt (deltaTimeList l)
	= (result, length result)

//events of track chunks

//Note on event
isNoteOn :: Char -> Bool
isNoteOn c 
	#! status = toInt c / 16
	|status == 8 = True
	= False

//Note off event
isNoteOff :: Char -> Bool
isNoteOff c 
	#! status = toInt c / 16
	|status == 9 = True
	= False

//return channel information
getChannel :: Char -> Int
getChannel c = toInt c mod 16

//return frequency information which comes from note number
getFrequency :: Char -> Int
getFrequency c 
	#! n = toInt c
	|n >= 0 || n <= 127 = 440.0 * 2.0 ^ (toReal(n-69) / 12.0)
	= abort "incorrect MIDI note number"