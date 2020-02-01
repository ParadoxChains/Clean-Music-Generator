implementation module Input.Chunks

import StdEnv
import Util.Byte, Util.TypeDefs


//midi is consists of chunks there are two types of chunks there

//check if it is header chunk
isHeader :: [Char] -> Bool
isHeader l
	|length l < 4 = False 
	#! type = toString (take 4 l)
	|type == "MThd" = True
	= False
	
//check if it is track chunk
isTrack :: [Char] -> Bool
isTrack l
	|length l < 4 = False 
	#! type = toString (take 4 l)
	|type == "MTrk" = True
	= False

//calculate the length of current track chunk
//4 bytes for length information
trackChunkLen :: [Char] -> Int
trackChunkLen l = fromBytes Unsigned BE (take 4 l)

//read the format info in header
calcFormat :: [Char] -> Int
calcFormat l = fromBytes Unsigned BE l

//read the division info in header
calcDivision :: [Char] -> Int
calcDivision l = fromBytes Unsigned BE l

//byte to int function for calculating delta time
deltaByteToInt :: [Char] -> Int
deltaByteToInt [] = 0
deltaByteToInt [c:cs]
	#! len = length cs
	#! n = toInt c
	| n > 128 = (n-128)* 2 ^ (7*len) + deltaByteToInt cs
	= n
	 * 2 ^ (7*len) + deltaByteToInt cs

//filter bytes that contain delta time info
//read next byte only if the previous byte has 1 as its first digit 
deltaTimeList :: [Char] -> [Char]
deltaTimeList [] = []
deltaTimeList [c:cs]
	#! d = toInt c
	|d < 128 = [c]
	= [c:deltaTimeList cs]

//read delta time of an event
//fst:delta time
//snd: length of bytes that uses to store delta time info
deltaTime :: [Char] -> (Int,Int)
deltaTime l 
	#! deltaL = deltaTimeList l
	#! result = deltaByteToInt deltaL
	= (result, length deltaL)
//events of track chunks

firstHalfStatus :: Char -> Int
firstHalfStatus c = toInt c / 16

secondHalfStatus :: Char -> Int
secondHalfStatus c = toInt c rem 16

//Note on event
isNoteOn :: Char -> Bool
isNoteOn c = firstHalfStatus c == 9

//Note off event
isNoteOff :: Char -> Bool
isNoteOff c = firstHalfStatus c == 8

//return channel information
getChannel :: Char -> Int
getChannel c = secondHalfStatus c

//return frequency information which comes from note number
getFrequency :: Char -> Frequency
getFrequency c 
	#! n = toInt c
	|n >= 0 || n <= 127 = 440.0 * 2.0 ^ (toReal(n-69) / 12.0)
	= abort "incorrect MIDI note number"

getVelocity :: Char -> Int
getVelocity c = toInt c
	
	
	
	
