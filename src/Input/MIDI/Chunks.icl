implementation module Input.MIDI.Chunks

import StdEnv
import Util.Byte, Util.TypeDefs


/*
Name: isHeader
Args: a list of bytes
Output: boolean value
Info: check if current is a header chunk
*/
isHeader :: [Char] -> Bool
isHeader l
	|length l < 4 = False
	#! type = toString (take 4 l)
	|type == "MThd" = True
	= False

/*
Name: isTrack
Args: a list of bytes
Output: boolean value
Info: check if current is a track chunk
*/
isTrack :: [Char] -> Bool
isTrack l
	|length l < 4 = False
	#! type = toString (take 4 l)
	|type == "MTrk" = True
	= False

/*
Name: trackChunkLen
Args: a list of bytes
Output: the length of current track chunk
*/
trackChunkLen :: [Char] -> Int
trackChunkLen l = fromBytes Unsigned BE (take 4 l)

/*
Name: calcFormat
Args: a list of bytes
Output: the format of MIDI file
Info: possible result -- 0 1 2
*/
calcFormat :: [Char] -> Int
calcFormat l = fromBytes Unsigned BE l

/*
Name: calcDivision
Args: a list of bytes
Output: the default unit of delta-time for this MIDI file
Info: possible result -- 0 1
*/
calcDivision :: [Char] -> Int
calcDivision l = fromBytes Unsigned BE l

/*
Name: deltaByteToInt
Args: a list of bytes
Output: delta time vaule in integer form
Info: delta time --  the number of 'ticks' from the previous event
*/
deltaByteToInt :: [Char] -> Int
deltaByteToInt [] = 0
deltaByteToInt [c:cs]
	#! len = length cs
	#! n = toInt c
	| n > 128 = (n-128) * 2 ^ (7 * len) + deltaByteToInt cs
	= n * 2 ^ (7 * len) + deltaByteToInt cs

/*
Name: deltaTimeList
Args: a list of bytes
Output: a list of bytes that contain delta time info
Info: filter bytes that contain delta time info,
		read next byte only if the previous byte has 1 as its first digit
*/
deltaTimeList :: [Char] -> [Char]
deltaTimeList [] = []
deltaTimeList [c:cs]
	#! d = toInt c
	|d < 128 = [c]
	= [c:deltaTimeList cs]

/*
Name: deltaTime
Args: a list of bytes
Output: a delta time value and its length in bytes
*/
deltaTime :: [Char] -> (Int,Int)
deltaTime l
	#! deltaL = deltaTimeList l
	#! result = deltaByteToInt deltaL
	= (result, length deltaL)

/*
Name: firstHalfStatus
Args: one character
Output: the type of MIDI event
*/
firstHalfStatus :: Char -> Int
firstHalfStatus c = toInt c / 16

/*
Name: secondHalfStatus
Args: one character
Output: channel message
*/
secondHalfStatus :: Char -> Int
secondHalfStatus c = toInt c rem 16

/*
Name: isNoteOn
Args: one character
Output: boolean value
Info: check if current event is note on event
*/
isNoteOn :: Char -> Bool
isNoteOn c = firstHalfStatus c == 9

/*
Name: isNoteOff
Args: one character
Output: boolean value
Info: check if current event is note off event
*/
isNoteOff :: Char -> Bool
isNoteOff c = firstHalfStatus c == 8

/*
Name: isMeta
Args: one character
Output: boolean value
Info: check if current event is meta event
*/
isMeta :: Char -> Bool
isMeta c = toInt c == 255

/*
Name: isTempo
Args: a list of bytes
Output: boolean value
Info: check if current event is meta event and the type is "Set Tempo"
*/
isTempo :: [Char] -> Bool
isTempo [event, type] = isMeta event && (toInt type) == 81

/*
Name: isTimeSignature
Args: a list of bytes
Output: boolean value
Info: check if current event is meta event and the type is "Time Signature"
*/
isTimeSignature :: [Char] -> Bool
isTimeSignature [event, type] = isMeta event && (toInt type) == 88

/*
Name: getChannel
Args: one character
Output: return the channel message of an event
*/
getChannel :: Char -> Int
getChannel c = secondHalfStatus c

/*
Name: getFrequency
Args: one character
Output: a real number of frequency of an event
Info: frequency information comes from note number
*/
getFrequency :: Char -> Frequency
getFrequency c
	#! n = toInt c
	|n >= 0 || n <= 127 = 440.0 * 2.0 ^ (toReal(n - 69) / 12.0)
	= abort "incorrect MIDI note number\n"

/*
Name: getVelocity
Args: one character
Output: an integer of velocity value of an event
*/
getVelocity :: Char -> Int
getVelocity c = toInt c




