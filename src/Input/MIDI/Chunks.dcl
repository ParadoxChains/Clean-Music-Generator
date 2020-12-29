definition module Input.MIDI.Chunks
import Util.TypeDefs

/*
Name: isHeader
Args: a list of bytes
Output: boolean value
Info: check if current is a header chunk
*/
isHeader :: ![Char] -> Bool

/*
Name: isTrack
Args: a list of bytes
Output: boolean value
Info: check if current is a track chunk
*/
isTrack :: ![Char] -> Bool

/*
Name: trackChunkLen
Args: a list of bytes
Output: the length of current track chunk
*/
trackChunkLen :: ![Char] -> Int

/*
Name: calcFormat
Args: a list of bytes
Output: the format of MIDI file 
Info: possible result -- 0 1 2
*/
calcFormat :: ![Char] -> Int

/*
Name: calcDivision
Args: a list of bytes
Output: the default unit of delta-time for this MIDI file
Info: possible result -- 0 1
*/
calcDivision :: ![Char] -> Int

/*
Name: deltaTime
Args: a list of bytes
Output: a delta time value and its length in bytes
*/
deltaTime :: ![Char] -> (!Int,!Int)

/*
Name: firstHalfStatus
Args: one character
Output: the type of MIDI event
*/
firstHalfStatus :: !Char -> Int

/*
Name: secondHalfStatus
Args: one character
Output: channel message
*/
secondHalfStatus :: !Char -> Int

/*
Name: isNoteOn
Args: one character
Output: boolean value
Info: check if current event is note on event
*/
isNoteOn :: !Char -> Bool

/*
Name: isNoteOff
Args: one character
Output: boolean value
Info: check if current event is note off event
*/
isNoteOff :: !Char -> Bool

/*
Name: isMeta
Args: one character
Output: boolean value
Info: check if current event is meta event
*/
isMeta :: !Char -> Bool

/*
Name: isTempo
Args: a list of bytes
Output: boolean value
Info: check if current event is meta event and the type is "Set Tempo"
*/
isTempo :: ![Char] -> Bool

/*
Name: isTimeSignature
Args: a list of bytes
Output: boolean value
Info: check if current event is meta event and the type is "Time Signature"
*/
isTimeSignature :: ![Char] -> Bool

/*
Name: getChannel
Args: one character
Output: return the channel message of an event
*/
getChannel :: !Char -> Int

/*
Name: getFrequency
Args: one character
Output: a real number of frequency of an event
Info: frequency information comes from note number
*/
getFrequency :: !Char -> Frequency

/*
Name: getVelocity
Args: one character
Output: an integer of velocity value of an event
*/
getVelocity :: !Char -> Int
