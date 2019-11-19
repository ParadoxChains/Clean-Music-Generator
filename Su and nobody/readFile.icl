module readFile

import StdEnv
import StdFile
import chunks

:: HeaderInfo = 
	{
		//MIDI file format,valid:0,1,2
		format :: Int,
		//default unit of delta-time for this MIDI file
		division:: Int
	}

:: TrackInfo :== [Message]

:: Message = 
	{
		//number of 'ticks' from the previous event, represented as a variable length quantity
		deltaTime :: Int,
		//different events in the chunk
		event :: Event
	}

:: Channel :== Int

:: Frequency :== Real

::Event = NoteOn Channel Frequency| NoteOff Channel Frequency

:: Info = 
	{ 
		headerInfo :: HeaderInfo,
		trackInfo :: TrackInfo
	}

process :: [Char] -> Info
process l
	|length l > 14 && isHeader (take 4 l) = 
		{ 
			headerInfo = processHeader (drop 8 l), 
			trackInfo = processTrack (drop 14 l)
		}
	= abort "not enough information"

processHeader :: [Char] -> HeaderInfo
processHeader l = 
	{
		format = calcFormat (take 2 l),
		division = calcDivision(drop 4 l)
	}

processTrack :: [Char] -> TrackInfo
processTrack l 
	#! (result,len) =  deltaTime (drop 8 l)
	= [{
		deltaTime = result,
		event = precessEvent(drop len l)
	}:processTrack (drop eventLen n l)]

precessEvent :: [Char] -> (Event,Int)
precessEvent [c:cs] 
	|isNoteOn c = NoteOn (getChannel c) (getFrequency c) 
	|isNoteOff c = NoteOff (getChannel c) (getFrequency c) 
	= abort "unknown"

readBytes :: *File -> ([Char], *File)
readBytes oldF 
	#! (b, c, newF) = freadc oldF
	|not b = ([], newF)
	#! (l, f) = readBytes newF
	= ([c:l], f)

read :: !*World -> (*World, [Char])
read oldW
	#! (b, oldF, newW) = fopen "bytes.txt" FReadData oldW
	|not b = (newW, [])
	#! (l, newF) = readBytes oldF
	#! (b, newW2) = fclose newF newW
	= (newW2, l)
		
//Start w = read w