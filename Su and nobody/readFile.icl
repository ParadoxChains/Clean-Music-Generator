module readFile

import StdEnv
import StdFile
//import StdMaybe
import chunks


:: Maybe x
	= Just x
	| Nothing

//calculate the length of each event
eventLen:: Int [Char]->Int
eventLen lastLen l
	#! n1 = firstHalfStatus (hd l)
	#! n2 = secondHalfStatus (hd l)
	//meta events -- status byte, type byte, length byte
	|n1 >= 15 && n2 == 15 = toInt(l !! 2) + 3
	//system exclusive events -- status byte, length byte
	|n1 >= 15 && n2 >=0 && n2 <= 7 = toInt(l !! 1) + 2
	//midi events
	|n1 == 12 || n1 == 13 = 2
	|(n1 >= 8 && n1 <= 11) || n1 == 14 = 3
	|n1 < 8 = lastLen - 1
	= abort (toString n2)
//store the useful information of header chunk		

:: HeaderInfo = 
	{
		//MIDI file format,valid:0,1,2
		format :: Int,
		//default unit of delta-time for this MIDI file
		division:: Int
	}

//store the information of a track which has several events(messages)
:: TrackInfo :== [Message]

//store the information about a piece of event
:: Message = 
	{
		//number of 'ticks' from the previous event, represented as a variable length quantity
		deltaTime :: Int,
		//different events in the chunk
		event :: Event
	}

//type alias
:: Channel :== Int

:: Frequency :== Real

//note on,note off events and relative useful information
::Event = NoteOn Channel Frequency| NoteOff Channel Frequency

//the final information that read from the file
:: Info = 
	{
		//a record which store the information of header chunk 
		headerInfo :: HeaderInfo,
		//a list of TrackInfo which contains a list of Message which is a record
		//so it is basically a list of list of record
		trackInfo :: [TrackInfo]
	}

//start to process the file
process :: [Char] -> Info
process l
	|length l > 14 && isHeader (take 4 l) = 
		{ 
			headerInfo = processHeader (drop 8 l), 
			trackInfo = processTrack (drop 14 l)
		}
	= abort "not enough information"

//read information in the header chunk
processHeader :: [Char] -> HeaderInfo
processHeader l = 
	{
		format = calcFormat (take 2 l),
		division = calcDivision (take 2(drop 4 l))
	}

//read information in the track chunk
processTrack :: [Char] -> [TrackInfo]
processTrack [] = []
processTrack l 
	//4bytes:type of chunk-mtrk
	|isTrack l = processTrackBody (drop 4 l)
	= processTrackBody l

//process track chunk with information beginning with chunk head dropped
//list of list of Message
processTrackBody :: [Char] -> [TrackInfo]
processTrackBody l
	//4 bytes for length information
	#! chunkLen = trackChunkLen l
	#! chunkBody = drop 4 l
	//delete delta time info bytes
	#! nextChunk = drop chunkLen chunkBody 
	= [processMessage 0 chunkBody: processTrack nextChunk]
	
processMessage :: Int [Char] -> TrackInfo
processMessage lastEventLen [] = []
processMessage lastEventLen l 
	#! (result,deltaLen) =  deltaTime l
	#! chunkbody = drop deltaLen l
	#! eventLen = eventLen lastEventLen chunkbody
	= case processEvent chunkbody of 
		Just correctEvent -> [{
			deltaTime = result,
			event = correctEvent
			}: processMessage eventLen (drop eventLen chunkbody)]
		Nothing -> processMessage eventLen (drop eventLen chunkbody)
	
//read information about events in the track chunks
//give back events or
processEvent :: [Char] -> Maybe Event
processEvent [c:cs] 
	|isNoteOn c = Just(NoteOn (getChannel c) (getFrequency c))
	|isNoteOff c = Just(NoteOff (getChannel c) (getFrequency c))
	= Nothing

readBytes :: *File -> ([Char], *File)
readBytes oldF 
	#! (b, c, newF) = freadc oldF
	|not b = ([], newF)
	#! (l, f) = readBytes newF
	= ([c:l], f)

read :: !*World -> (*World, Info)
read oldW
	#! (b, oldF, newW) = fopen "simple.mid" FReadData oldW
	|not b = (newW, abort"can not open file")
	#! (l, newF) = readBytes oldF
	#! (b, newW2) = fclose newF newW
	= (newW2, process l)
		
Start w = read w