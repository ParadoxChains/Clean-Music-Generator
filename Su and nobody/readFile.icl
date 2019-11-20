module readFile

import StdEnv
import StdFile
import StdDebug
import chunks


SysexEventF0 :== 240
SysexEventF7 :== 247

//calculate the length of each event
eventLen::[Char]->Int
eventLen event = sum [((toInt x) rem 128)*pw 
					\\ x<-lengthData & pw<-(reverse (map (\x=2^((x-1)*7))[1,2..(length lengthData)]))]
where
	eventType = toInt (hd event) // Event type - First byte
	lengthData 											// Length data
		| (eventType == SysexEventF0 || eventType == SysexEventF7) 
		   = (takeWhile (\x=((toInt x)/128)==1) (drop 1 event)) 
		   	++ (take 1 (dropWhile (\x=((toInt x)/128)==1) (drop 1 event)))// Extract time for Sysex events
		= (takeWhile (\x=((toInt x)/128)==1) (drop 2 event)) 
			++ (take 1 (dropWhile (\x=((toInt x)/128)==1) (drop 2 event))) // Extract time for Meta events

//store the useful information of header chunk		
:: HeaderInfo = 
	{
		//MIDI file format,valid:0,1,2
		format :: Int,
		//default unit of delta-time for this MIDI file
		division:: Int
	}

:: TrackInfo :== [Message]

//store the information about a piece of event
:: Message = 
	{
		//number of 'ticks' from the previous event, represented as a variable length quantity
		deltaTime :: Int,
		//different events in the chunk
		event :: Event
	}

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
processTrack l 
	|isTrack l = processTrackHead l
	= processTrackBody l

//process track chunk from the beginning
processTrackHead :: [Char] -> [TrackInfo]
processTrackHead l
	//4bytes:type of chunk-mtrk
	//4bytes length of track trunk-useless since there's always a type indication 
	//len-length of delta time
	//resultL
	#! resultL = drop 8 l
	#! (result,len) =  deltaTime resultL
	#! processL = drop len resultL 
	= [{
		deltaTime = result,
		event = processEvent processL
	}:processTrack (drop (eventLen processL) processL)]

//process track chunk with information beginning at chunk body	
processTrackBody :: [Char] -> [TrackInfo]

//read information about events in the track chunks
processEvent :: [Char] -> Event
processEvent [c:cs] 
	|isNoteOn c = NoteOn (getChannel c) (getFrequency c) 
	|isNoteOff c = NoteOff (getChannel c) (getFrequency c) 
	= abort "unknow event"

readBytes :: *File -> ([Char], *File)
readBytes oldF 
	#! (b, c, newF) = freadc oldF
	|not b = ([], newF)
	#! (l, f) = readBytes newF
	= ([c:l], f)

read :: !*World -> (*World, Info)
read oldW
	#! (b, oldF, newW) = fopen "minimal.mid" FReadData oldW
	|not b = (newW, abort"can not open file")
	#! (l, newF) = readBytes oldF
	#! (b, newW2) = fclose newF newW
	= (newW2, process l)
		
Start w = read w