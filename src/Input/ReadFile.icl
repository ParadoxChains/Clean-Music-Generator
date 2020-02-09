implementation module Input.ReadFile

import StdEnv
import StdFile
import StdMaybe
import Input.Chunks
import Util.Byte, Util.TimeUtils

:: PreviousDeltaTime :== Int


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

//note on,note off,two meta events and relative useful information
::Event = NoteOn Channel Frequency Velocity | NoteOff Channel Frequency Velocity
		| TP Tempo| TS TimeSignature| Other

//the information that read from the file
:: Info = 
	{
		//a record which store the information of header chunk 
		headerInfo :: HeaderInfo,
		//a list of TrackInfo which contains a list of Message which is a record
		//so it is basically a list of list of record
		trackInfo :: [TrackInfo]
	}


readFile :: [Char] -> [Note]
readFile l = processInfo(process l)

processInfo :: Info -> [Note]
processInfo {headerInfo,trackInfo} 
	= flatten(map (\x = note 0 x) trackInfo)

note :: PreviousDeltaTime TrackInfo -> [Note] 
note _ [] = []
note pd l
	#! {deltaTime,event} = hd l 
	#! initialT = pd + deltaTime
	= case event of
		NoteOn ch bgFre veo -> [{
				channel = ch,
				frequency = bgFre,
				veolocity = veo,
				initialTime = initialT,
				duration = findDeltaTime bgFre (tl l),
				/*
					TODO: Remove hardcoding.
					Currently hardcoded to Liszt Hungarian Rhapsody 2.
				*/
				ts = {barVal = 2, noteVal = 4},
				temp = 80.0
			} : note initialT (tl l)]
		_ -> note initialT (tl l)
		


//return delta time to calculate the duration
findDeltaTime :: Real TrackInfo -> Int
findDeltaTime _ [] = 0
findDeltaTime f l
	#! {deltaTime,event} = hd l  
	#! fre = case event of
		NoteOff a b c -> b
		_ -> -1.0
	|f == fre = deltaTime
	= deltaTime + findDeltaTime f (tl l)

//calculate the duration of an event
//calcDuration ::

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
		division = calcDivision (take 2(drop 4 l))
	}

processTrack :: [Char] -> [TrackInfo]
processTrack [] = []
processTrack l 
	//4bytes:type of chunk(mtrk)
	|isTrack l = processTrackBody (drop 4 l)
	= processTrackBody l

processTrackBody :: [Char] -> [TrackInfo]
processTrackBody l
	//4 bytes for length information
	#! chunkLen = trackChunkLen l
	#! chunkBody = drop 4 l
	//delete delta time info bytes
	= [processMessage 0 '\0' (take chunkLen chunkBody): processTrack (drop chunkLen chunkBody)]
	
processMessage :: Int Char [Char] -> TrackInfo
processMessage _ _ [] = []
processMessage lastEventLen lastType l 
	#! (result,deltaLen) =  deltaTime l
	#! chunkbody = drop deltaLen l
	#! eventLen = eventLen lastEventLen chunkbody
	#! eventType = hd chunkbody
	| toInt eventType < 128 = [{	deltaTime = result,
									event = processEvent [lastType:chunkbody]
									}: processMessage lastEventLen lastType (drop eventLen chunkbody)]
	= [{	deltaTime = result,
			event = processEvent chunkbody
			}: processMessage eventLen eventType (drop eventLen chunkbody)]
			
processEvent :: [Char] -> Event
processEvent l
	# cons = NoteOff (getChannel (l!!0)) (getFrequency (l!!1)) (getVelocity (l!!2))
	| isNoteOn (l!!0) 
		| getVelocity (l!!2) <> 0 = NoteOn (getChannel (l!!0)) (getFrequency (l!!1)) (getVelocity (l!!2))
		= cons
	| isNoteOff (l!!0) = cons
	| isTimeSignature (take 2 l) = TS {barVal = toInt(l!!3),noteVal = 2^toInt(l!!4)}
	| isTempo (take 2 l) = TP fromBytes Unsigned BE(take 3 (drop 3 l))
	= Other
//,what's worse,I won't have internet in two hours,but I can still write the code.
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
