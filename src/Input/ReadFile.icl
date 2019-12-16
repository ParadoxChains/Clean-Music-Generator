implementation module Input.ReadFile

import StdEnv
import StdFile
import StdMaybe
import Input.Chunks
import Util.Byte

:: PreviousDeltaTime :== Int

:: TrackInfo :== [Message]

:: HeaderInfo = 
	{
		format :: Int,
		division:: Int
	}

:: Message = 
	{
		deltaTime :: Int,
		event :: Event
	}

::Event = NoteOn Channel Frequency Velocity| NoteOff Channel Frequency Velocity | Other


:: Info = 
	{
		headerInfo :: HeaderInfo,
		trackInfo :: [TrackInfo]
	}

:: Note = 
	{
		channel :: Channel,
		frequency :: Frequency,
		veolocity :: Velocity,
		initialTime :: Int,
		duration :: Duration
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
				duration = findDeltaTime bgFre (tl l)
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
	= Other

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
