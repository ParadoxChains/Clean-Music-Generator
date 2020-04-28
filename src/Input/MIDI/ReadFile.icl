implementation module Input.ReadFile

import StdEnv
import StdFile
import StdMaybe
import Input.Chunks
import Util.Byte, Util.TimeUtils

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
		| TP Int | TS TimeSignature| Other

//the information that read from the file
:: Info = 
	{
		//a record which store the information of header chunk 
		headerInfo :: HeaderInfo,
		//a list of TrackInfo which contains a list of Message which is a record
		//so it is basically a list of list of record
		trackInfo :: [TrackInfo]
	}

/*
Name: readFile
Args: a list of bytes
Output: a list of note messages
*/
readFile :: [Char] -> [Note]
readFile l
	#! info = process l
	= processInfo info (getT tp info) (getT ts info)

/*
Name: getT
Args: a function and track chunk information
Output: a list of tempo or timesignature events
Info: a higher order function for getting tempo and timesignature
*/	
getT :: (AccumulatedTime TrackInfo -> [(InitialTime, a)]) Info -> [(InitialTime, a)]
getT f {trackInfo} = flatten(map(\x = f 0 x)trackInfo)

/*
Name: tp
Args: accumelated delta time and track chunk information
Output: a list of tempo event
*/	
tp :: AccumulatedTime TrackInfo -> TPEvents
tp _ [] = []
tp acc [m:ms] 
	#! initialT = acc + m.deltaTime
	= case m.event of 
		TP tempo -> [(initialT, tempo):tp initialT ms]
		_ -> tp initialT ms

/*
Name: ts
Args: accumelated delta time and track chunk information
Output: a list of time signature event
*/	
ts :: AccumulatedTime TrackInfo -> TSEvents
ts _ [] = []
ts acc [m:ms] 
	#! initialT = acc + m.deltaTime
	= case m.event of 
		TS timeS -> [(initialT, timeS):ts initialT ms]
		_ -> ts initialT ms

/*
Name: processInfo
Args: information of chunks and two meta events
Output: a list of note messages
*/		
processInfo :: Info TPEvents TSEvents-> [Note]
processInfo {headerInfo,trackInfo} tp ts
	= flatten(map (\x = note 0 x tp ts) trackInfo)

/*
Name: note
Args: accumelated delta time, track chunk information and two meta events
Output: a list of note messages
*/
note :: AccumulatedTime TrackInfo TPEvents TSEvents-> [Note] 
note _ [] _ _= []
note acc l tpe tse
	#! {deltaTime,event} = hd l 
	#! initialT = acc + deltaTime
	#! timeSig = findT (-1,{barVal = 4, noteVal = 4}) initialT tse 
	= case event of
		NoteOn ch bgFre veo -> [{
				channel = ch,
				frequency = bgFre,
				veolocity = veo,
				initialTime = initialT,
				duration = findDeltaTime bgFre (tl l),
				ts = timeSig,
				temp = calcTempo (findT (-1,10^6*timeSig.noteVal/8) initialT tpe) timeSig.noteVal
			} : note initialT (tl l) tpe tse]
		_ -> note initialT (tl l) tpe tse

/*
Name: calcTempo
Args: two integers
Output: tempo value (default: 120 beats/minute)
*/		
calcTempo :: Int Int -> Real
calcTempo x v = 1.5 * 10.0^7.0 * (toReal v) / (toReal x)

/*
Name: findT
Args: the latest deltatime with tempo/timesig that is earlier than initial time, 
		initial time, a list of tempo and timesig events
Output: the latest tempo or timesignature event that is earlier than initial time
*/
findT :: (Int,a) Int [(Int,a)] -> a
findT (t,x) _ [] = x
findT (t,x) initT [e:es]
	| initT >= fst e && t <= fst e = findT e initT es
	= findT (t,x) initT es

/*
Name: findDeltaTime
Args: frequency and track chunk information
Output: an integer value of delta time
*/	
findDeltaTime :: Real TrackInfo -> Int
findDeltaTime _ [] = 0
findDeltaTime frequency l
	#! {deltaTime,event} = hd l  
	#! fre = case event of
		NoteOff a b c -> b
		_ -> -1.0
	|frequency == fre = deltaTime
	= deltaTime + findDeltaTime frequency (tl l)

/*
Name: process
Args: a list of bytes
Output: information about header chunk and truck chunks
*/
process :: [Char] -> Info
process l
	|length l > 14 && isHeader (take 4 l) = 
		{ 
			headerInfo = processHeader (drop 8 l), 
			trackInfo = processTrack (drop 14 l)
		}
	= abort "not enough information"

/*
Name: processHeader
Args: a list of bytes
Output: information about the header chunk
*/
processHeader :: [Char] -> HeaderInfo
processHeader l = 
	{
		format = calcFormat (take 2 l),
		division = calcDivision (take 2(drop 4 l))
	}

/*
Name: processHeader
Args: a list of bytes
Output: a list of track chunk information
*/
processTrack :: [Char] -> [TrackInfo]
processTrack [] = []
processTrack l 
	//4bytes:type of chunk(mtrk)
	|isTrack l = processTrackBody (drop 4 l)
	= processTrackBody l

/*
Name: processTrackBody
Args: a list of bytes
Output: a list of track chunk information
*/
processTrackBody :: [Char] -> [TrackInfo]
processTrackBody l
	//4 bytes for length information
	#! chunkLen = trackChunkLen l
	#! chunkBody = drop 4 l
	//delete delta time info bytes
	= [processMessage 0 '\0' (take chunkLen chunkBody): processTrack (drop chunkLen chunkBody)]

/*
Name: processMessage
Args: the length in bytes of previous event, the type of previous event, a list of bytes
Output: one piece of track chunk information
*/		
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

/*
Name: processEvent
Args: a list of bytes
Output: one event
*/			
processEvent :: [Char] -> Event
processEvent l
	# cons = NoteOff (getChannel (l!!0)) (getFrequency (l!!1)) (getVelocity (l!!2))
	| isNoteOn (l!!0) 
		| getVelocity (l!!2) <> 0 = NoteOn (getChannel (l!!0)) (getFrequency (l!!1)) (getVelocity (l!!2))
		= cons
	| isNoteOff (l!!0) = cons
	| isTimeSignature (take 2 l) = TS {barVal = toInt(l!!3),noteVal = 2^toInt(l!!4)}
	| isTempo (take 2 l) = TP (fromBytes Unsigned BE(take 3 (drop 3 l)))
	= Other

/*
Name: eventLen
Args: an integer, a list of bytes
Output: the length of current event
*/
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
