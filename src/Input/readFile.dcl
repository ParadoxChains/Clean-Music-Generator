definition module Input.readFile

:: Maybe x
	= Just x
	| Nothing

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
	
eventLen:: Int [Char]->Int

process :: [Char] -> Info

processHeader :: [Char] -> HeaderInfo

processTrack :: [Char] -> [TrackInfo]

processTrackBody :: [Char] -> [TrackInfo]

processMessage :: Int [Char] -> TrackInfo

processEvent :: [Char] -> Maybe Event

readBytes :: *File -> ([Char], *File)

read :: !*World -> (*World, Info)