definition module Input.readFile
import StdMaybe

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


process :: [Char] -> Info

//read information in the header chunk
processHeader :: [Char] -> HeaderInfo

//read information in the track chunk
processTrack :: [Char] -> [TrackInfo]

//process track chunk with chunk type dropped
//return a list of list of Message
processTrackBody :: [Char] -> [TrackInfo]

//give back a piece of message info in track chunk
processMessage :: Int [Char] -> TrackInfo
	
//read events information in track chunks
//give back events or nothing
processEvent :: [Char] -> Maybe Event
	
//calculate the length of each event which depends on the event type
eventLen:: Int [Char]->Int

readBytes :: *File -> ([Char], *File)

read :: !*World -> (*World, Info)