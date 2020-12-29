definition module Input.MIDI.ReadFile
import StdMaybe
import Util.TypeDefs, Util.TimeUtils

:: Note = 
	{
		channel :: !Channel,
		frequency :: !Frequency,
		veolocity :: !Velocity,
		initialTime :: !Int,
		duration :: !Duration,
		ts :: !TimeSignature,
		temp :: !Tempo
	}

/*
Name: readFile
Args: a list of bytes
Output: a list of note messages
*/
readFile :: [Char] -> [Note]
