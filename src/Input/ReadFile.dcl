definition module Input.ReadFile
import StdMaybe
import Util.TypeDefs, Util.TimeUtils



:: Note = 
	{
		channel :: Channel,
		frequency :: Frequency,
		veolocity :: Velocity,
		initialTime :: Int,
		duration :: Duration,
		ts :: TimeSignature,
		temp :: Tempo
	}

readFile :: [Char] -> [Note]
