definition module util.TimeUtils

:: Time = {minutes :: Real
		  ,seconds :: Real
		  }

:: TimeSignature = {barVal :: Int
				   ,noteVal :: Int
				   }

// Gets Time and converts to seconds
timeToSeconds :: Time -> Real

// Samples per second
sampleRate :: Int

// Gets seconds and returns number of samples
secondsToSamples :: Real -> Int

// Gets time and returns number of samples
timeToSamples :: Time -> Int

// Gets notelength, time signature and tempo and returns number of samples
noteToSamples :: (Int, Int) TimeSignature Real -> Int