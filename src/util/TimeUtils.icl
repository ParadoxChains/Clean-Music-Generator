implementation module util.TimeUtils
import StdEnv
import util.ListUtils
import util.Constants

timeToSeconds :: Time -> Real
timeToSeconds t = t.minutes*60.0 + t.seconds

secondsToSamples :: Real -> Int
secondsToSamples seconds = floor (seconds * (toReal SAMPLING_RATE))

timeToSamples :: Time -> Int
timeToSamples t = secondsToSamples (timeToSeconds t)

noteToSamples :: (Int, Int) TimeSignature Real -> Int
noteToSamples (p, q) timeSig tempo = secondsToSamples totalTime
where
	beats = ((toReal (p*timeSig.noteVal)) / (toReal q))
	beatLength = 60.0 / tempo
	totalTime = beats * beatLength

