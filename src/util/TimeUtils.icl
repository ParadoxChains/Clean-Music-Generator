implementation module util.TimeUtils
import StdEnv
import util.ListUtils

timeToSeconds :: Time -> Real
timeToSeconds t = t.minutes*60.0 + t.seconds

sampleRate :: Int
sampleRate = 44100

secondsToSamples :: Real -> Int
secondsToSamples seconds = floor (seconds * (toReal sampleRate))

timeToSamples :: Time -> Int
timeToSamples t = secondsToSamples (timeToSeconds t)

noteToSamples :: (Int, Int) TimeSignature Real -> Int
noteToSamples (p, q) timeSig tempo = secondsToSamples totalTime
where
	beats = ((toReal (p*q)) / (toReal timeSig.noteVal))
	beatLength = 60.0 / tempo
	totalTime = beats * beatLength

//Start = noteToSamples (1, 1) {barVal = 4, noteVal = 4} 120.0