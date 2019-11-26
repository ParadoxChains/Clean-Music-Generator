implementation module Util.TimeUtils
import StdEnv
import Util.ListUtils
import Util.Constants
import Input.ReadFile

timeToSeconds :: Time -> Real
timeToSeconds t = t.minutes*60.0 + t.seconds

secondsToSamples :: Real -> Int
secondsToSamples seconds = floor (seconds * (toReal SAMPLING_RATE))

timeToSamples :: Time -> Int
timeToSamples t = secondsToSamples (timeToSeconds t)

noteToSamples :: Beat TimeSignature Tempo -> Int
noteToSamples b timeSig t = secondsToSamples totalTime
where
	beats = ((toReal (b.p*timeSig.noteVal)) / (toReal b.q))
	beatLength = 60.0 / t
	totalTime = beats * beatLength

simplifyBeat :: Beat -> Beat
simplifyBeat x = {p = (x.p)/g, q = (x.q)/g}
where
    g = gcd x.p x.q

convertDurToBeats :: Duration TimeSignature -> Beat
convertDurToBeats x {barVal = b, noteVal = n} = newBeat
where
    single = 24*n
    tempBeat = {p = x, q = single}
    newBeat = simplifyBeat tempBeat

convertBeatsToDur :: Beat TimeSignature -> Duration
convertBeatsToDur {p = x, q = y} {barVal = b, noteVal = n} = final
where
    single = toReal(24*n)
    div = (toReal x) / (toReal y)
    final = toInt(div*single)