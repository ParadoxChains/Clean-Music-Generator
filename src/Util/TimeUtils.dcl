definition module Util.TimeUtils
import Input.MIDI.ReadFile
import Util.TypeDefs

instance + Beat
instance == Beat
instance zero Beat

// Gets Time and converts to seconds
timeToSeconds :: Time -> Real

// Gets seconds and returns number of samples
secondsToSamples :: Real -> Int

// Gets time and returns number of samples
timeToSamples :: Time -> Int

// Gets notelength, time signature and tempo and returns number of samples
noteToSamples :: Beat TimeSignature Tempo -> Int

simplifyBeat :: Beat -> Beat

convertDurToBeats :: Duration TimeSignature -> Beat

convertBeatsToDur :: Beat TimeSignature -> Duration
