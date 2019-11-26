definition module Util.TimeUtils
import Input.ReadFile

:: Tempo :== Real

:: Time = {minutes :: Real
          ,seconds :: Real
          }

:: TimeSignature = {barVal :: Int
                   ,noteVal :: Int
                   }

:: Beat = {p :: Int,
           q :: Int}

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
