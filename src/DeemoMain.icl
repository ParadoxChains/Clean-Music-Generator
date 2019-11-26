module DeemoMain
import StdEnv

//Custom library imports
import Synthesis.Accesstable, Synthesis.Generate, Synthesis.Wave, Synthesis.Wavetable
import Util.Byte, Util.Constants, Util.ListUtils, Util.Rand, Util.TimeUtils
import Input.Chunks, Input.ReadFile
import Output.MiddleLayer, Output.Pcm

:: Next = On Note | Off Beats
:: Beats :== Real
:: Melody :== [Next]
:: Harmony :== [Next]
:: NoteNumber :== Int

instance + Next
where
    + (Off x) (Off y) = Off (x+y)
    + (On x) (Off y) = Off (convertDurToBeats(x.duration) + y)
    + (Off x) (On y) = Off (x + convertDurToBeats(y.duration))
    + (On x) (On y) = Off (convertDurToBeats(x.duration + y.duration))

genericNote :: NoteNumber Beats -> Note
genericNote n b = {channel=0, frequency = getFrequency(toChar n), veolocity = 0, duration = (convertBeatsToDur b)}

convertDurToBeats :: Duration TimeSignature -> Beats
convertDurToBeats x {barVal = b, noteVal = n} = x/single
where
    single = 24*n
    measure = single * b

convertBeatsToDur :: Beats TimeSignature -> Duration
convertBeatsToDur x {barVal = b, noteVal = n} = x*single
where
    single = 24*n
    measure = single * b

:: Envelope = { attack :: Real, decay :: Real, sustain :: Real, release :: Real}

:: SynthProfile = { type :: Wave, env :: Envelope}

SquareProfile :: SynthProfile
SquareProfile = {type = Square, env = {attack=}}

Start = 1