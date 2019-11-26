module DeemoMain
import StdEnv

//Custom library imports
import Synthesis.Accesstable, Synthesis.Generate, Synthesis.Wave, Synthesis.Wavetable
import Util.Byte, Util.Constants, Util.ListUtils, Util.Rand, Util.TimeUtils
import Input.Chunks, Input.ReadFile
import Output.MiddleLayer, Output.Pcm
/*
:: Next = On Note | Off Beats
:: Beats :== Real
:: Melody :== [Next]
:: Harmony :== [Next]
:: NoteNumber :== Int

instance + Next
where
    + (Off x) (Off y) = Off (x+y)
    + (On x) (Off y) = Off (convertDurToBeats(x.duration) {barVal = 4, noteVal = 4} + y)
    + (Off x) (On y) = Off (x + convertDurToBeats(y.duration) {barVal = 4, noteVal = 4})
    + (On x) (On y) = Off (convertDurToBeats(x.duration + y.duration) {barVal = 4, noteVal = 4})

genericNote :: NoteNumber Beats -> Note
genericNote n b = {channel=0, frequency = getFrequency(toChar n), veolocity = 0, duration = (convertBeatsToDur b {barVal = 4, noteVal = 4})}

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
SquareProfile = {type = Square, env = {attack=(1.0/32.0), decay=(1.0/64.0), sustain = 0.2, release = 0.25}}
*/
Start = 1