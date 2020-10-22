implementation module Util.Notation
import StdEnv

import Util.Pitch, Util.TimeUtils
import Input.MIDI.Chunks, Input.MIDI.Readfile

:: ManualNote = { note :: String, duration :: Beat}
:: Next = On ManualNote | Off Beat
:: Melody :== [Next]

genNote :: (String,Int,Int) -> ManualNote
genNote (x,a,b) = {note = x, duration = {p = a, q = b}}

getMelodyLength :: Melody -> Beat
getMelodyLength mel = sum[extractBeat this\\this<-mel]
where
    extractBeat :: Next -> Beat
    extractBeat (On {note = _, duration = d}) = d
    extractBeat (Off d) = d
