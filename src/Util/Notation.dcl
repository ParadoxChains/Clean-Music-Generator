definition module Util.Notation
import StdEnv

import Util.Pitch, Util.TimeUtils
import Input.MIDI.Chunks, Input.MIDI.Readfile

:: ManualNote = { note :: String, duration :: Beat}
:: Next = On ManualNote | Off Beat
:: Melody :== [Next]

genNote :: (String,Int,Int) -> ManualNote

getMelodyLength :: Melody -> Beat
