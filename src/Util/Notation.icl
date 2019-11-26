implementation module Util.Notation
import StdEnv

import Util.Pitch, Util.TimeUtils
import Input.Chunks, Input.ReadFile

:: ManualNote = { note :: String, duration :: Beat}
:: Next = On ManualNote | Off Beat
:: Melody :== [Next]

genNote :: (String,Int,Int) -> ManualNote
genNote (x,a,b) = {note = x, duration = {p = a, q = b}}