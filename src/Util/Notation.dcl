definition module Util.Notation
import StdEnv

import Util.Pitch, Util.TimeUtils
import Input.Chunks, Input.ReadFile

:: ManualNote = { note :: String, duration :: Beat}
:: Next = On ManualNote | Off Beat
:: Melody :== [Next]

genNote :: (String,Int,Int) -> ManualNote

getMelodyLength :: Melody -> Beat