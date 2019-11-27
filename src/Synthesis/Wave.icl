implementation module Synthesis.Wave
import StdEnv
import Util.Constants
import Synthesis.Accesstable
import Synthesis.Wavetable
import Util.ListUtils


sineTable :== (wavetable 1.0)


// takes harmonics and amplitudes as parameter and generates wave
wave :: [Real] [Real] Frequency Int -> [Real] 
wave h a freq dur = sumAll l
where 
    l = (get sineTable h a freq dur)


