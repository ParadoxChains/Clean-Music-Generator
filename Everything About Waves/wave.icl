implementation module wave
import StdEnv
import constants
import accesstable
import wavetable
import utils


sineTable :== (wavetable 1.0)


// takes harmonics and amplitudes as parameter and generates wave
wave :: [Real] [Real] -> [Real] 
wave h a = sumAll l
where 
    l = (get sineTable h a freq)

