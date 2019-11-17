implementation module wave
import StdEnv
import constants
import accesstable
import sinewave
import utils


sineTable :== (generateSine 1.0)


// takes harmonics and amplitudes as parameter and generates wave
wave :: [Real] [Real] -> [Real] 
wave h a = sumAll l
where 
    l = (get sineTable h a freq)

