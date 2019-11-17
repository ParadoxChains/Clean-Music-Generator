implementation module sawtooth
import StdEnv
import constants
import accesstable
import sinewave
import utils


sineTable = generateSine 1.0
h = [1.0,2.0..50.0]
a = [toReal((-1)^(((toInt k) rem 2) + 1)) * (1.0 / k) \\ k <- h]

// generates sawtooth wave
generateSawTooth :: [Real] 
generateSawTooth = foldr sumLists (repeatn (length (l!!0)) 0.0) l
where 
    l = (get sineTable h a freq)

