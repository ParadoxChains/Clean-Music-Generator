implementation module sawtooth
import StdEnv
import constants
import accesstable
import sinewave
import utils


sineTable = generateSine 1.0
h = [1.0,2.0..50.0]
a = [toReal((-1)^(((toInt k) rem 2) + 1)) * (1.0 / k) \\ k <- h]

generateSawTooth :: [Real] 
generateSawTooth = foldr sumLists (repeatn (length (l!!0)) 0.0) l
where 
    l = (get sineTable h a freq)


generatePulse :: Real Real -> [Real]
generatePulse t bigt = [ sum [2.0 / (n * PI) * sin(PI*n*t/bigt) * cos(2.0*PI*n*i/bigt) \\ n <- [1.0,2.0..100.0]] + t/bigt \\ i <- [0.0,1.0..(toReal tableSize)] ]
// Start = generatePulse 200.0 1000.0
