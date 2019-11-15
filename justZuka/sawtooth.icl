module sawtooth
import StdEnv
import constants
import accesstable
import sinewave

:: Wave = {
            timbre :: [Real],
            harmonics :: [Real]
          }


sineTable = generateSine 1.0
h = [1.0,2.0..50.0]
a = [toReal((-1)^(((toInt k) rem 2) + 1)) * (1.0 / k) \\ k <- h]

sumLists :: [Real] [Real] -> [Real]
sumLists [] [] = []
sumLists [a] [b] = [a+b]
sumLists [x:xs] [y:ys] = [x+y] ++ (sumLists xs ys)

generateSawTooth :: [Real] 
generateSawTooth = foldr sumLists (repeatn (length (l!!0)) 0.0) l
where 
    l = (get sineTable h a freq)


generatePulse :: Real Real -> [Real]
generatePulse t bigt = [ sum [2.0 / (n * PI) * sin(PI*n*t/bigt) * cos(2.0*PI*n*i/bigt) \\ n <- [1.0,2.0..100.0]] + t/bigt \\ i <- [0.0,1.0..(toReal tableSize)] ]
// Start = generatePulse 200.0 1000.0
