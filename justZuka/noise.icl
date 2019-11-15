module noise
import StdEnv
import constants
import accesstable
import sinewave
import rand
import utils



sineTable = generateSine 1.0
h = map (\x = x * 36.0) (take 500 (genRandReal 1))
a = repeatn 500 1.0
randoms = map (\x = x rem 30) (take 5000 (genRandInt 1))






generateNoise :: [Real] 
generateNoise = foldr sumLists (repeatn (length (l!!0)) 0.0) l
where 
    l = take 2 [phaseShift list i \\ list <- (get sineTable h a freq) & i <- randoms]
    // l = get sineTable h a freq



// Start = generateNoise