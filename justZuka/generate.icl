implementation module generate
import StdEnv
import constants
import accesstable
import sinewave
import utils
import wave
import rand


// sawtooth wave
hSawtooth = [1.0,2.0..50.0]
aSawtooth = [ (-1.0)^(k+1.0) * (1.0 / k) \\ k <- hSawtooth]

sawtooth :: [Real]
sawtooth = wave hSawtooth aSawtooth


// square wave
hSquare = [1.0,3.0..100.0]
aSquare = [1.0 / x \\ x <- hSquare]

square :: [Real]
square = wave hSquare aSquare


// triangle wave
hTriangle = [1.0,3.0..100.0]
aTriangle = [ (1.0/(x^2.0)) * (-1.0)^(y + 1.0) \\ x <- hTriangle & y <- [1.0..] ]

triangle :: [Real] 
triangle = wave hTriangle aTriangle


// noise wave
hNoise = map (\x = x * 36.0) (take 500 (genRandReal 1))
aNoise = repeatn 500 1.0
randoms = map (\x = x rem 30) (take 5000 (genRandInt 1))

noise :: [Real] 
noise = foldr sumLists (repeatn tableSize 0.0) l
where 
    l =  take 2 [phaseShift list i \\ list <- (get (generateSine 1.0) hNoise aNoise freq) & i <- randoms]
//       ^^^^^^      works only with take 2 


// pulse wave

pulse :: [Real]
pulse = subtractLists (phaseShift saw (SAMPLING_RATE/(4*freq))) saw
where                                   //            ^^^^^^ works only with 4*freq or more
    saw = sawtooth
