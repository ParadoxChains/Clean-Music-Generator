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
aTriangle = [ (-1.0)^(i + 1.0) * (1.0/(k^2.0)) \\ k <- hTriangle & i <- [1.0..] ]

triangle :: [Real] 
triangle = wave hTriangle aTriangle


// noise wave
// requires VERY large heap  --  100M is enough
hNoise = map (\x = x * 36.0) (take 100 (genRandReal 1))
aNoise = repeatn 100 1.0
randoms = map (\x = x rem 30) (take 100 (genRandInt 1))

noise :: [Real] 
noise =  sumAll l
where
    l = [phaseShift list i \\ list <- (get (generateSine 1.0) hNoise aNoise freq) & i <- randoms]



// pulse wave
// requires VERY large heap  --  100M is enough
pulse :: [Real]
pulse = subtractLists (phaseShift saw (SAMPLING_RATE/(2*freq))) saw
where
    saw = sawtooth
