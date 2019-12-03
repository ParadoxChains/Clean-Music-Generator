implementation module Synthesis.Generate
import StdEnv
import Util.Constants
import Synthesis.Accesstable
import Synthesis.Wavetable
import Util.ListUtils
import Synthesis.Wave
import Util.Rand
import Util.TypeDefs
import Util.ArrayUtils

//Sine wave
hSine = [1.0]
aSine = [1.0]


// sawtooth wave
hSawtooth = [1.0,2.0..50.0]
aSawtooth = [ (-1.0)^(k+1.0) * (1.0 / k) \\ k <- hSawtooth]




// Start = sawtooth
// square wave
hSquare = [1.0,3.0..100.0]
aSquare = [1.0 / x \\ x <- hSquare]



// triangle wave
hTriangle = [1.0,3.0..100.0]
aTriangle = [ (-1.0)^(i + 1.0) * (1.0/(k^2.0)) \\ k <- hTriangle & i <- [1.0..] ]



// noise wave
// requires VERY large heap  --  100M is enough
hNoise = map (\x = x * 36.0) (take 100 (genRandReal 1))
aNoise = repeatn 100 1.0
randoms = map (\x = x rem 40) (take 100 (genRandInt 1))



// pulse wave
// requires VERY large heap  --  100M is enough



generate :: Wave Frequency Duration -> [Real]
generate Sine freq dur = wave hSine aSine freq dur
generate Square freq dur = wave hSquare aSquare freq dur
generate Sawtooth freq dur = wave hSawtooth aSawtooth freq dur
generate Triangle freq dur = wave hTriangle aTriangle freq dur
generate Noise freq dur = sumAll l
where 
    // l = [shiftLeft list i \\ list <- (get (wavetable 1.0) hNoise aNoise freq dur) & i <- randoms]
generate Pulse freq dur = subtractLists (shiftLeft saw (SAMPLING_RATE/(2*(toInt freq)))) saw
where
    saw = generate Sawtooth freq dur
generate Silence _ dur = [abs(x*0.0)\\x<-(wave [1.0] [0.0] 440.00 dur)]

// Start = generate Square

