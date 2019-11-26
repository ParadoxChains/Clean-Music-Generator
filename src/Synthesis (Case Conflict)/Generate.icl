implementation module Synthesis.Generate
import StdEnv
import Util.Constants
import Synthesis.Accesstable
import Synthesis.Wavetable
import Util.ListUtils
import Synthesis.Wave
import Util.Rand




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



generate :: Wave Int -> [Real]
generate Square freq = wave hSquare aSquare freq
generate Sawtooth freq = wave hSawtooth aSawtooth freq
generate Triangle freq = wave hTriangle aTriangle freq
generate Noise freq = sumAll l
where 
    l = [shiftLeft list i \\ list <- (get (wavetable 1.0) hNoise aNoise freq) & i <- randoms]
generate Pulse freq = subtractLists (shiftLeft saw (SAMPLING_RATE/(2*freq))) saw
where
    saw = generate Sawtooth freq


// Start = generate Square
