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

generateSawTooth :: Real -> [Real]
generateSawTooth ampl = [(ampl/2.0) - (ampl / PI) * (sum  [(-1.0)^k * (sin(2.0*PI*k*t/offset) /k) \\ k <- [1.0,2.0..100.0]])  \\ t <- [0.0,1.0..(offset - 1.0)]] 
where
    offset = toReal(SAMPLING_RATE/I_DONT_KNOW_WHAT_ITS_CALLED)
// Start = generateSawTooth 1.0

generateSquare :: Real -> [Real]
generateSquare ampl = [(sum  [(-1.0)^k * (sin(2.0*PI*k*t/offset) / k) \\ k <- [1.0,3.0..100.0]])  \\ t <- [0.0,1.0..(offset - 1.0)]] 
where
    offset = toReal(SAMPLING_RATE/I_DONT_KNOW_WHAT_ITS_CALLED)
// Start = generateSquare 1.0

generatePulse :: Real Real -> [Real]
generatePulse t bigt = [ sum [2.0 / (n * PI) * sin(PI*n*t/bigt) * cos(2.0*PI*n*i/bigt) \\ n <- [1.0,2.0..100.0]] + t/bigt \\ i <- [0.0,1.0..(toReal tableSize)] ]
Start = generatePulse 500.0 1000.0
