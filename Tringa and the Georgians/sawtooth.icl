module sawtooth
import StdEnv
import constants
import accesstable
import sinewave

:: Wave = {
            timbre :: [Real],
            harmonics :: [Real]
          }


generateSawTooth :: Real -> [Real]
generateSawTooth ampl = [(ampl/2.0) - (ampl / PI) * (sum  [(-1.0)^k * (sin(2.0*PI*k*t/offset) /k) \\ k <- [1.0,2.0..100.0]])  \\ t <- [0.0,1.0..(offset - 1.0)]] 
where
    offset = toReal(SAMPLING_RATE/I_DONT_KNOW_WHAT_ITS_CALLED)

generateSquare :: Real -> [Real]
generateSquare ampl = [(sum  [(-1.0)^k * (sin(2.0*PI*k*t/offset) / k) \\ k <- [1.0,3.0..10.0]])  \\ t <- [0.0,1.0..(offset - 1.0)]] 
where
    offset = toReal(SAMPLING_RATE/I_DONT_KNOW_WHAT_ITS_CALLED)


generatePulse :: Real Real -> [Real]
generatePulse t bigt = [ sum [2.0 / (n * PI) * sin(PI*n*t/bigt) * cos(2.0*PI*n*i/bigt) \\ n <- [1.0,2.0..100.0]] + t/bigt \\ i <- [0.0,1.0..(toReal tableSize)] ]
Start = generatePulse 5.0 10.0
