module sinewave
import StdEnv


:: Wave = {
            amplitudes :: [Real],
            harmonics :: [Int]
          }


PI :== 3.1415926535898
SAMPLING_RATE :== 44100
I_DONT_KNOW_WHAT_ITS_CALLED :== 20
tableSize :== 2205
freq :== 440

generateSine :: Real -> [Real]
generateSine ampl = [ampl * sin(0.0 + i * 2.0 * PI /offset) \\ i<-[0.0,1.0..(offset - 1.0)]]
where
    offset = toReal(SAMPLING_RATE/I_DONT_KNOW_WHAT_ITS_CALLED)


// Start = generateSine 1.0

getSubset :: [Real] [Int] -> [Real]
getSubset waveTable indexes = [waveTable!!i \\ i <- indexes]

getIndexes :: Int Int -> [Int]
getIndexes frequency harmonic = take newRate [0, newRate..]
where
    newRate = SAMPLING_RATE/(harmonic*frequency)                  // Linear Interpolation needed 

// Start = getIndexes 440 3
