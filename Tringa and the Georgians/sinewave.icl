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

floor :: Real -> Int
floor r
| toReal (toInt r) > r = (toInt r) - 1
= toInt r

getSubset :: [Real] [Real] -> [Real]
getSubset waveTable indexes = [(getValue i waveTable) \\ i <- indexes]

getIndexes :: Int Int -> [Real]
getIndexes frequency harmonic = takeWhile (\x = (toInt x) < tableSize) [0.0, newRate..]
where
    newRate = toReal(SAMPLING_RATE)/toReal(harmonic*frequency) 

getValue :: Real [Real] -> Real
getValue r waveTable
| toReal (floor r) == r = waveTable!!(floor r)
= interpolate r (floor r) ((floor r) + 1) waveTable

interpolate :: Real Int Int [Real] -> Real
interpolate r x0 x1 waveTable = (waveTable!!x0) + (r-toReal(x0)) * (waveTable!!x0 - waveTable!!x1) / toReal(x1-x0)

// Start = getSubset (generateSine 1.0) (getIndexes 440 3)
// Start = getIndexes 440 3
