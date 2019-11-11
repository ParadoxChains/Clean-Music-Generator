implementation module accesstable
import StdEnv
import constants


floor :: Real -> Int
floor r
| toReal (toInt r) > r = (toInt r) - 1
= toInt r

// Takes wavetable, frequency and harmonic and gets us desired values from wavetable. 
getValues :: [Real] Int Int -> [Real]
getValues waveTable frequency harmonic = [(getValue i waveTable) \\ i <- indexes]
where
    indexes = getIndexes frequency harmonic

// Takes frequency and and harmonic as parameters and generates list of points 
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



get :: [Real] [Real] [Real] Int -> [[Real]]
get waveTable harmonics amplitudes freq = [map (\x = x * ampl) l \\ l <- values & ampl <- amplitudes]
where
    values = [getValues waveTable (toInt(har)) freq \\ har <- harmonics]

