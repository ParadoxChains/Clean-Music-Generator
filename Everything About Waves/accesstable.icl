implementation module accesstable
import StdEnv
import constants
import utils




// Takes wavetable, frequency and harmonic and gets us desired values from wavetable. 
getValues :: [Real] Int Int -> [Real]
getValues waveTable frequency harmonic = [(getValue i waveTable) \\ i <- indexes]
where
    indexes = getIndexes frequency harmonic

// Takes frequency and and harmonic as parameters and generates list of points 
getIndexes :: Int Int -> [Real]
getIndexes frequency harmonic = map (\x = realRem x (toReal tableSize)) (take (SAMPLING_RATE/20) [0.0, rate..])
where
    newRate = toReal(SAMPLING_RATE)/toReal(harmonic*frequency)
    rate = toReal(tableSize)/newRate

// Takes Real index and wave table and gives us value at that index
// If neccessary uses linear interpolation
getValue :: Real [Real] -> Real
getValue r waveTable
    | toReal (floor r) == r = waveTable!!(floor r)
    | (floor r) == tableSize - 1 = interpolate r (floor r) 0 waveTable
    = interpolate r (floor r) ((floor r) + 1) waveTable

// linear interpolation
interpolate :: Real Int Int [Real] -> Real
interpolate r x0 x1 waveTable = (waveTable!!x0) + (r-toReal(x0)) * (waveTable!!x0 - waveTable!!x1) / toReal(x1-x0)


// Takes wavetable, list of harmonics, list of amplitudes and frequency
get :: [Real] [Real] [Real] Int -> [[Real]]
get waveTable harmonics amplitudes freq = [map (\x = x * ampl) l \\ l <- values & ampl <- amplitudes]
where
    values = [getValues waveTable freq (toInt(har)) \\ har <- harmonics]

