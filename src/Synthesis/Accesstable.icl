implementation module Synthesis.Accesstable
import StdEnv
import Util.Constants
import Util.ListUtils
import Util.TypeDefs



// Takes wavetable, frequency and harmonic and gets us desired values from wavetable. 
getValues :: {Real} Frequency Int Int -> [Real]
getValues waveTable frequency harmonic dur = [(getValue i waveTable) \\ i <- indexes]
where
    indexes = getIndexes frequency harmonic dur

// Takes frequency harmonic and duration as parameters and generates list of points 
getIndexes :: Frequency Int Int -> [Real]
getIndexes frequency harmonic dur = map (\x = realRem x (toReal tableSize)) (take dur [0.0, rate..])
where
    newRate = toReal(SAMPLING_RATE)/((toReal harmonic)*frequency)
    rate = toReal(tableSize)/newRate

// Takes Real index and wave table and gives us value at that index
// If neccessary uses linear interpolation
getValue :: Real {Real} -> Real
getValue r waveTable
    | toReal (floor r) == r = waveTable.[(floor r)]
    | (floor r) == tableSize - 1 = interpolate r (floor r) 0 waveTable
    = interpolate r (floor r) ((floor r) + 1) waveTable

// linear interpolation
interpolate :: Real Int Int {Real} -> Real
interpolate r x0 x1 waveTable = (waveTable.[x0]) + (r-toReal(x0)) * (waveTable.[x0] - waveTable.[x1]) / toReal(x1-x0)


// Takes wavetable, list of harmonics, list of amplitudes, frequency and duration
get :: {Real} [Real] [Real] Frequency Int -> [[Real]]
get waveTable harmonics amplitudes freq dur = [map (\x = x * ampl) l \\ l <- values & ampl <- amplitudes]
where
    values = [getValues waveTable freq (toInt(har)) dur \\ har <- harmonics]

