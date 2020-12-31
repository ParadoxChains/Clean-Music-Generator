implementation module Synthesis.Accesstable
import StdEnv
import Util.Constants
import Util.ListUtils
import Util.TypeDefs


// Takes wave_table, frequency, harmonic and number of samples and gets us desired values from wave_table.
getValuesFromWavetable :: !{Real} !Frequency !Int !Samples -> [Real]
getValuesFromWavetable wave_table frequency harmonic dur = [(getValueFromWavetable i wave_table) \\ i <- indexes]
where
    indexes = generateIndexes frequency harmonic dur

// Takes frequency harmonic and number of samples as parameters and generates list of points
generateIndexes :: !Frequency !Int !Samples -> [Real]
generateIndexes frequency harmonic dur = map (\x = realRem x (toReal TABLE_SIZE)) (take dur [0.0, rate..])
where
    new_rate = toReal(SAMPLING_RATE)/((toReal harmonic)*frequency)
    rate = toReal(TABLE_SIZE)/new_rate

// Takes Real index and wave table and gives us value at that index
// If neccessary uses linear interpolation
getValueFromWavetable :: !Real !{Real} -> Real
getValueFromWavetable r wave_table
    | toReal (floor r) == r = wave_table.[(floor r)]
    | (floor r) == TABLE_SIZE - 1 = interpolate r (floor r) 0 wave_table
    = interpolate r (floor r) ((floor r) + 1) wave_table

// linear interpolation
interpolate :: !Real !Int !Int !{Real} -> Real
interpolate r x0 x1 wave_table = (wave_table.[x0]) + (r-toReal(x0)) * (wave_table.[x0] - wave_table.[x1]) / toReal(x1-x0)

get :: !{Real} [Real] [Real] !Frequency !Samples -> [[Real]]
get wave_table harmonics amplitudes freq dur = [map (\x = x * ampl) l \\ l <- values & ampl <- amplitudes]
where
    values = [getValuesFromWavetable wave_table freq (toInt(har)) dur \\ har <- harmonics]
