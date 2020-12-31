definition module Synthesis.Accesstable
import Util.TypeDefs


// Takes wave_table, list of harmonics, list of amplitudes, frequency and number of samples
/*
Name: get
Input: wave_table, Harmonics, Amplitudes, Frequency, Number of Samples
*/
get :: !{Real} [Real] [Real] !Frequency !Samples -> [[Real]]
