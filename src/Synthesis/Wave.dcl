definition module Synthesis.Wave
import Util.TypeDefs


/*
Name: wave
Input: Harmonics, Amplitudes, Frequency, Number of samples
Output: Generated wave
Used in Generate.dcl, with interface function.
*/
wave :: [Real] [Real] !Frequency !Samples -> [Real]
