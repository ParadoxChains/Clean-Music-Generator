definition module Synthesis.Generate
import Synthesis.Wave
import Util.TypeDefs


/*
Name: generate
Input: Wave type, Frequency, Number of samples
Output: Generated wave
Interface for 'wave' function from Generate.dcl
*/
generate :: Wave Frequency Samples -> [Real]
