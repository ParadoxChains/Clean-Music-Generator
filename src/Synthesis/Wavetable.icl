implementation module Synthesis.Wavetable
import StdEnv
import Util.Constants
import Synthesis.Accesstable


// Takes amplitude as parameter and generates Sine wave table.
wavetable :: Real -> {Real}
wavetable ampl = {ampl * sin(0.0 + i * 2.0 * PI /offset) \\ i<-[0.0,1.0..(offset - 1.0)]}
where
    offset = toReal(tableSize)



