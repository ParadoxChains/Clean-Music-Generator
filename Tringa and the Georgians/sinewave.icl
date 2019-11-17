implementation module sinewave
import StdEnv
import constants
import accesstable
import sinewave


// Takes amplitude as parameter and generates Sine wave table.
generateSine :: Real -> [Real]
generateSine ampl = [ampl * sin(0.0 + i * 2.0 * PI /offset) \\ i<-[0.0,1.0..(offset - 1.0)]]
where
    offset = toReal(tableSize)


