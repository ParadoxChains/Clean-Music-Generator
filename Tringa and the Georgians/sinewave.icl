implementation module sinewave
import StdEnv
import constants
import accesstable


generateSine :: Real -> [Real]
generateSine ampl = [ampl * sin(0.0 + i * 2.0 * PI /offset) \\ i<-[0.0,1.0..(offset - 1.0)]]
where
    offset = toReal(SAMPLING_RATE/I_DONT_KNOW_WHAT_ITS_CALLED)


// Start = getValues (generateSine 1.0) 440 3
