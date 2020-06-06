implementation module Flanger

import StdEnv
import Util.Constants

// Controls
A = 10
rate = 0.2
manual = 10
period = 1.0 / toReal(SAMPLING_RATE)

flanger :: [Real] -> [Real]
flanger wave = [wave!!t + wave!!(t + manual + toInt((toReal A) * sin(2.0*pi*(toReal t)*rate*period)))/2.0 
                    \\ t <- [0..(length wave - A - manual-1)]]
