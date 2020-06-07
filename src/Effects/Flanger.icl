implementation module Flanger

import StdEnv
import Util.Constants



flanger :: [Real] Int Int Real -> [Real]
flanger wave delay range sweep_freq = [wave!!i + wave!!(i + delay + toInt (offset)) \\ i <- [0..((length wave) - delay - range)]]
where
    offset = toReal(range) * sin( (2.0*PI*sweep_freq) / toReal(SAMPLING_RATE) )
