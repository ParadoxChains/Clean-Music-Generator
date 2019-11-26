definition module Synthesis.Wave


:: Wave = Square | Triangle | Noise | Pulse | Sawtooth

// takes harmonics, amplitudes and frequency as parameter and generates wave
wave :: [Real] [Real] Int -> [Real] 


