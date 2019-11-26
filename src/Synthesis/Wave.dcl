definition module Synthesis.Wave


:: Wave = Square | Triangle | Noise | Pulse | Sawtooth

// takes harmonics, amplitudes, frequency and duration as parameter and generates wave
wave :: [Real] [Real] Int Int -> [Real] 


