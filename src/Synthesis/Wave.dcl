definition module Synthesis.Wave
import Util.TypeDefs


:: Wave = Sine | Square | Triangle | Noise | Pulse | Sawtooth | Silence

// takes harmonics, amplitudes, frequency and duration as parameter and generates wave
wave :: [Real] [Real] Frequency Int -> [Real] 


