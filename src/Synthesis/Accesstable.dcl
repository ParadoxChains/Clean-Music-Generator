definition module Synthesis.Accesstable
import Util.TypeDefs


// Takes wavetable, list of harmonics, list of amplitudes, frequency and duration
get :: {Real} [Real] [Real] Frequency Int -> [[Real]]
