definition module Synthesis.Accesstable
import Util.TypeDefs


// Takes wavetable, list of harmonics, list of amplitudes, frequency and number of samples
get :: {Real} [Real] [Real] Frequency Samples -> [[Real]]
