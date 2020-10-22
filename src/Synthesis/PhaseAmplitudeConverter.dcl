definition module Synthesis.PhaseAmplitudeConverter
import StdEnv
import Util.TypeDefs


/*
Name: generateLocal
Arguments: localTime - Int
           waveType - Wave
           freq - Frequency
Output: sampleOut - Real

Takes an Int representing local position of a note to be rendered,
the wave type to be rendered, and the frequency to render.
Returns a single Real value at that position.
*/
generateLocal :: Int Wave Frequency -> Real
