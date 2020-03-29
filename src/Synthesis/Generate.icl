implementation module Synthesis.Generate
import StdEnv
import Util.Constants
import Synthesis.Accesstable
import Synthesis.Wavetable
import Util.ListUtils
import Synthesis.Wave
import Util.Rand
import Util.TypeDefs
import Util.ArrayUtils


// generating random numbers

randoms = map (\x = x rem 40) (take 100 (genRandInt (hd (genRandInt 1))))


// noise wave characteristics
noise_harmonics = map (\x = x * 36.0) (take 100 (genRandReal 1))
noise_amplitudes = repeatn 100 1.0

// takes Wave type as parameter and gives appropriate tuple of harmonics and amplitudes.
harmonics_amplitudes :: Wave -> ([Real], [Real])
harmonics_amplitudes Sine = ([1.0], [1.0])
harmonics_amplitudes Sawtooth = ([1.0,2.0..50.0],[ (-1.0)^(k+1.0) * (1.0 / k) \\ k <- [1.0,2.0..50.0]])
harmonics_amplitudes Square = ([1.0,3.0..100.0], [1.0 / x \\ x <- [1.0,3.0..100.0]])
harmonics_amplitudes Triangle = ([1.0,3.0..100.0], [ (-1.0)^(i + 1.0) * (1.0/(k^2.0)) \\ k <- [1.0,3.0..100.0] & i <- [1.0..] ])

generate :: Wave Frequency Samples -> [Real]
generate Noise freq dur = sumAll l
where 
    l = [shiftLeft list i \\ list <- (get (wavetable 1.0) noise_harmonics noise_amplitudes freq dur) & i <- randoms]
generate Pulse freq dur = subtractLists (shiftLeft saw (SAMPLING_RATE/(2*(toInt freq)))) saw
where
    saw = generate Sawtooth freq dur
generate Silence _ dur = [abs(x*0.0)\\x<-(wave [1.0] [0.0] 440.00 dur)]
generate wavetype freq dur = wave harmonics amplitudes freq dur
where
    (harmonics, amplitudes) = harmonics_amplitudes wavetype


