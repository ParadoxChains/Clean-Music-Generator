module pulse
import StdEnv
import constants
import accesstable
import sinewave
import rand
import utils
import sawtooth


// TODO:
// Figure out why it gives Heap Full.
// Split into definition and implementation modules

sawWave = generateSawTooth
// shifted = phaseShift sawWave (SAMPLING_RATE/(4*freq))

// Start = sawWave
// Start = length (sawWave)
sineWave = generateSine 1.0

// Start = length sineWave