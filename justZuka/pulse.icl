module pulse
import StdEnv
import constants
import accesstable
import sinewave
import rand
import utils
import sawtooth


sawWave = generateSawTooth
// shifted = phaseShift sawWave (SAMPLING_RATE/(4*freq))

// Start = sawWave
// Start = length (sawWave)
sineWave = generateSine 1.0

// Start = length sineWave