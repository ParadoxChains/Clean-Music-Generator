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


// Start = subtractLists shifted sawWave
// Start = sawWave
Start = length (take 2000 sawWave)
