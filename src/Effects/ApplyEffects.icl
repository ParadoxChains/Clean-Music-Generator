implementational module Effects.ApplyEffects

import StdEnv
import Util.Constants
import Synthesis.Wave
import Util.TypeDefs
import Effects.Chorus
import Effects.Delay
import Effects.Flanger

applyEffects :: Wave [(Effect, Parameters)] -> Wave
applyEffects wave [] = wave
applyEffects wave [(effect, parameters): effects] = applyEffects (applyEffect wave effect parameters) effects

applyEffect :: Wave Effect Parameters -> Wave
applyEffect wave Chorus parameters = applyChorus wave parameters
applyEffect wave Flanger parameters = applyFlanger wave parameters
applyEffect wave Delay parameters = applyDelay wave parameters
