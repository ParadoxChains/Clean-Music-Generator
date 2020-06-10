definition module Effects.ApplyEffects


import StdEnv
import Util.Constants
import Synthesis.Wave
import Util.TypeDefs
import Effects.Chorus
import Effects.Delay
import Effects.Flanger

applyEffects :: Wave [Effect] -> Wave

applyEffect Wave Effect -> Wave