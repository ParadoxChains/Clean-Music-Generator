implementation module Effects.Flanger

import StdEnv
import Util.Constants
import Synthesis.Wave
import Util.TypeDefs


applyFlanger :: Wave FlangerParameters -> Wave
applyFlanger wave (A, rate, manual) = [wave!!t + wave!!(t + manual + toInt((toReal A) * sin(2.0*pi*(toReal t)*rate*period)))/2.0 
                        \\ t <- [0..(length wave - A - manual-1)]]
