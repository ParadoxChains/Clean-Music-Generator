definition module Effects.Reverb
import StdEnv
import Util.TypeDef, Util.TimeUtils


generateReverbSeconds :: [Real] Int Real Real Int [Real] -> [Real]
generateReverb :: [Real] Int Beat TimeSignature Tempo Real Int [Real] -> [Real]
