definition module Synthesis.Seperation
import StdEnv
import Util.Constants


/*--------------------------------------------
Seperation takes list of reals the sound that should be
modified into two signals for left and right ears
----------------------------------------------*/
seperation :: [Real] !Real -> ([Real],[Real])