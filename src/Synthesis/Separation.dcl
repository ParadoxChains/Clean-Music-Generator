definition module Synthesis.Separation
import StdEnv
import Util.Constants


/*--------------------------------------------
Separation takes list of reals the sound that should be
modified into two signals for left and right ears
----------------------------------------------*/
separation :: [Real] !Real -> ([Real],[Real])