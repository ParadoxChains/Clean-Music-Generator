implementation module Synthesis.Separation
import StdEnv
import Util.Constants



/*--------------------------------------------
Separation takes list of reals the sound that should be
modified into two signals for left and right ears
----------------------------------------------*/

separation :: [Real] !Real -> ([Real],[Real])
separation orig_samples amt = result
where

  c_dist =sqrt( 2.0*(HALF_HEAD^2.0) - 2.0*(HALF_HEAD^2.0)*cos( (1.0-amt) * 90.0))
  b_dist = sqrt( 2.0*(HALF_HEAD^2.0) - 2.0*(HALF_HEAD^2)*cos( (1.0+amt) * 90.0))
  c_loudness = 0.5 + 0.5*(1.0- (  b_dist/HALF_HEAD ) )
  c_phase_off = toReal(SAMPLING_RATE) * (c_dist/SOUND_SPEED)

  b_loudness = 0.5 + 0.5*(1.0 - (  c_dist/HALF_HEAD.75 ) )
  b_phase_off = toReal(SAMPLING_RATE) * (b_dist/SOUND_SPEED)
  c_samples =  repeatn (toInt((c_phase_off - (min c_phase_off b_phase_off)))) 0.0 ++ (map (\x = x*c_loudness) orig_samples)

  b_samples =  repeatn (toInt((b_phase_off - (min c_phase_off b_phase_off)))) 0.0 ++ (map (\x = x*b_loudness) orig_samples)
  result = (c_samples, b_samples)