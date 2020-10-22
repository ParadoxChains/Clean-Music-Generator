implementation module Util.DecibelConverts
import StdEnv

//convert power ratio to decibels
powerToDb::Real -> Real
powerToDb ratio
|ratio < 0.0 = abort "please write positive ratio\n"
= 10.0 * (log10 ratio)

// convert decibels to power ratio
DbToPower::Real -> Real
DbToPower x = 10.0 ^ (x/10.0)


// convert amplitude ratio to decibels
amplitudeToDb::Real -> Real
amplitudeToDb ratio = 10.0 * (log10 (ratio*ratio))

// convert decibels to amplitude ratio
DbToAmplitude::Real -> Real
DbToAmplitude x = sqrt (10.0 ^ (x/10.0))
