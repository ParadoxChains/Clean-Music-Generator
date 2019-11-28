implementation module Synthesis.Envelope
import StdEnv
import Util.TimeUtils
import Util.ListUtils
import Util.Constants
import Synthesis.Wavetable
import Synthesis.Generate
import Synthesis.Wave
import Util.TypeDefs
import Synthesis.Accesstable

:: ADSR = {att :: Real
          ,dec :: Real
          ,sus :: Real
          ,rel :: Real
          }

getADSR :: Real ADSR -> [Real]
getADSR noteDur adsr = shortenedEnv ++ [endValue-(endValue*((toReal x)/(adsr.rel * (toReal SAMPLING_RATE)))) \\ x <- [1,2..releaseSamples]] // Release
where
	attackSamples = secondsToSamples adsr.att
	decaySamples = (secondsToSamples (adsr.att+adsr.dec)) - attackSamples
	sustainSamples = (secondsToSamples noteDur) - attackSamples - decaySamples
	releaseSamples = secondsToSamples adsr.rel
	wholeEnv = [1.0*((toReal x)/(adsr.att * (toReal SAMPLING_RATE))) \\ x <- [1,2..attackSamples]] // Attack
	           ++ [1.0-(adsr.sus*((toReal x)/(adsr.dec * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decaySamples]] // Decay
	           ++ [adsr.sus \\ x <- [1,2..sustainSamples]] // Sustain
	shortenedEnv = take (secondsToSamples noteDur) wholeEnv
	endValue | length shortenedEnv == 0 = 0.0
			= last shortenedEnv  
			 		

:: DAHDSR = {delay :: Real
            ,attack :: Real
            ,hold :: Real
            ,decay :: Real
            ,sustain :: Real
            ,release :: Real
            }

getDAHDSR :: Real DAHDSR -> [Real]
getDAHDSR noteDur dahdsr = shortenedEnv ++ [endValue-(endValue*((toReal x)/(dahdsr.release * (toReal SAMPLING_RATE)))) \\ x <- [1,2..releaseSamples]] // Release
where
    delaySamples = secondsToSamples dahdsr.delay
	attackSamples = (secondsToSamples (dahdsr.attack+dahdsr.delay)) - delaySamples
	holdSamples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold)) - delaySamples - attackSamples
	decaySamples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold+dahdsr.decay)) - attackSamples - delaySamples - holdSamples
	sustainSamples = (secondsToSamples noteDur) - attackSamples - delaySamples - holdSamples - decaySamples
	releaseSamples = secondsToSamples dahdsr.release
	wholeEnv = [0.0 \\ x <- [1,2..delaySamples]] // Delay
	           ++ [1.0*((toReal x)/(dahdsr.attack * (toReal SAMPLING_RATE))) \\ x <- [1,2..attackSamples]] // Attack
	           ++ [1.0 \\ x <- [1,2..holdSamples]] // Hold
	           ++ [1.0-(dahdsr.sustain*((toReal x)/(dahdsr.decay * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decaySamples]] // Decay
	           ++ [dahdsr.sustain \\ x <- [1,2..sustainSamples]] // Sustain
	shortenedEnv = take (secondsToSamples noteDur) wholeEnv
	endValue | length shortenedEnv == 0 = 0.0
			 = last shortenedEnv  

applyEnvelope :: [Real] [Real] -> [Real]
applyEnvelope wave envelope = [x*e \\ x <- wave & e <- envelope]



