implementation module Synthesis.CasioEnvelope
import StdEnv
import Util.TimeUtils
import Util.Constants

getCasioCZ :: Real CasioCZ -> [Real]
getCasioCZ noteDur casio = shortenedEnv ++ releaseEnv // Release
where
    delaySamples = secondsToSamples casio.delayCZ
	attack1Samples = (secondsToSamples (casio.attack1CZ+casio.delayCZ))
	                  - delaySamples
	holdSamples = (secondsToSamples (casio.attack1CZ+casio.delayCZ+casio.holdCZ))
	               - delaySamples - attack1Samples
	decay1Samples = (secondsToSamples (casio.attack1CZ+casio.delayCZ+casio.holdCZ+casio.decay1CZ))
	                 - attack1Samples - delaySamples - holdSamples
	decay2Samples = (secondsToSamples (casio.attack1CZ+casio.delayCZ+casio.holdCZ+casio.decay1CZ+casio.decay2CZ))
	                 - attack1Samples - delaySamples - holdSamples - decay1Samples
	decay3Samples = (secondsToSamples (casio.attack1CZ+casio.delayCZ+casio.holdCZ+casio.decay1CZ+casio.decay2CZ+casio.decay3CZ))
	                 - attack1Samples - delaySamples - holdSamples - decay1Samples - decay2Samples
	attack2Samples = (secondsToSamples (casio.attack1CZ+casio.delayCZ+casio.holdCZ+casio.decay1CZ+casio.decay2CZ+casio.decay3CZ+casio.attack2CZ))
	                  - attack1Samples - delaySamples - holdSamples - decay1Samples - decay2Samples - decay3Samples
	sustainSamples = (secondsToSamples noteDur)
	                  - attack1Samples - delaySamples - holdSamples - decay1Samples - decay2Samples - decay3Samples - attack2Samples
	wholeEnv = [0.0 \\ x <- [1,2..delaySamples]] // Delay
	           ++ [1.0*((toReal x)/(casio.attack1CZ * (toReal SAMPLING_RATE))) \\ x <- [1,2..attack1Samples]] // 1st Attack
	           ++ [1.0 \\ x <- [1,2..holdSamples]] // Hold
	           ++ [1.0-((1.0-casio.breakpoint1CZ)*((toReal x)/(casio.decay1CZ * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decay1Samples]] // 1st Decay
	           ++ [casio.breakpoint1CZ-((casio.breakpoint1CZ-casio.breakpoint2CZ)*((toReal x)/(casio.decay2CZ * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decay2Samples]] // 2nd Decay
	           ++ [casio.breakpoint2CZ-((casio.breakpoint2CZ-casio.breakpoint3CZ)*((toReal x)/(casio.decay3CZ * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decay3Samples]] // 3rd Decay
	           ++ [casio.breakpoint3CZ + (casio.sustainCZ-casio.breakpoint3CZ)*((toReal x)/(casio.attack2CZ * (toReal SAMPLING_RATE))) \\ x <- [1,2..attack2Samples]] // 2nd Attack
	           ++ [casio.sustainCZ \\ x <- [1,2..sustainSamples]] // Sustain
	shortLength = (secondsToSamples noteDur)
    shortenedEnv = take shortLength wholeEnv     
    endValue | shortLength <= delaySamples = 0.0
             | shortLength <= delaySamples + attack1Samples 
               = 1.0*((toReal (shortLength-delaySamples))/(casio.attack1CZ * (toReal SAMPLING_RATE)))
             | shortLength <= delaySamples + attack1Samples + holdSamples = 1.0
             | shortLength <= delaySamples + attack1Samples + holdSamples + decay1Samples 
               = 1.0-((1.0-casio.breakpoint1CZ)*((toReal (shortLength-delaySamples-attack1Samples-holdSamples))/(casio.decay1CZ * (toReal SAMPLING_RATE))))              
	         | shortLength <= delaySamples + attack1Samples + holdSamples + decay1Samples + decay2Samples 
	           = casio.breakpoint1CZ-((casio.breakpoint1CZ-casio.breakpoint2CZ)*((toReal (shortLength-delaySamples-attack1Samples-holdSamples-decay1Samples))/(casio.decay2CZ * (toReal SAMPLING_RATE))))
	         | shortLength <= delaySamples + attack1Samples + holdSamples + decay1Samples + decay2Samples + decay3Samples 
	           = casio.breakpoint2CZ-((casio.breakpoint2CZ-casio.breakpoint3CZ)*((toReal (shortLength-delaySamples-attack1Samples-holdSamples-decay1Samples-decay2Samples))/(casio.decay3CZ * (toReal SAMPLING_RATE)))) 
	         | shortLength <= delaySamples + attack1Samples + holdSamples + decay1Samples + decay2Samples + decay3Samples + attack2Samples 
	           = casio.breakpoint3CZ + (casio.sustainCZ-casio.breakpoint3CZ)*((toReal (shortLength-delaySamples-attack1Samples-holdSamples-decay1Samples-decay2Samples-decay3Samples))/(casio.attack2CZ * (toReal SAMPLING_RATE)))
	         = casio.sustainCZ           
	releaseDecaySamples = secondsToSamples casio.releaseDecayCZ
	releaseAttackSamples = secondsToSamples casio.releaseAttackCZ
	releaseSamples = secondsToSamples casio.releaseCZ
	releaseEnv = [endValue-((endValue-casio.releaseBreakpoint1CZ)*((toReal x)/(casio.releaseDecayCZ * (toReal SAMPLING_RATE)))) \\ x <- [1,2..releaseDecaySamples]]
				 ++ [casio.releaseBreakpoint1CZ+((casio.releaseBreakpoint2CZ-casio.releaseBreakpoint1CZ)*((toReal x)/(casio.releaseAttackCZ * (toReal SAMPLING_RATE)))) \\ x <- [1,2..releaseAttackSamples]]
				 ++ [casio.releaseBreakpoint2CZ-((casio.releaseBreakpoint2CZ)*((toReal x)/(casio.releaseCZ * (toReal SAMPLING_RATE)))) \\ x <- [1,2..releaseSamples]]
	