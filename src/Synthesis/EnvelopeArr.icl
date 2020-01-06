implementation module Synthesis.EnvelopeArr
import StdEnv
import Util.TimeUtils
import Util.Constants
import Util.ArrayUtils

ADSRtoDAHDSR::ADSR ->DAHDSR
ADSRtoDAHDSR adsr = {delay = 0.0, attack = adsr.att, hold = 0.0, decay = adsr.dec,
					 sustain = adsr.sus, release = adsr.rel}
					
DAHDSRtoADSR::DAHDSR->ADSR
DAHDSRtoADSR dahdsr = {att = dahdsr.attack,dec = dahdsr.decay, 
					   sus = dahdsr.sustain, rel = dahdsr.release}

getIndexArr :: Int -> {Int}
getIndexArr n = arrSeq (1,n,1)

getADSR :: Beat TimeSignature Tempo ADSR -> {Real}
getADSR beat timeSig tempo adsr = addArr shortenedEnv {endValue-(endValue*((toReal x)/(adsr.rel * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr releaseSamples)} // Release
where
    noteDur = noteToSamples beat timeSig tempo
	attackSamples = min (noteDur) (secondsToSamples adsr.att)
	decaySamples = (min (noteDur) (secondsToSamples (adsr.att+adsr.dec))) - attackSamples
	sustainSamples = noteDur - attackSamples - decaySamples
	releaseSamples = secondsToSamples adsr.rel
	halfEnv = (addArr {1.0*((toReal x)/(adsr.att * (toReal SAMPLING_RATE))) \\ x <-: (getIndexArr attackSamples)} // Attack
	                  {1.0-(adsr.sus*((toReal x)/(adsr.dec * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr decaySamples)})// Decay
	wholeEnv = addArr (addArr {1.0*((toReal x)/(adsr.att * (toReal SAMPLING_RATE))) \\ x <-: (getIndexArr attackSamples)} // Attack
	                          {1.0-(adsr.sus*((toReal x)/(adsr.dec * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr decaySamples)})// Decay
	                   {adsr.sus \\ x <-: (getIndexArr sustainSamples)} // Sustain
	shortenedEnv = takeArr noteDur wholeEnv
	endValue | noteDur == 0 = 0.0
			 | noteDur <= attackSamples = 1.0*((toReal (noteDur))/(adsr.att * (toReal SAMPLING_RATE)))
			 | noteDur <= attackSamples+decaySamples = 1.0-(adsr.sus*((toReal (noteDur-attackSamples))/(adsr.dec * (toReal SAMPLING_RATE))))
			 = adsr.sus

getDAHDSR :: Beat TimeSignature Tempo DAHDSR -> {Real}
getDAHDSR beat timeSig tempo dahdsr = addArr shortenedEnv {endValue-(endValue*((toReal x)/(dahdsr.release * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr releaseSamples)} // Release
where
    noteDur = noteToSamples beat timeSig tempo
    delaySamples = min (noteDur) (secondsToSamples dahdsr.delay)
	attackSamples = (min (noteDur) (secondsToSamples (dahdsr.attack+dahdsr.delay))) - delaySamples
	holdSamples = (min (noteDur) (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold))) - delaySamples - attackSamples
	decaySamples = (min (noteDur) (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold+dahdsr.decay))) - attackSamples - delaySamples - holdSamples
	sustainSamples = (noteDur) - attackSamples - delaySamples - holdSamples - decaySamples
	releaseSamples = secondsToSamples dahdsr.release
	wholeEnv = addArr (addArr {0.0 \\ x <-: (getIndexArr delaySamples)} // Delay
	                           {1.0*((toReal x)/(dahdsr.attack * (toReal SAMPLING_RATE))) \\ x <-: (getIndexArr attackSamples)}) // Attack
	                  (addArr {1.0 \\ x <-: (getIndexArr holdSamples)} // Hold
	                          (addArr {1.0-(dahdsr.sustain*((toReal x)/(dahdsr.decay * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr decaySamples)} // Decay
	                                  {dahdsr.sustain \\ x <-: (getIndexArr sustainSamples)})) // Sustain
	shortenedEnv = takeArr noteDur wholeEnv
	endValue | noteDur == 0 = 0.0
			 | noteDur <= delaySamples = 0.0
			 | noteDur <= delaySamples+attackSamples = 1.0*((toReal (noteDur-delaySamples))/(dahdsr.attack * (toReal SAMPLING_RATE)))
			 | noteDur <= delaySamples+attackSamples+holdSamples = 1.0
			 | noteDur <= delaySamples+attackSamples+holdSamples+decaySamples = 1.0-(dahdsr.sustain*((toReal (shortLength-delaySamples-attackSamples-holdSamples))/(dahdsr.decay * (toReal SAMPLING_RATE))))
			 = dahdsr.sustain

applyEnvelope :: [Real] [Real] -> [Real]
applyEnvelope wave envelope = [x*e \\ x <- wave & e <- envelope]