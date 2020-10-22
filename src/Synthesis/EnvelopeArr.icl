implementation module Synthesis.EnvelopeArr
import StdEnv
import Util.TimeUtils
import Util.Constants
import Util.ArrayUtils


ADSRtoDAHDSR::ADSR ->DAHDSR
ADSRtoDAHDSR adsr = {delay = 0.0, attack = adsr.att, hold = 0.0, decay = adsr.dec,
					 sustain = adsr.sus, release = adsr.rel}

DAHDSRtoADSR::DAHDSR -> ADSR
DAHDSRtoADSR dahdsr = {att = dahdsr.attack,dec = dahdsr.decay,
					   sus = dahdsr.sustain, rel = dahdsr.release}

getIndexArr :: Int -> {Int}
getIndexArr n = arrSeq (1,n,1)

getADSR :: Beat TimeSignature Tempo ADSR -> {Real}
getADSR beat time_sig tempo adsr = addArr shortened_env {end_value-(end_value*((toReal x)/(adsr.rel * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr release_samples)} // Release
where
    note_dur = noteToSamples beat time_sig tempo
	attack_samples = min (note_dur) (secondsToSamples adsr.att)
	decay_samples = (min (note_dur) (secondsToSamples (adsr.att+adsr.dec))) - attack_samples
	sustain_samples = note_dur - attack_samples - decay_samples
	release_samples = secondsToSamples adsr.rel
	half_env = (addArr {1.0*((toReal x)/(adsr.att * (toReal SAMPLING_RATE))) \\ x <-: (getIndexArr attack_samples)} // Attack
	                  {1.0-(adsr.sus*((toReal x)/(adsr.dec * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr decay_samples)})// Decay
	whole_env = addArr (addArr {1.0*((toReal x)/(adsr.att * (toReal SAMPLING_RATE))) \\ x <-: (getIndexArr attack_samples)} // Attack
	                          {1.0-(adsr.sus*((toReal x)/(adsr.dec * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr decay_samples)})// Decay
	                   {adsr.sus \\ x <-: (getIndexArr sustain_samples)} // Sustain
	shortened_env = takeArr note_dur whole_env
	end_value | note_dur == 0 = 0.0
			 | note_dur <= attack_samples = 1.0*((toReal (note_dur))/(adsr.att * (toReal SAMPLING_RATE)))
			 | note_dur <= attack_samples+decay_samples = 1.0-(adsr.sus*((toReal (note_dur-attack_samples))/(adsr.dec * (toReal SAMPLING_RATE))))
			 = adsr.sus

getDAHDSR :: Beat TimeSignature Tempo DAHDSR -> {Real}
getDAHDSR beat time_sig tempo dahdsr = addArr shortened_env {end_value-(end_value*((toReal x)/(dahdsr.release * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr release_samples)} // Release
where
    note_dur = noteToSamples beat time_sig tempo
    delaySamples = min (note_dur) (secondsToSamples dahdsr.delay)
	attack_samples = (min (note_dur) (secondsToSamples (dahdsr.attack+dahdsr.delay))) - delaySamples
	holdSamples = (min (note_dur) (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold))) - delaySamples - attack_samples
	decay_samples = (min (note_dur) (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold+dahdsr.decay))) - attack_samples - delaySamples - holdSamples
	sustain_samples = (note_dur) - attack_samples - delaySamples - holdSamples - decay_samples
	release_samples = secondsToSamples dahdsr.release
	whole_env = addArr (addArr {0.0 \\ x <-: (getIndexArr delaySamples)} // Delay
	                           {1.0*((toReal x)/(dahdsr.attack * (toReal SAMPLING_RATE))) \\ x <-: (getIndexArr attack_samples)}) // Attack
	                  (addArr {1.0 \\ x <-: (getIndexArr holdSamples)} // Hold
	                          (addArr {1.0-(dahdsr.sustain*((toReal x)/(dahdsr.decay * (toReal SAMPLING_RATE)))) \\ x <-: (getIndexArr decay_samples)} // Decay
	                                  {dahdsr.sustain \\ x <-: (getIndexArr sustain_samples)})) // Sustain
	shortened_env = takeArr note_dur whole_env
	end_value | note_dur == 0 = 0.0
			 | note_dur <= delaySamples = 0.0
			 | note_dur <= delaySamples+attack_samples = 1.0*((toReal (note_dur-delaySamples))/(dahdsr.attack * (toReal SAMPLING_RATE)))
			 | note_dur <= delaySamples+attack_samples+holdSamples = 1.0
			 | note_dur <= delaySamples+attack_samples+holdSamples+decay_samples = 1.0-(dahdsr.sustain*((toReal (note_dur-delaySamples-attack_samples-holdSamples))/(dahdsr.decay * (toReal SAMPLING_RATE))))
			 = dahdsr.sustain

applyEnvelope :: [Real] {Real} -> {Real}
applyEnvelope wave envelope = {x*e \\ x <- wave & e <-: envelope}
