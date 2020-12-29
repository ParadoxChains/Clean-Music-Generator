implementation module Synthesis.Envelope
import StdEnv
import Util.TimeUtils
import Util.Constants


ADSRtoDAHDSR::!ADSR ->DAHDSR
ADSRtoDAHDSR adsr = {delay = 0.0, attack = adsr.att, hold = 0.0, decay = adsr.dec,
					sustain = adsr.sus, release = adsr.rel}

DAHDSRtoADSR::!DAHDSR->ADSR
DAHDSRtoADSR dahdsr = {att = dahdsr.attack,dec = dahdsr.decay,
							sus = dahdsr.sustain, rel = dahdsr.release}

getADSR :: !Beat !TimeSignature !Tempo !ADSR -> [Real]
getADSR beat time_sig tempo adsr = shortened_env ++ [end_value-(end_value*((toReal x)/(adsr.rel * (toReal SAMPLING_RATE)))) \\ x <- [1,2..release_samples]] // Release
where
	note_dur = noteToSamples beat time_sig tempo
	attack_samples = secondsToSamples adsr.att
	decay_samples = (secondsToSamples (adsr.att+adsr.dec)) - attack_samples
	sustain_samples = note_dur - attack_samples - decay_samples
	release_samples = secondsToSamples adsr.rel
	whole_env = [1.0*((toReal x)/(adsr.att * (toReal SAMPLING_RATE))) \\ x <- [1,2..attack_samples]] // Attack
	           ++ [1.0-((1.0-adsr.sus)*((toReal x)/(adsr.dec * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decay_samples]] // Decay
	           ++ [adsr.sus \\ x <- [1,2..sustain_samples]] // Sustain
	shortened_env = take note_dur whole_env
	end_value | note_dur == 0 = 0.0
			 | note_dur <= attack_samples = 1.0*((toReal (note_dur))/(adsr.att * (toReal SAMPLING_RATE)))
			 | note_dur <= attack_samples+decay_samples = 1.0-(adsr.sus*((toReal (note_dur-attack_samples))/(adsr.dec * (toReal SAMPLING_RATE))))
			 = adsr.sus

getDAHDSR :: !Beat !TimeSignature !Tempo !DAHDSR -> [Real]
getDAHDSR beat time_sig tempo dahdsr = shortened_env ++ [end_value-(end_value*((toReal x)/(dahdsr.release * (toReal SAMPLING_RATE)))) \\ x <- [1,2..release_samples]] // Release
where
    note_dur = noteToSamples beat time_sig tempo
    delay_samples = secondsToSamples dahdsr.delay
	attack_samples = (secondsToSamples (dahdsr.attack+dahdsr.delay)) - delay_samples
	hold_samples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold)) - delay_samples - attack_samples
	decay_samples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold+dahdsr.decay)) - attack_samples - delay_samples - hold_samples
	sustain_samples = note_dur - attack_samples - delay_samples - hold_samples - decay_samples
	release_samples = secondsToSamples dahdsr.release
	whole_env = [0.0 \\ x <- [1,2..delay_samples]] // Delay
	           ++ [1.0*((toReal x)/(dahdsr.attack * (toReal SAMPLING_RATE))) \\ x <- [1,2..attack_samples]] // Attack
	           ++ [1.0 \\ x <- [1,2..hold_samples]] // Hold
	           ++ [1.0-((1.0-dahdsr.sustain)*((toReal x)/(dahdsr.decay * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decay_samples]] // Decay
	           ++ [dahdsr.sustain \\ x <- [1,2..sustain_samples]] // Sustain
	shortened_env = take note_dur whole_env
	end_value | note_dur == 0 = 0.0
			 | note_dur <= delay_samples = 0.0
			 | note_dur <= delay_samples+attack_samples = 1.0*((toReal (note_dur-delay_samples))/(dahdsr.attack * (toReal SAMPLING_RATE)))
			 | note_dur <= delay_samples+attack_samples+hold_samples = 1.0
			 | note_dur <= delay_samples+attack_samples+hold_samples+decay_samples = 1.0-(dahdsr.sustain*((toReal (note_dur-delay_samples-attack_samples-hold_samples))/(dahdsr.decay * (toReal SAMPLING_RATE))))
			 = dahdsr.sustain

getLocalDAHDSR :: !Int !Beat !TimeSignature !Tempo !DAHDSR -> Real
getLocalDAHDSR index beat time_sig tempo dahdsr
| index <= delay_samples = 0.0
| index <= delay_samples+attack_samples = 1.0*((toReal (index-delay_samples))/(dahdsr.attack * (toReal SAMPLING_RATE)))
| index <= delay_samples+attack_samples+hold_samples = 1.0
| index <= decay_samples+attack_samples+hold_samples+decay_samples = 1.0-((1.0-dahdsr.sustain)*((toReal (index-(delay_samples+attack_samples+hold_samples)))/(dahdsr.decay * (toReal SAMPLING_RATE))))
| index <= note_dur = dahdsr.sustain
| index <= note_dur+release_samples = end_value-(end_value*((toReal (index - note_dur))/(dahdsr.release * (toReal SAMPLING_RATE))))
= 0.0
where
    note_dur = noteToSamples beat time_sig tempo
    delay_samples = secondsToSamples dahdsr.delay
	attack_samples = (secondsToSamples (dahdsr.attack+dahdsr.delay)) - delay_samples
	hold_samples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold)) - delay_samples - attack_samples
	decay_samples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold+dahdsr.decay)) - attack_samples - delay_samples - hold_samples
	sustain_samples = note_dur - attack_samples - delay_samples - hold_samples - decay_samples
	release_samples = secondsToSamples dahdsr.release
	end_value | note_dur == 0 = 0.0
			 | note_dur <= delay_samples = 0.0
			 | note_dur <= delay_samples+attack_samples = 1.0*((toReal (note_dur-delay_samples))/(dahdsr.attack * (toReal SAMPLING_RATE)))
			 | note_dur <= delay_samples+attack_samples+hold_samples = 1.0
			 | note_dur <= delay_samples+attack_samples+hold_samples+decay_samples = 1.0-(dahdsr.sustain*((toReal (note_dur-delay_samples-attack_samples-hold_samples))/(dahdsr.decay * (toReal SAMPLING_RATE))))
			 = dahdsr.sustain

applyEnvelope :: [Real] [Real] -> [Real]
applyEnvelope wave envelope = [x*e \\ x <- wave & e <- envelope]
