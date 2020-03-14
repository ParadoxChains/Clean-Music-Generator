implementation module Synthesis.Envelope
import StdEnv
import Util.TimeUtils
import Util.Constants

ADSRtoDAHDSR::ADSR ->DAHDSR
ADSRtoDAHDSR adsr = {delay = 0.0, attack = adsr.att, hold = 0.0, decay = adsr.dec,
					sustain = adsr.sus, release = adsr.rel}
					
DAHDSRtoADSR::DAHDSR->ADSR
DAHDSRtoADSR dahdsr = {att = dahdsr.attack,dec = dahdsr.decay, 
							sus = dahdsr.sustain, rel = dahdsr.release}

getADSR :: Beat TimeSignature Tempo ADSR -> [Real]
getADSR beat timeSig tempo adsr = shortenedEnv ++ [endValue-(endValue*((toReal x)/(adsr.rel * (toReal SAMPLING_RATE)))) \\ x <- [1,2..releaseSamples]] // Release
where
	noteDur = noteToSamples beat timeSig tempo
	attackSamples = secondsToSamples adsr.att
	decaySamples = (secondsToSamples (adsr.att+adsr.dec)) - attackSamples
	sustainSamples = noteDur - attackSamples - decaySamples
	releaseSamples = secondsToSamples adsr.rel
	wholeEnv = [1.0*((toReal x)/(adsr.att * (toReal SAMPLING_RATE))) \\ x <- [1,2..attackSamples]] // Attack
	           ++ [1.0-((1.0-adsr.sus)*((toReal x)/(adsr.dec * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decaySamples]] // Decay
	           ++ [adsr.sus \\ x <- [1,2..sustainSamples]] // Sustain
	shortenedEnv = take noteDur wholeEnv
	endValue | noteDur == 0 = 0.0
			 | noteDur <= attackSamples = 1.0*((toReal (noteDur))/(adsr.att * (toReal SAMPLING_RATE)))
			 | noteDur <= attackSamples+decaySamples = 1.0-(adsr.sus*((toReal (noteDur-attackSamples))/(adsr.dec * (toReal SAMPLING_RATE))))
			 = adsr.sus  

getDAHDSR :: Beat TimeSignature Tempo DAHDSR -> [Real]
getDAHDSR beat timeSig tempo dahdsr = shortenedEnv ++ [endValue-(endValue*((toReal x)/(dahdsr.release * (toReal SAMPLING_RATE)))) \\ x <- [1,2..releaseSamples]] // Release
where
    noteDur = noteToSamples beat timeSig tempo
    delaySamples = secondsToSamples dahdsr.delay
	attackSamples = (secondsToSamples (dahdsr.attack+dahdsr.delay)) - delaySamples
	holdSamples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold)) - delaySamples - attackSamples
	decaySamples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold+dahdsr.decay)) - attackSamples - delaySamples - holdSamples
	sustainSamples = noteDur - attackSamples - delaySamples - holdSamples - decaySamples
	releaseSamples = secondsToSamples dahdsr.release
	wholeEnv = [0.0 \\ x <- [1,2..delaySamples]] // Delay
	           ++ [1.0*((toReal x)/(dahdsr.attack * (toReal SAMPLING_RATE))) \\ x <- [1,2..attackSamples]] // Attack
	           ++ [1.0 \\ x <- [1,2..holdSamples]] // Hold
	           ++ [1.0-((1.0-dahdsr.sustain)*((toReal x)/(dahdsr.decay * (toReal SAMPLING_RATE)))) \\ x <- [1,2..decaySamples]] // Decay
	           ++ [dahdsr.sustain \\ x <- [1,2..sustainSamples]] // Sustain
	shortenedEnv = take noteDur wholeEnv
	endValue | noteDur == 0 = 0.0
			 | noteDur <= delaySamples = 0.0
			 | noteDur <= delaySamples+attackSamples = 1.0*((toReal (noteDur-delaySamples))/(dahdsr.attack * (toReal SAMPLING_RATE)))
			 | noteDur <= delaySamples+attackSamples+holdSamples = 1.0
			 | noteDur <= delaySamples+attackSamples+holdSamples+decaySamples = 1.0-(dahdsr.sustain*((toReal (noteDur-delaySamples-attackSamples-holdSamples))/(dahdsr.decay * (toReal SAMPLING_RATE))))
			 = dahdsr.sustain

getLocalDAHDSR :: Int Beat TimeSignature Tempo DAHDSR -> Real
getLocalDAHDSR index beat timeSig tempo dahdsr
| index <= delaySamples = 0.0
| index <= delaySamples+attackSamples = 1.0*((toReal (index-delaySamples))/(dahdsr.attack * (toReal SAMPLING_RATE)))
| index <= delaySamples+attackSamples+holdSamples = 1.0
| index <= decaySamples+attackSamples+holdSamples+decaySamples = 1.0-((1.0-dahdsr.sustain)*((toReal (index-(delaySamples+attackSamples+holdSamples)))/(dahdsr.decay * (toReal SAMPLING_RATE))))
| index <= noteDur = dahdsr.sustain
| index <= noteDur+releaseSamples = endValue-(endValue*((toReal (index - noteDur))/(dahdsr.release * (toReal SAMPLING_RATE))))
= 0.0
where
    noteDur = noteToSamples beat timeSig tempo
    delaySamples = secondsToSamples dahdsr.delay
	attackSamples = (secondsToSamples (dahdsr.attack+dahdsr.delay)) - delaySamples
	holdSamples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold)) - delaySamples - attackSamples
	decaySamples = (secondsToSamples (dahdsr.attack+dahdsr.delay+dahdsr.hold+dahdsr.decay)) - attackSamples - delaySamples - holdSamples
	sustainSamples = noteDur - attackSamples - delaySamples - holdSamples - decaySamples
	releaseSamples = secondsToSamples dahdsr.release
	endValue | noteDur == 0 = 0.0
			 | noteDur <= delaySamples = 0.0
			 | noteDur <= delaySamples+attackSamples = 1.0*((toReal (noteDur-delaySamples))/(dahdsr.attack * (toReal SAMPLING_RATE)))
			 | noteDur <= delaySamples+attackSamples+holdSamples = 1.0
			 | noteDur <= delaySamples+attackSamples+holdSamples+decaySamples = 1.0-(dahdsr.sustain*((toReal (noteDur-delaySamples-attackSamples-holdSamples))/(dahdsr.decay * (toReal SAMPLING_RATE))))
			 = dahdsr.sustain

applyEnvelope :: [Real] [Real] -> [Real]
applyEnvelope wave envelope = [x*e \\ x <- wave & e <- envelope]
