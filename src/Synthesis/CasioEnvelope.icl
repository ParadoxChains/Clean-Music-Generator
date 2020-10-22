implementation module Synthesis.CasioEnvelope
import StdEnv
import Util.TimeUtils
import Util.Constants
import Util.ListUtils


getCasioCZ :: Real CasioCZ -> [Real]
getCasioCZ note_dur casio = shortened_env ++ release_env
where
    note_samples = secondsToSamples note_dur
    rt1 | 0.0 > casio.level1 = ~(casio.rate1 / (toReal SAMPLING_RATE))
	    = (casio.rate1 / (toReal SAMPLING_RATE))
	rt2 | casio.level1 > casio.level2 = ~(casio.rate2 / (toReal SAMPLING_RATE))
	    = (casio.rate2 / (toReal SAMPLING_RATE))
	rt3 | casio.level2 > casio.level3 = ~(casio.rate3 / (toReal SAMPLING_RATE))
	    = (casio.rate3 / (toReal SAMPLING_RATE))
	rt4 | casio.level3 > casio.level4 = ~(casio.rate4 / (toReal SAMPLING_RATE))
	    = (casio.rate4 / (toReal SAMPLING_RATE))
	rt5 | casio.level4 > casio.level5 = ~(casio.rate5 / (toReal SAMPLING_RATE))
	    = (casio.rate5 / (toReal SAMPLING_RATE))
    line1 = generateLine rt1 0.0 casio.level1
    line2 = generateLine rt2 (casio.level1-rt2*(snd line1)) casio.level2
    line3 = generateLine rt3 (casio.level2-rt3*(snd line2)) casio.level3
    line4 = generateLine rt4 (casio.level3-rt4*(snd line3)) casio.level4
    line5 = generateLine rt5 (casio.level4-rt5*(snd line4)) casio.level5
    front_env = (fst line1) ++ (fst line2) ++ (fst line3) ++ (fst line4) ++ (fst line5)
	sustain = [casio.level5 \\ x <-[1,2..(note_samples-(length front_env))]]
	shortened_env = take note_samples (front_env ++ sustain)
	offset = last shortened_env
	rt6 | offset > casio.level6 = ~(casio.rate6 / (toReal SAMPLING_RATE))
	    = (casio.rate6 / (toReal SAMPLING_RATE))
	rt7 | casio.level6 > casio.level7 = ~(casio.rate7 / (toReal SAMPLING_RATE))
	    = (casio.rate7 / (toReal SAMPLING_RATE))
	rt8 | casio.level7 > casio.level8 = ~(casio.rate8 / (toReal SAMPLING_RATE))
	    = (casio.rate8 / (toReal SAMPLING_RATE))
	line6 = generateLine rt6 offset casio.level6
	line7 = generateLine rt7 (casio.level6-rt7*(snd line6)) casio.level7
	line8 = generateLine rt8 (casio.level7-rt8*(snd line7)) casio.level8
	release_env = (fst line6) ++ (fst line7) ++ (fst line8)


generateLine :: Real Real Real -> ([Real], Real)
generateLine rt level1 level2 = (line, lst)
where
	line = [level1+rt,level1+2.0*rt..level2]
	llst = level2 - ((level2-level1)-rt*(toReal(floor ((level2-level1)/rt))))
	lst | llst == level2 = 0.0
		= ((level2-llst)/rt)
