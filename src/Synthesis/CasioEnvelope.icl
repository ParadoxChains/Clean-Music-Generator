implementation module Synthesis.CasioEnvelope
import StdEnv
import Util.TimeUtils
import Util.Constants

getCasioCZ :: Real CasioCZ -> [Real]
getCasioCZ noteDur casio = shortenedEnv ++ releaseEnv
where
    noteSamples = secondsToSamples noteDur
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
    frontEnv = (fst line1) ++ (fst line2) ++ (fst line3) ++ (fst line4) ++ (fst line5)
	sustain = [casio.level5 \\ x <-[1,2..(noteSamples-(length frontEnv))]]
	shortenedEnv = take noteSamples (frontEnv ++ sustain)
	offset = last shortenedEnv
	rt6 | offset > casio.level6 = ~(casio.rate6 / (toReal SAMPLING_RATE)) 
	    = (casio.rate6 / (toReal SAMPLING_RATE))
	rt7 | casio.level6 > casio.level7 = ~(casio.rate7 / (toReal SAMPLING_RATE)) 
	    = (casio.rate7 / (toReal SAMPLING_RATE))
	rt8 | casio.level7 > casio.level8 = ~(casio.rate8 / (toReal SAMPLING_RATE)) 
	    = (casio.rate8 / (toReal SAMPLING_RATE)) 
	line6 = generateLine rt6 offset casio.level6
	line7 = generateLine rt7 (casio.level6-rt7*(snd line6)) casio.level7
	line8 = generateLine rt8 (casio.level7-rt8*(snd line7)) casio.level8
	releaseEnv = (fst line6) ++ (fst line7) ++ (fst line8)
	
	
generateLine :: Real Real Real -> ([Real], Real)
generateLine rt level1 level2 = (line, lst)
where
	line = [level1+rt,level1+2.0*rt..level2]
	llst = last line
	lst | llst == level2 = 0.0
		= ((level2-llst)/rt)

/*Start = getCasioCZ 10.0025 {rate1 = 0.5, level1 = 0.9
						  ,rate2 = 0.1, level2 = 0.6
						  ,rate3 = 0.2, level3 = 0.7
						  ,rate4 = 0.1, level4 = 0.65
						  ,rate5 = 0.4, level5 = 0.4
						  ,rate6 = 0.3, level6 = 0.8
						  ,rate7 = 0.3, level7 = 0.4
						  ,rate8 = 0.1, level8 = 0.0
						  }*/