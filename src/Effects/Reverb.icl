implementation module Effects.Reverb
import StdEnv
import Util.TypeDef,Util.TimeUtils


//Works with seconds

resultForPlotSeconds:: [Real] Int  Real Real->[(Real,Real)]
resultForPlotSeconds amp_val bounces seconds decay = [(toReal x,y) \\ y<-y_vals & x<-[0..]]
where
    y_vals = generateReverbSeconds amp_val bounces seconds decay 1 amp_val

generateReverbSeconds :: [Real] Int Real Real Int [Real] -> [Real]
generateReverbSeconds original bounces seconds decay nth_bounce results
| nth_bounce > bounces = results
= generateReverbSeconds original bounces seconds decay (nth_bounce + 1) new_result
where
    delay=secondsToSamples seconds
    decayed_signal = map (\x = x * (decay^(toReal nth_bounce))) original
    decay_silence = repeatn (nth_bounce * delay) 0.0
    delayed_decayed_signal = decay_silence ++ decayed_signal
    extended_results = results ++ decay_silence
    new_result = [a+b \\ a <- extended_results & b<-delayed_decayed_signal]

createList::Real->[Real]
createList a= [toReal(i)*0.05\\i<-[0..toInt(a)*20]]
            ++reverse [toReal(i)*0.05\\i<-[1..toInt(a)*20]]
            ++[toReal(i)*(-0.05)\\i<-[0..toInt(a)*20]]
            ++reverse[toReal(i)*(-0.05)\\i<-[1..toInt(a)*20]]
//repeating createList n number of times
makeBigList:: Real Int->[Real]
makeBigList _ 0=[]
makeBigList num cnt=(createList num)++ makeBigList num (cnt-1)
//Tryout with beats

resultForPlot:: [Real] Int  Beat TimeSignature Tempo Real->[(Real,Real)]
resultForPlot amp_val bounces beat ts tempo decay = [(toReal x,y) \\ y<-y_vals & x<-[0..]]
where
    y_vals = generateReverb amp_val bounces beat ts  tempo decay 1 amp_val

generateReverb :: [Real] Int Beat TimeSignature Tempo Real Int [Real] -> [Real]
generateReverb original bounces beat ts  tempo decay nth_bounce results
| nth_bounce > bounces = results
= generateReverb original bounces beat ts tempo decay (nth_bounce + 1) new_result
where
    delay=noteToSamples beat ts tempo
    decayed_signal = map (\x = x * (decay^(toReal nth_bounce))) original
    decay_silence = repeatn (nth_bounce * delay) 0.0
    delayed_decayed_signal = decay_silence ++ decayed_signal
    extended_results = results ++ decay_silence
    new_result = [a+b \\ a <- extended_results & b<-delayed_decayed_signal]
