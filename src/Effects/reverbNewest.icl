implementation module Effects.reverbNewest
import StdEnv
import Util.TypeDef,Util.TimeUtils
//Works with seconds

resultForPlotSeconds:: [Real] Int  Real Real->[(Real,Real)]
resultForPlotSeconds ampVal bounces seconds decay = [(toReal x,y) \\ y<-yVals & x<-[0..]]
where
	yVals = generateReverbSeconds ampVal bounces seconds decay 1 ampVal

generateReverbSeconds :: [Real] Int Real Real Int [Real] -> [Real]
generateReverbSeconds original bounces seconds decay nthBounce results
| nthBounce > bounces = results
= generateReverbSeconds original bounces seconds decay (nthBounce + 1) newResult
where
	delay=secondsToSamples seconds
	decayedSignal = map (\x = x * (decay^(toReal nthBounce))) original
	decaySilence = repeatn (nthBounce * delay) 0.0
	delayedDecayedSignal = decaySilence ++ decayedSignal
	extendedResults = results ++ decaySilence
	newResult = [a+b \\ a <- extendedResults & b<-delayedDecayedSignal]

//Start=resultForPlotSeconds (makeBigList 2.0 2) 1 0.4 0.5 
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
resultForPlot ampVal bounces beat ts tempo decay = [(toReal x,y) \\ y<-yVals & x<-[0..]]
where
	yVals = generateReverb ampVal bounces beat ts  tempo decay 1 ampVal

generateReverb :: [Real] Int Beat TimeSignature Tempo Real Int [Real] -> [Real]
generateReverb original bounces beat ts  tempo decay nthBounce results
| nthBounce > bounces = results
= generateReverb original bounces beat ts tempo decay (nthBounce + 1) newResult
where
	delay=noteToSamples beat ts tempo
	decayedSignal = map (\x = x * (decay^(toReal nthBounce))) original
	decaySilence = repeatn (nthBounce * delay) 0.0
	delayedDecayedSignal = decaySilence ++ decayedSignal
	extendedResults = results ++ decaySilence
	newResult = [a+b \\ a <- extendedResults & b<-delayedDecayedSignal]
//Start=resultForPlot (makeBigList 2.0 2) 2 {p=1,q=4} {barVal=4,noteVal=4} 120.0 0.5 
