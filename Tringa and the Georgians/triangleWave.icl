implementation module triangleWave
import StdEnv
import constants
import accesstable


generateSquare :: Real -> [Real]
generateSquare ampl = [4.0/PI*(sum  [(-1.0)^k * (sin(2.0*PI*k*t/offset) / k) \\ k <- [1.0,3.0..100.0]])  \\ t <- [0.0,1.0..(offset - 1.0)]] 
where
    offset = toReal(SAMPLING_RATE/I_DONT_KNOW_WHAT_ITS_CALLED)
//Start = generateSquare 1.0


generateTriangle :: Real -> [Real]
generateTriangle ampl = [4.0/P*(t-P/2.0*(toReal (floor (2.0*t/P + 1.0/2.0))))*((-1.0)^(toReal (floor(2.0*t/P+1.0/2.0)))) \\ t <- [0.0,1.0..(offset - 1.0)] ]
where
	offset = toReal(SAMPLING_RATE/I_DONT_KNOW_WHAT_ITS_CALLED)
	P = offset
//Start = generateTriangle 1.0
