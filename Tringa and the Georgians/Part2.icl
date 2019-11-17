module Part2
import StdEnv


oddHarmonics= [(1.0),(3.0)..]

generateSquareAmp:: [Real]
generateSquareAmp = take 1000 [1.0/x \\x<-oddHarmonics]

//Start = generateSquareAmp

generateTriangleAmp :: [Real]
generateTriangleAmp = take 100 [(1.0/(x^2.0))*(-1.0)^y\\x<-oddHarmonics & y<-[1.0..]]
//Start = generateTriangleAmp

PI :: Real
PI = 3.1415926535898

trialSquare::[Real]
trialSquare =[(4.0/PI)*sum([(1.0/x)*sin(2.0*PI*440.0 * x*amp)\\x<-take 100 oddHarmonics])\\amp<-generateSquareAmp]

//Start =trialSquare

trialTriangle :: [Real]
trialTriangle = [PI/4.0 * sum([(1.0/(x^2.0))*(-1.0)^y * sin(2.0*PI*440.0*x*amp)\\x<-take 100 oddHarmonics & y<-[1.0..100.0]])\\amp<-generateTriangleAmp]

//Start = trialTriangle
