module Part2
import StdEnv
import constants
import sinewave
import accesstable
import utils



oddHarmonics= [(1.0),(3.0)..(100.0)]

generateSquareAmp:: [Real]
generateSquareAmp = [1.0/x \\x<-oddHarmonics]


generateTriangleAmp :: [Real]
generateTriangleAmp = [(1.0/(x^2.0))*(-1.0)^y\\x<-oddHarmonics & y<-[1.0..]]


generateTriangle :: [Real]
generateTriangle = foldr sumLists (repeatn tableSize 0.0) l
where
    l = (get (generateSine 1.0) oddHarmonics generateTriangleAmp freq)


generateSquare :: [Real]
generateSquare = foldr sumLists (repeatn tableSize 0.0) l
where
    l = (get (generateSine 1.0) oddHarmonics generateSquareAmp freq)

