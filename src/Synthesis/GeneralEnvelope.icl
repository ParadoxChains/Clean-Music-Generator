implementation module Synthesis.GeneralEnvelope
import StdEnv
import Util.TimeUtils
import Util.ListUtils
import Util.Constants


:: EnvLevel = {rate :: Real
              ,level :: Real
              }

:: LineData = {lRate :: Real
              ,lStart :: Real
              ,lEnd :: Real
              }

:: GenEnv = {levels :: [EnvLevel]
            ,sustainLevel :: Int
            }


getEnvelope :: Real GenEnv -> [Real]
getEnvelope duration envelope = envShortened ++ envRelease
where
    noteSamples = secondsToSamples duration
    sustL = toReal (hd [x.level \\ x <- envelope.levels & ind <- [1,2..(length envelope.levels)] | ind == envelope.sustainLevel])
    envIntroData = parseData (take envelope.sustainLevel envelope.levels) 0.0 0.0
    envIntro = [0.0] ++ (flatten [generateLine data \\ data <- envIntroData])
    envSustain = [sustL \\ x <- [1,2..(noteSamples-(length envIntro))]]
    envShortened = take noteSamples (envIntro ++ envSustain)
    envReleaseData = parseData (drop envelope.sustainLevel envelope.levels) (last envShortened) 0.0
    envRelease = flatten [generateLine data \\ data <- envReleaseData]

generateLine :: LineData -> [Real]
generateLine data = [data.lStart+data.lRate,data.lStart+2.0*data.lRate..data.lEnd]

parseData :: [EnvLevel] Real Real -> [LineData]
parseData [] _ _ = []
parseData [x:xs] prevLevel diff = [currData] ++ (parseData xs x.level newDiff)
where
    lineRate | prevLevel > x.level = ~(x.rate / (toReal SAMPLING_RATE))
                = (x.rate / (toReal SAMPLING_RATE))
    currData = {lRate = lineRate, lStart = prevLevel-lineRate*diff, lEnd = x.level}
    levelLength = currData.lEnd - currData.lStart
    llst = currData.lEnd - (levelLength-currData.lRate*(toReal(floor(levelLength/currData.lRate))))
    newDiff = (currData.lEnd - llst) / currData.lRate
