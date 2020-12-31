implementation module Synthesis.GeneralEnvelope
import StdEnv
import Util.TimeUtils
import Util.ListUtils
import Util.Constants


:: EnvLevel = {rate :: !Real
              ,level :: !Real
              }

:: LineData = {lRate :: !Real
              ,lStart :: !Real
              ,lEnd :: !Real
              }

:: GenEnv = {levels :: [EnvLevel]
            ,sustainLevel :: !Int
            }


getEnvelope :: !Real !GenEnv -> [Real]
getEnvelope duration envelope = env_shortened ++ env_release
where
    note_samples = secondsToSamples duration
    sust_length = toReal (hd [x.level \\ x <- envelope.levels & ind <- [1,2..(length envelope.levels)] | ind == envelope.sustainLevel])
    env_intro_data = parseData (take envelope.sustainLevel envelope.levels) 0.0 0.0
    env_intro = [0.0] ++ (flatten [generateLine data \\ data <- env_intro_data])
    env_sustain = [sust_length \\ x <- [1,2..(note_samples-(length env_intro))]]
    env_shortened = take note_samples (env_intro ++ env_sustain)
    env_release_data = parseData (drop envelope.sustainLevel envelope.levels) (last env_shortened) 0.0
    env_release = flatten [generateLine data \\ data <- env_release_data]

generateLine :: !LineData -> [Real]
generateLine data = [data.lStart+data.lRate,data.lStart+2.0*data.lRate..data.lEnd]

parseData :: [EnvLevel] !Real !Real -> [LineData]
parseData [] _ _ = []
parseData [x:xs] prev_level diff = [curr_data] ++ (parseData xs x.level new_diff)
where
    line_rate | prev_level > x.level = ~(x.rate / (toReal SAMPLING_RATE))
                = (x.rate / (toReal SAMPLING_RATE))
    curr_data = {lRate = line_rate, lStart = prev_level-line_rate*diff, lEnd = x.level}
    level_length = curr_data.lEnd - curr_data.lStart
    llst = curr_data.lEnd - (level_length-curr_data.lRate*(toReal(floor(level_length/curr_data.lRate))))
    new_diff = (curr_data.lEnd - llst) / curr_data.lRate
