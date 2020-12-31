definition module Synthesis.GeneralEnvelope


:: EnvLevel = {rate :: !Real
              ,level :: !Real
              }

:: GenEnv = {levels :: [EnvLevel]
            ,sustainLevel :: !Int
            }

getEnvelope :: !Real !GenEnv -> [Real]
