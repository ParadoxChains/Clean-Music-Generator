definition module Synthesis.Envelope
import StdEnv
import Util.TimeUtils
import Util.Constants

:: ADSR = {att :: Real
          ,dec :: Real
          ,sus :: Real
          ,rel :: Real
          }

:: DAHDSR = {delay :: Real
            ,attack :: Real
            ,hold :: Real
            ,decay :: Real
            ,sustain :: Real
            ,release :: Real
            }

// Conversion functions
ADSRtoDAHDSR::ADSR ->DAHDSR
DAHDSRtoADSR::DAHDSR->ADSR

// Takes note duration and ADSR to generate envelope
getADSR :: Beat TimeSignature Tempo ADSR -> [Real]

// Takes note duration and DAHDSR to generate envelope
getDAHDSR :: Beat TimeSignature Tempo DAHDSR -> [Real]
getLocalDAHDSR :: Int Beat TimeSignature Tempo DAHDSR -> Real

// Get's wave and envelope and applies envelope to wave
applyEnvelope :: [Real] [Real] -> [Real]
