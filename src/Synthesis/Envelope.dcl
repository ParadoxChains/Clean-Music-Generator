definition module Synthesis.Envelope

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
getADSR :: Real ADSR -> [Real]

// Takes note duration and DAHDSR to generate envelope
getDAHDSR :: Real DAHDSR -> [Real]

// Get's wave and envelope and applies envelope to wave
applyEnvelope :: [Real] [Real] -> [Real] 