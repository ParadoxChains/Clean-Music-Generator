implementational module Effects.Delay

import StdEnv
import Util.Constants
import Synthesis.Wave
import Util.TypeDefs


applyDelay :: Wave DelayParameters -> Wave
applyDelay wave (ts, delay) = [0 \\ i <- [0..N]] + wave
where
    N = delay / ts
