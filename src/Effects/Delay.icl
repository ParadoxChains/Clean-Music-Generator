implementational module Effects.Delay

import StdEnv
import Util.Constants
import Synthesis.Wave
import Util.TypeDefs


applyDelay :: Wave DelayParameters -> Wave
applyDelay wave (Ts, Delay) = [wave!!i \ i <- [0..N]] + [wave!!i + wave!!(i - N) \\ i <- [N..(length wave)]]
where
    N = Delay / Ts
