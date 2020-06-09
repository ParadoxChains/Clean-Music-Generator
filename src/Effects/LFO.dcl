definition module Effects.LFO
import StdEnv
import Util.TypeDefs
import Synthesis.PhaseAmplitudeConverter

::LFOPhaseMode = FreePhase | RestrictedPhase

::LFOProfile = NoLFO |
               LFO Real Wave Frequency |
               PhasedLFO Phase Real Wave Frequency LFOPhaseMode |
               ReversePhasedLFO Phase Real Wave Frequency |
               ExponentialPhasedLFO Phase Real Wave Frequency


::DualLFOProfile = NoDualLFO |
                   ConvexDualLFO Real Phase Real Wave Frequency |
                   ConcaveDualLFO Real Phase Real Wave Frequency

tremeloLFOEF :: LFOProfile
fastRippleLFOEF :: LFOProfile
slowRippleLFOEF :: LFOProfile
rippleLFOEF :: LFOProfile

getLocalLFO :: Int LFOProfile -> Real
getLocalDualLFO :: Int DualLFOProfile -> Real