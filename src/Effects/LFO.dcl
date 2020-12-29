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


// tremeloLFOEF :: LFOProfile
tremeloLFOEF :== PhasedLFO 2.0 1.0 Sine 10.0 FreePhase

// fastRippleLFOEF :: LFOProfile
fastRippleLFOEF :== LFO 1.0 Sine 5.0

// slowRippleLFOEF :: LFOProfile
slowRippleLFOEF :== LFO 10.0 Sine 0.5

// rippleLFOEF :: LFOProfile
rippleLFOEF :== ReversePhasedLFO 1.1 5.0 Sine 20.0


getLocalLFO :: Int LFOProfile -> Real
getLocalDualLFO :: Int DualLFOProfile -> Real