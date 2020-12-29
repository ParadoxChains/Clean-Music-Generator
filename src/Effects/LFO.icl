implementation module Effects.LFO
import StdEnv
import Synthesis.PhaseAmplitudeConverter
import Util.Constants


isFree :: LFOPhaseMode -> Bool
isFree FreePhase = True
isFree _ = False

getLocalLFO :: Int LFOProfile -> Real
getLocalLFO index NoLFO = 1.0
getLocalLFO index (LFO amp waveShape freq) = amp * (generateLocal index waveShape freq)
getLocalLFO index (PhasedLFO phase amp waveShape freq phaseMode)
    = amp * restrictedPhaseValue * (generateLocal index waveShape freq)
where
    currentPhaseValue = (abs phase) * (toReal index) / (toReal SAMPLING_RATE)
    restrictedPhaseValue | ((isFree phaseMode) || currentPhaseValue < 1.0) = currentPhaseValue = 1.0 
getLocalLFO index (ReversePhasedLFO phase amp waveShape freq)
    = amp * restrictedPhaseValue * (generateLocal index waveShape freq)
where
    currentPhaseValue = 1.0 - ((abs phase) * (toReal index) / (toReal SAMPLING_RATE))
    restrictedPhaseValue | currentPhaseValue > 0.0 = currentPhaseValue = 0.0
getLocalLFO index (ExponentialPhasedLFO phase amp waveShape freq)
    = amp * currentPhaseValue * (generateLocal index waveShape freq)
where
    currentPhaseValue = (abs phase) ^ (freq * (toReal index) / (toReal SAMPLING_RATE))


getLocalDualLFO :: Int DualLFOProfile -> Real
getLocalDualLFO index NoDualLFO = 1.0
getLocalDualLFO index (ConvexDualLFO peak phase amp waveShape freq)
    = amp * restrictedPhaseValue * (generateLocal index waveShape freq)
where
    increasePhaseValue = (abs phase) * (toReal index) / (toReal SAMPLING_RATE)
    currentPhaseValue | increasePhaseValue <= peak = increasePhaseValue = peak-(increasePhaseValue-peak)
    restrictedPhaseValue | currentPhaseValue > 0.0 = currentPhaseValue = 0.0
getLocalDualLFO index (ConcaveDualLFO peak phase amp waveShape freq)
    = amp * currentPhaseValue * (generateLocal index waveShape freq)
where
    increasePhaseValue = peak - ((abs phase) * (toReal index) / (toReal SAMPLING_RATE))
    currentPhaseValue | increasePhaseValue >= 0.0 = increasePhaseValue = (~increasePhaseValue)
    restrictedPhaseValue | currentPhaseValue > 0.0 = currentPhaseValue = 0.0


applyLFO :: [Real] LFOProfile Real -> [Real]
applyLFO ls prof mix = [(curr * (getLocalLFO i prof) * mix) + (curr * (1.0 - mix)) \\ curr <- ls & i <- [1,2..]]

applyLocalLFO :: Int Real LFOProfile Real -> Real
applyLocalLFO indx val prof mix = (val * (getLocalLFO indx prof) * mix) + (val * (1.0 - mix)) 

applyDualLFO :: [Real] DualLFOProfile Real -> [Real]
applyDualLFO ls prof mix = [(curr * (getLocalDualLFO i prof) * mix) + (curr * (1.0 - mix)) \\ curr <- ls & i <- [1,2..]]

applyLocalDualLFO :: Int Real LFOProfile Real -> Real
applyLocalDualLFO indx val prof mix = (val * (getLocalLFO indx prof) * mix) + (val * (1.0 - mix)) 


