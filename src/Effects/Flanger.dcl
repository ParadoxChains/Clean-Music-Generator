definition module Effects.Flanger
import Synthesis.Wave
import Util.TypeDefs

// Applying flanger to given Wave
// Parameters: (A, rate, manual)
applyFlanger :: Wave FlangerParameters -> Wave
