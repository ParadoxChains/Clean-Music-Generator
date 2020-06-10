definition module panning
import StdEnv

:: Direction= Left | Right


//linear amplitude panning for mono sound
ampPanMonoLinear :: [Real] Real Direction ->([Real],[Real])

//linear amplitude panning for stereophony sound
ampPanStereoLinear :: [Real] [Real] Real Direction ->([Real],[Real])

//linear delay panning for mono sound
delayPanMonoLinear:: [Real] Int Direction ->([Real],[Real])

//linear delay panning for stereophony sound
delayPanStereoLinear:: [Real] [Real] Int Direction ->([Real],[Real])

//circular amplitude panning for mono sound
ampPanMonoCircular :: [Real] Real Direction ->([Real],[Real])

//circular amplitude panning for stereophony sound
ampPanStereoCircular :: [Real] [Real] Real Direction ->([Real],[Real])

//circular delay panning for mono sound
delayPanMonoCircular:: [Real] Real Direction ->([Real],[Real])

//circular delay panning for stereophony sound
delayPanStereoCircular:: [Real] [Real] Real Direction ->([Real],[Real])

//mixed panning for mono sound
panningMono :: [Real] Real Real Direction->([Real],[Real])

//mixed panning for stereophony sound
panningStereo :: [Real] [Real] Real Real Direction->([Real],[Real])