implementation module Effects.Panning
import StdEnv
import Util.Constants
import Synthesis.Wave
import Util.TypeDefs





ampPanMonoLinear :: [Real] Real Direction ->([Real],[Real])
ampPanMonoLinear wave pan_val dir
| dir == Left = ([x*(1.0+pan_val)\\x<-wave],[x*(1.0-pan_val)\\x<-wave])
=([x*(1.0-pan_val)\\x<-wave],[x*(1.0-pan_val)\\x<-wave])

ampPanStereoLinear :: [Real] [Real] Real Direction ->([Real],[Real])
ampPanStereoLinear wave_l wave_r pan_val dir
| dir == Left = ([x*(1.0+pan_val)\\x<-wave_l],[x*(1.0-pan_val)\\x<-wave_r])
=([x*(1.0-pan_val)\\x<-wave_l],[x*(1.0+pan_val)\\x<-wave_r])


delayPanMonoLinear:: [Real] Int Direction ->([Real],[Real])
delayPanMonoLinear wave shift_val dir
| dir == Left = ((repeatn shift_val 0.0)++wave,wave++(repeatn shift_val 0.0))
=(wave++(repeatn shift_val 0.0),(repeatn shift_val 0.0)++wave)

delayPanStereoLinear:: [Real] [Real] Int Direction ->([Real],[Real])
delayPanStereoLinear wave_l wave_r shift_val dir
| dir == Left = ((repeatn shift_val 0.0)++wave_l,wave_r++(repeatn shift_val 0.0))
=(wave_l++(repeatn shift_val 0.0),(repeatn shift_val 0.0)++wave_r)


ampPanMonoCircular :: [Real] Real Direction ->([Real],[Real])
ampPanMonoCircular wave pan_val dir
| dir == Left = ([x*loudness\\x<-wave],[x*(1.0-loudness)\\x<-wave])
=([x*(1.0-loudness)\\x<-wave],[x*loudness\\x<-wave])
where
    PI=3.14
    dist=sqrt(2.0*HALF_HEAD^2.0 - 2.0*HALF_HEAD^2.0*cos((1.0-pan_val) * PI/2.0))
    loudness=0.5 + 0.5*(1.0-(dist/HALF_HEAD))

ampPanStereoCircular :: [Real] [Real] Real Direction ->([Real],[Real])
ampPanStereoCircular wave_l wave_r pan_val dir
| dir == Left = ([x*loudness\\x<-wave_l],[x*(1.0-loudness)\\x<-wave_r])
=([x*(1.0-loudness)\\x<-wave_l],[x*loudness\\x<-wave_r])
where
    PI=3.14
    dist=sqrt(2.0*HALF_HEAD^2.0 - 2.0*HALF_HEAD^2.0*cos((1.0-pan_val) * PI/2.0))
    loudness=0.5 + 0.5*(1.0-(dist/HALF_HEAD))



delayPanMonoCircular:: [Real] Real Direction ->([Real],[Real])
delayPanMonoCircular wave pan_val dir
| dir == Left = ((repeatn shift_val 0.0)++wave,wave++(repeatn shift_val 0.0))
=(wave++(repeatn shift_val 0.0),(repeatn shift_val 0.0)++wave)
where
    dist=sqrt(2.0*HALF_HEAD^2.0 - 2.0*HALF_HEAD^2.0*cos((1.0-pan_val) * PI/2.0))
    shift_val=toInt (SAMPLING_RATE * (dist/SOUND_SPEED))


delayPanStereoCircular:: [Real] [Real] Real Direction ->([Real],[Real])
delayPanStereoCircular wave_l wave_r pan_val dir
| dir == Left = ((shift_val shiftVal 0.0)++wave_l,wave_r++(repeatn shift_val 0.0))
=(wave_l++(repeatn shift_val 0.0),(repeatn shift_val 0.0)++wave_r)
where
    dist=sqrt(2.0*HALF_HEAD^2.0 - 2.0*HALF_HEAD^2.0*cos((1.0-pan_val) * PI/2.0))
    shift_val=toInt(SAMPLING_RATE * (dist/SOUND_SPEED))


panningMono :: [Real] Real Real Direction->([Real],[Real])
panningMono wave pan_val mix dir  = ( map (\(dry, wet) = (mix*wet) + ((1.0-mix)*dry)) [ (d,w) \\ d<-wave_l & w<-wet_left],
                                    map (\(dry, wet) = (mix*wet) + ((1.0-mix)*dry)) [ (d,w) \\ d<-wave_r & w<-wet_right])
where
    wave_r=wave
    wave_l=wave
    waves = ampPanStereoCircular wave_l wave_r pan_val dir
    w_l=fst waves
    w_r=snd waves
    (wet_left, wet_right) = delayPanStereoCircular w_l w_r pan_val dir


panningStereo :: [Real] [Real] Real Real Direction->([Real],[Real])
panningStereo wave_l wave_r pan_val mix dir  = ( map (\(dry, wet) = mix*wet + (1.0-mix)*dry) [ (d,w) \\ d<-wave_l & w<-wet_left],
                                                map (\(dry, wet) = mix*wet + (1.0-mix)*dry) [ (d,w) \\ d<-wave_r & w<-wet_right])
where
    waves = ampPanStereoCircular wave_l wave_r pan_val dir
    w_l=fst waves
    w_r=snd waves
    (wet_left, wet_right) = delayPanStereoCircular w_l w_r pan_val dir

