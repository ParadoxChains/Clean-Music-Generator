implementation module Effects.Chorus

import StdEnv
import Util.Constants
import Synthesis.Wave
import Util.TypeDefs

ceil :: Real -> Int
ceil x
| toReal(toInt x) == x = toInt x
= (toInt x) + 1

applyChorus :: Wave ChorusParameters -> Wave
applyChorus wave (f, delay, a, b) = [wave!!i \\ i<-[0..toInt(delay_in_sampels)]] ++ 
                              [a * (wave!!i)  + 
                               b * (wave!!(ceil(toReal(i) - delay_in_sampels - abs(sin(2*PI*i*f))))) 
                                        \\ i <- [(toInt(delay_in_sampels)+1)..(length wave - 1)]]
where 
    delay_in_sampels = delay * toReal(SAMPLING_RATE)

