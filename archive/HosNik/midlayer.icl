implementation module midlayer

import StdEnv
import sinewave
import PcmWav.Byte


//Turning the sine wave into list of Bytes(Chars)
sineToByte :: Real Int Int -> [Byte]
sineToByte amp freq harm = map (\x=toChar(toInt((x+amp)*255.0))) (getSubset (generateSine amp) (getIndexes freq harm))

//Start = sineToByte 2.5 500 4


