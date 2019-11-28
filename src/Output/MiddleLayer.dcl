definition module Output.MiddleLayer
import StdEnv


//Takes a wavetable and the upper bound of the interval and
//converts the signals to bytes
transform8 ::[Real] Real->[Char]  
////Takes a list of wavetables and the upper bound of 
//the intervals and converts the lists of signals to lists of bytes
transform8_what_if ::[[Real]] Real->[[Char]]

//Takes a wavetable and the upper bound of the interval and converts
//the signals to Bytes(32 bit version)
transform16 :: [Real] Real -> [Char]

//Takes a list of wavetables and the upper bound of the interval and converts
//lists of wavetables to lists of Bytes(16 bit version)
transform16_what_if::[[Real]] Real->[[Char]]


//Takes a wavetable and the upper bound of the interval and converts
//the signals to Bytes(32 bit version)
transform32::[Real] Real->[Char]

//Takes a list of wavetables and the upper bound of the interval and converts
//lists of wavetables to lists of Bytes(32 bit version)
transform32_what_if::[[Real]] Real->[[Char]]
