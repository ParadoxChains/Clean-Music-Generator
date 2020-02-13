module main
import StdEnv
import Input.ReadFile
import Util.Byte
import Agglomerator

/*
read :: !*World -> (*World, [Note])
read oldW
 	#! (b, oldF, newW) = fopen "Input/MIDI/debussy-clair-de-lune.mid" FReadData oldW
 	|not b = (newW, abort"can not open file")
 	#! (l, newF) = readBytes oldF
 	#! (b, newW2) = fclose newF newW
 	= (newW2, readFile l)

Start w = read w
*/

//USER SECTION
//You should change this as necessary.
importMIDI :: String
importMIDI = "./Input/MIDI/liz_rhap02.mid"

outputWave :: String
outputWave = "./TestRenders/WAV/Liszt_Hungarian-Rhapsody-2.wav"

EnvProfile :: ADSR
EnvProfile = {
                att = 0.01,
                dec = 0.05,
                sus = 0.5,
                rel = 0.8
             }

WavType :: Wave
WavType = Square

Bits :: BitVersion
Bits = ThirtyTwo

//NO GO ZONE
//Don't change this Start line.
Start w = LetsGo importMIDI outputWave EnvProfile WavType Bits w