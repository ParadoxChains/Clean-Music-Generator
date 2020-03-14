module main
import StdEnv
import Input.ReadFile
import Util.Byte
import Agglomerator


//USER SECTION
//You should change this as necessary.

/*
Name: importMIDI
Arguments: None
Output: String

Used to set the constant of the input file.
*/
importMIDI :: String
importMIDI = "./Input/MIDI/mz_330_1.mid"

outputWave :: String
outputWave = "./mozart_test-03-14_01.wav"

EnvProfile :: ADSR
EnvProfile = {
                att = 0.01,
                dec = 0.05,
                sus = 0.5,
                rel = 0.8
             }

WavType :: Wave
WavType = Sine

Bits :: BitVersion
Bits = ThirtyTwo

//NO GO ZONE
//Don't change this Start line.
Start w = LetsGo importMIDI outputWave EnvProfile WavType Bits w