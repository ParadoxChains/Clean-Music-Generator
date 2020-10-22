module main
import StdEnv
import Input.MIDI.ReadFile
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

SIMPLE_MIDI :: String
SIMPLE_MIDI = "simple.mid"

FURELISESHORT_MIDI :: String
FURELISESHORT_MIDI = "FurElise-Short.mid"

FURELISE_MIDI :: String 
FURELISE_MIDI = "FurElise.mid"

importMIDI :: String -> String
importMIDI name = "./Input/MIDI/sample_input/" +++ name

outputWave :: String
outputWave = "./diag.wav"


EnvProfile :: ADSR
EnvProfile = {
                att = 0.01,
                dec = 0.05,
                sus = 0.5,
                rel = 0.8
             }

WavType :: Wave
WavType = Pulse

Bits :: BitVersion
Bits = ThirtyTwo

diagOut :: String
diagOut = "./diag01.txt"


//NO GO ZONE
//Don't change this Start line.
Start w = LetsGo (importMIDI FURELISESHORT_MIDI) outputWave EnvProfile WavType Bits w

listMIDIs :: [String]
listMIDIs = [importMIDI SIMPLE_MIDI, importMIDI FURELISESHORT_MIDI, importMIDI FURELISE_MIDI]

//Start w = Diagnostics listMIDIs diagOut EnvProfile WavType Bits w
