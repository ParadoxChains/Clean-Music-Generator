implementation module Util.Pitch
import StdEnv

//custom library imports
import Input.MIDI.Chunks
import Util.ListUtils, Util.TypeDefs

convStrToFreq :: String -> Frequency
convStrToFreq input = getFrequency(toChar(parseString input))


parseString :: String -> Int
parseString input = result
where
    inputList = strToList input
    noteName = hd inputList
    octave = toInt(last inputList) - 48
    modifiers = init(tl inputList)
    x = convertNoteName noteName
    sharps = length(filter ((==)'#') modifiers)
    flats = length(filter ((==)'b') modifiers)
    result = (x+sharps-flats)+(12*(octave+1))

convertNoteName :: Char -> Int
convertNoteName 'C' = 0
convertNoteName 'D' = 2
convertNoteName 'E' = 4
convertNoteName 'F' = 5
convertNoteName 'G' = 7
convertNoteName 'A' = 9
convertNoteName 'B' = 11
