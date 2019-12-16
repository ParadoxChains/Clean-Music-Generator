definition module Synthesis.Render
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Synthesis.Generate
import Input.ReadFile
import Util.ArrayUtils

:: NoteChunk = { note :: Note
               , wave :: Wave
               , timeSig :: TimeSignature
               , tempo :: Tempo
               , dahdsr :: DAHDSR
               }

// Generate sample of silence
generateSilence :: Int -> [Real]

//Gets note, renders and returns list of samples
renderNoteChunk :: NoteChunk -> [Real]

//Number of samples baes on time signature, tempo and duration
numberOfSamples :: NoteChunk Int -> Int

//Normalization of samples
normalizeList :: [Real] -> [Real]
//Render notes to get samples
render :: [NoteChunk] -> [Real]

