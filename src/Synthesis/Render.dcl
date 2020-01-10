definition module Synthesis.Render
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Synthesis.Envelope
import Synthesis.Generate
import Input.ReadFile

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
render :: [Note] -> [Real]

renderAux :: [NoteChunk] -> [Real]
noteToChunk :: Note -> NoteChunk
