definition module Synthesis.ArrayRender
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Synthesis.EnvelopeArr
import Synthesis.Generate
import Input.ReadFile

:: NoteChunk = { note :: Note
               , wave :: Wave
               , timeSig :: TimeSignature
               , tempo :: Tempo
               , dahdsr :: DAHDSR
               }

// Generate sample of silence
generateSilence :: Int -> {Real}

//Render notes to get samples
render :: [Note] ChannelProfile -> {Real}

:: ChannelProfile = {
                        wavType :: Wave,
                        envelope :: DAHDSR
                    }
                    