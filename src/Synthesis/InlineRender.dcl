definition module Synthesis.InlineRender
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Synthesis.Envelope
import Synthesis.Generate
import Input.MIDI.ReadFile


:: NoteChunk = { note :: !Note
               , wave :: !Wave
               , timeSig :: !TimeSignature
               , tempo :: !Tempo
               , dahdsr :: !DAHDSR
               }

:: ChannelProfile = {
                        wavType :: !Wave,
                        envelope :: !DAHDSR
                    }

// Generate sample of silence
generateSilence :: !Int -> [Real]

//Render notes to get samples
render :: [Note] !ChannelProfile -> [Real]

//------------------------------------------------------------------

// Lenght of timeline
renderTotalSamples :: ![Note] !ChannelProfile -> Int

// Number of rendered values
totalRendered :: ![Note] !ChannelProfile -> Int

// Can you get that list and send me txt? :) (For simple, FurElise-Short, FurElise and liz-raph02)
renderData :: [Note] !ChannelProfile -> [(Int,Int)]
