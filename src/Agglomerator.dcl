definition module Agglomerator

import StdEnv, StdFile
import Input.Chunks, Input.ReadFile
import Output.MiddleLayer, Output.Pcm
import Synthesis.Accesstable, Synthesis.CasioEnvelope, Synthesis.Envelope,  Synthesis.GeneralEnvelope, Synthesis.Generate, Synthesis.BufferRender, Synthesis.Wave, Synthesis.Wavetable
import Util.Byte, Util.Constants, Util.ListUtils, Util.Monad, Util.Rand, Util.TimeUtils, Util.TypeDefs

 
//debug, file read
//LetsGo :: String String ADSR Wave BitVersion !*World -> [Note]



//debug, render.
//LetsGo :: String String ADSR Wave BitVersion !*World -> [Real]


LetsGo :: String String ADSR Wave BitVersion !*World -> *World

Diagnostics :: [String] String ADSR Wave BitVersion !*World -> *World
