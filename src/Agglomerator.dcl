definition module Agglomerator

import StdEnv, StdFile
import Input.Chunks, Input.ReadFile
import Output.MiddleLayer, Output.Pcm
import Synthesis.Accesstable, Synthesis.CasioEnvelope, Synthesis.Envelope,  Synthesis.GeneralEnvelope, Synthesis.Generate, Synthesis.Render, Synthesis.Wave, Synthesis.Wavetable
import Util.Byte, Util.Constants, Util.ListUtils, Util.Monad, Util.Rand, Util.TimeUtils, Util.TypeDefs

LetsGo :: String String ADSR Wave BitVersion !*World -> *World
