module maintest

import StdEnv
import StdFile
import Util.Byte
import Util.ListUtils
import Input.ReadFile
import Output.Pcm
import Output.MiddleLayer
import Synthesis.Wavetable
import Synthesis.Generate
import Synthesis.Wave
import Util.Constants

wavTest :: !*World -> *World
wavTest w
  #! (_, f, w) = fopen "test.wav" FWriteData w
  #! f = writePcmWav
      { numChannels    = 1
      , numBlocks      = 3 * 44100
      , samplingRate   = 44100
      , bytesPerSample = 1
      } (transform (wavetable 0.5) 0.5) f
  #! (_, w) = fclose f w
  = w

//Start w = wavTest w

read :: !*World -> (*World, [Note])
read oldW
	#! (b, oldF, newW) = fopen "Input/MIDI/simple.mid" FReadData oldW
	|not b = (newW, abort"can not open file")
	#! (l, newF) = readBytes oldF
	#! (b, newW2) = fclose newF newW
	= (newW2, readFile l)
		
//Start w = read w

//import synthesis.Wave, synthesis.Generate

saw = generate Sawtooth 220 2205

shift = shiftLeft saw (SAMPLING_RATE/(440*2))
// Start = saw

Start = subtractLists shift saw