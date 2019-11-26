module maintest

import StdEnv
import StdFile
import util.Byte
import Input.readFile
import Output.Pcm
import Output.middle_layer
import synthesis.Wavetable
import synthesis.Generate
import synthesis.Wave

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

read :: !*World -> (*World, Info)
read oldW
	#! (b, oldF, newW) = fopen "Input/MIDI/simple.mid" FReadData oldW
	|not b = (newW, abort"can not open file")
	#! (l, newF) = readBytes oldF
	#! (b, newW2) = fclose newF newW
	= (newW2, process l)
		
//Start w = read w

//import synthesis.Wave, synthesis.Generate

Start = generate Sawtooth 440
