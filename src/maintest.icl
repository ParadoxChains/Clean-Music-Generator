module maintest
import StdEnv
import StdFile
import Output.Pcm
import Output.middle_layer
import synthesis.Wavetable
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

Start w = wavTest w
//import synthesis.Wave, synthesis.Generate

//Start = generate Sawtooth
