module Main

import StdEnv
import StdFile
import Wav.Pcm

wavTest :: !*World -> *World
wavTest w
  #! (_, f, w) = fopen "test.wav" FWriteData w
  #! f = writePcmWav
      { numChannels    = 1
      , numBlocks      = 3 * 44100
      , samplingRate   = 44100
      , bytesPerSample = 1
      } (repeatn (3 * 44100) '\0') f
  #! (_, w) = fclose f w
  = w

Start w = wavTest w
