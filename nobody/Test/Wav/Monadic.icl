implementation module Test.Wav.Monadic

import StdEnv
import Control.Monad.World
import Wav.Monadic

wavTestM :: WorldM ()
wavTestM = withFile "test.wav" FWriteData
  (writePcmWav
    { numChannels    = 1
    , numBlocks      = 3 * 44100
    , samplingRate   = 44100
    , bytesPerSample = 1
    } (repeatn (3 * 44100) '\0'))
