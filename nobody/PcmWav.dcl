definition module PcmWav

import PcmWav.Byte

:: PcmWavParams =
  { numChannels    :: !Int // Number of channels
  , numBlocks      :: !Int // Number of samples (for each channel)
  , samplingRate   :: !Int // Sampling rate in Hz (samples per second)
  , bytesPerSample :: !Int // Number of bytes in each sample
  }

writePcmWav :: !PcmWavParams [Byte] !*File -> *File
