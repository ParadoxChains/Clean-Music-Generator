definition module PcmWav

import PcmWav.Byte

/*:: PcmWavParams =
  { numChannels    :: !Int // Number of channels
  , numBlocks      :: !Int // Number of samples (for each channel)
  , samplingRate   :: !Int // Sampling rate in Hz (samples per second)
  , bytesPerSample :: !Int // Number of bytes in each sample
  }
*/
/*
writeBytes :: [Byte] !*File -> *File

writePcmWav :: !PcmWavParams [Byte] !*File -> *File

writeNat :: !Int !Int !*File -> *File

writeHeader :: !Int !*File -> *File

writeFormat :: !PcmWavParams !*File -> *File

writeData :: !Int [Byte] !*File -> *File
*/
test :: !*World -> *World
