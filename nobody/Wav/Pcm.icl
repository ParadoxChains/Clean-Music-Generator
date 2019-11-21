implementation module Wav.Pcm

import StdEnv
import StdFile
import Data.Byte

:: PcmWavParams =
  { numChannels    :: !Int
  , numBlocks      :: !Int
  , samplingRate   :: !Int
  , bytesPerSample :: !Int
  }

//Taking a list of bytes and a file, writing this list into the file
writeBytes :: [Byte] !*File -> *File
writeBytes []     f = f
writeBytes [b:bs] f 
  #! f = fwritec b f
  = writeBytes bs f

//This function is responsible for writing a single int to the file
//Taking two integers and a file, writing into the file using writeBytes function 
writeNat :: !Int !Int !*File -> *File
writeNat i n f = writeBytes (natToBytesLE i n) f

// The first parameter is the size of the file in bytes minus 8.
writeHeader :: !Int !*File -> *File
writeHeader l f
  #! f = fwrites "RIFF" f
  #! f = writeNat 4 l f
  #! f = fwrites "WAVE" f
  = f
  
//Writing the format and filling the PcmWavParams
writeFormat :: !PcmWavParams !*File -> *File
writeFormat p f
  #! f = fwrites "fmt " f
  #! f = writeNat 4 16 f // Size of the format block
  #! f = writeNat 2 1 f // Audio format: PCM
  #! f = writeNat 2 p.numChannels f
  #! f = writeNat 4 p.samplingRate f
  #! f = writeNat 4 (p.samplingRate * p.bytesPerSample * p.numChannels) f
    // Bytes per second
  #! f = writeNat 2 (p.bytesPerSample * p.numChannels) f // Bytes per block
  #! f = writeNat 2 (8 * p.bytesPerSample) f // Bits per sample
  = f

// The first parameter is the length of the data in bytes
writeData :: !Int [Byte] !*File -> *File
writeData l d f
  #! f = fwrites "data" f
  #! f = writeNat 4 l f
  #! f = writeBytes d f
  | isEven l = f
  = fwritec '\0' f

writePcmWav :: !PcmWavParams [Byte] !*File -> *File
writePcmWav p d f
  #! l = p.bytesPerSample * p.numChannels * p.numBlocks
  #! f = writeHeader (l + if (isEven l) 36 37) f
  #! f = writeFormat p f
  #! f = writeData l d f
  = f
