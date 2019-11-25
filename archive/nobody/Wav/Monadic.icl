implementation module Wav.Monadic

import StdEnv
import Control.Monad
import Control.Monad.File
import Data.Byte

:: PcmWavParams =
  { numChannels    :: !Int
  , numBlocks      :: !Int
  , samplingRate   :: !Int
  , bytesPerSample :: !Int
  }

writeUint :: !Int !Int -> FileM ()
writeUint i n = mapM_ writeChar (uintToBytesLE i n)

writeHeader :: !Int -> FileM ()
writeHeader l =
  writeString "RIFF" >>>
  writeUint 4 l >>>
  writeString "WAVE"

writeFormat :: !PcmWavParams -> FileM ()
writeFormat p =
  writeString "fmt " >>>
  writeUint 4 16 >>>
  writeUint 2 1 >>>
  writeUint 2 p.numChannels >>>
  writeUint 4 p.samplingRate >>>
  writeUint 4 (p.samplingRate * p.bytesPerSample * p.numChannels) >>>
  writeUint 2 (p.bytesPerSample * p.numChannels) >>>
  writeUint 2 (8 * p.bytesPerSample)

writeData :: !Int ![Byte] -> FileM ()
writeData l d =
  writeString "data" >>>
  writeUint 4 l >>>
  mapM_ writeChar d >>>
  when (isOdd l) (writeChar '\0')

writePcmWav :: !PcmWavParams ![Byte] -> FileM ()
writePcmWav p d =
  let l = p.bytesPerSample * p.numChannels * p.numBlocks in
  writeHeader (l + if (isEven l) 36 37) >>>
  writeFormat p >>>
  writeData l d
