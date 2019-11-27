definition module Input.Wav.Parse

import Util.Monad.Result
import Util.Byte

// Currently only parses the data without any processing
// so it only works with 8-bit 1-channel wav files
:: Wav :== [Byte]

parseWav :: [Byte] -> Result Wav
