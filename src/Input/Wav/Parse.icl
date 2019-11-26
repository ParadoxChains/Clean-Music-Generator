implementation module Input.Wav.Parse

import util.Monad
import util.Monad.Parser
import util.Monad.Result
import util.Byte

parseWav :: [Byte] -> Result Wav
parseWav bs = parse wave bs

wave :: Parser Wav
wave =
  string "RIFF" >>>
  takeP 4 >>>
  string "WAVE" >>>
  fmt >>>
  data >>= \bs.
  pure bs

fmt :: Parser ()
fmt =
  string "fmt " >>>
  uintBE 4 >>= \n.
  takeP n >>>
  pure ()

data :: Parser [Byte]
data =
  string "data" >>>
  uintBE 4 >>= \n.
  takeP n >>= \bs.
  pure bs
