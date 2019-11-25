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
  pure Todo
