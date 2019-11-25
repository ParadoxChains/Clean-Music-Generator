implementation module Input.SoundFont.Parse

import util.Monad.Parser
import util.Monad.Result
import util.Byte

parseSoundFont :: [Byte] -> Result SoundFont
parseSoundFont bs = parse sfbk bs

sfbk :: Parser SoundFont
sfbk =
  string "RIFF" >>>
  uintLE 4 >>= \n.
  string "sfbk" >>>
  pure (Todo n)
