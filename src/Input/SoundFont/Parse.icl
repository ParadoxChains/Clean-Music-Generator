implementation module Input.SoundFont.Parse

import Util.Monad.Parser
import Util.Monad.Result
import Util.Byte

parseSoundFont :: [Byte] -> Result SoundFont
parseSoundFont bs = parse sfbk bs

sfbk :: Parser SoundFont
sfbk =
  string "RIFF" >>>
  uintLE 4 >>= \n.
  string "sfbk" >>>
  pure (Todo n)
