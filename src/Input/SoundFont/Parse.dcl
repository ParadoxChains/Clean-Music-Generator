definition module Input.SoundFont.Parse

import Util.Monad.Result
import Util.Byte

:: SoundFont = Todo Int

parseSoundFont :: [Byte] -> Result SoundFont
