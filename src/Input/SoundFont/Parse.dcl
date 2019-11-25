definition module Input.SoundFont.Parse

import util.Monad.Result
import util.Byte

:: SoundFont = Todo Int

parseSoundFont :: [Byte] -> Result SoundFont
