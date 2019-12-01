definition module Input.SoundFont.Parse

import StdMaybe
import Util.Monad.Result
import Util.Byte

:: SoundFont =
  { sdta :: Sdta
  }

:: Sdta =
  { smpl :: [Int]
  , sm24 :: Maybe [Int]
  }

parseSoundFont :: [Byte] -> Result SoundFont
