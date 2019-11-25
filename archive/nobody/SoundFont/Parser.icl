implementation module SoundFont.Parser

import Control.Monad.Parser
import Data.Byte

parseSoundFont :: Parser SoundFont
parseSoundFont =
  riff >>>
  parseChunkLength >>= \n.
  sfbk >>>
  pure (Todo n)

parseChunkLength :: Parser Int
parseChunkLength = bytesToUintLE <$> takeP 4

riff :: Parser String
riff = string "RIFF"

sfbk :: Parser String
sfbk = string "sfbk"
