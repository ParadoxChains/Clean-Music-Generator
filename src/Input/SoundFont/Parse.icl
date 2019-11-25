implementation module Input.SoundFont.Parse

import StdMaybe
import util.Parser
import util.Byte

parseSoundFont :: [Byte] -> Maybe SoundFont
parseSoundFont bs = parse parseSfbk bs

parseSfbk :: Parser SoundFont
parseSfbk =
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
