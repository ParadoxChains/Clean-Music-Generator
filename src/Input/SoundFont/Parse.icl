implementation module Input.SoundFont.Parse

import Util.Monad.Parser
import Util.Monad.Result
import Util.Byte

parseSoundFont :: [Byte] -> Result SoundFont
parseSoundFont bs = parse parseSfbk bs

parseSfbk :: Parser SoundFont
parseSfbk =
  string "RIFF" >>>
  takeP 4 >>>
  string "sfbk" >>>
  parseInfo >>>
  parseStda >>= \stda.
  pure
    { sdta = stda
    }

parseInfo :: Parser ()
parseInfo =
  string "LIST" >>>
  uintLE 4 >>= \l.
  takeP l >>>
  pure ()

parseStda :: Parser Sdta
parseStda =
  string "LIST" >>>
  takeP 4 >>>
  string "sdta" >>>
  parseSmpl >>= \smpl.
  optional parseSm24 >>= \sm24.
  pure
    { smpl = smpl
    , sm24 = sm24
    }

parseSmpl :: Parser [Int]
parseSmpl =
  string "smpl" >>>
  uintLE 4 >>= \l.
  replicateM l (intLE 2) >>= \ns.
  pure ns

parseSm24 :: Parser [Int]
parseSm24 =
  string "sm24" >>>
  uintLE 4 >>= \l.
  replicateM l (intLE 1) >>= \ns.
  pure ns
