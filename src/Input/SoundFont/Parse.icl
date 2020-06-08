implementation module Input.SoundFont.Parse

import StdEnv
import Util.Monad.Parser
import Util.Monad.Result
import Util.Byte

parseSoundFont :: [Byte] -> Result Sfbk
parseSoundFont bs = parse (parseSfbk <* eof) bs

parseSfbk :: Parser Sfbk
parseSfbk =
  string "RIFF" >>>
  takeP 4 >>>
  string "sfbk" >>>
  parseInfo >>= \info.
  parseSdta >>= \sdta.
  parsePdta >>= \pdta.
  pure { Sfbk | info, sdta, pdta }

parseInfo :: Parser Info
parseInfo =
  string "LIST" >>>
  binint Unsigned LE 4 >>= \l.
  takeP l >>>
  pure Todo

parseSdta :: Parser Sdta
parseSdta =
  string "LIST" >>>
  takeP 4 >>>
  string "sdta" >>>
  parseSmpl >>= \(smplLen, smpl).
  optional (parseSm24 smplLen) >>= \sm24.
  pure { Sdta | smpl, sm24 }

parseSmpl :: Parser (!Int, ![Int])
parseSmpl = parseListWithLength "smpl" 2 (binint Signed LE 2)

parseSm24 :: !Int -> Parser [Int]
parseSm24 len =
  string "sm24" >>>
  binint Unsigned LE 4 >>= \size.
  when (size <> padded) (fail
    ("unexpected chunk size " +++ toString size
    +++ ", expecting " +++ toString padded)) >>>
  replicateM len (binint Unsigned LE 1) >>= \data.
  takeP pad >>>
  pure data
  where
  pad = if (isOdd len) 1 0
  padded = len + pad

parsePdta :: Parser Pdta
parsePdta =
  string "LIST" >>>
  takeP 4 >>>
  string "pdta" >>>
  parseList "phdr" 38 parsePresetHeader >>= \phdr.
  parseList "pbag" 4  parseBag          >>= \pbag.
  parseList "pmod" 10 parseModList      >>= \pmod.
  parseList "pgen" 4  parseGenList      >>= \pgen.
  parseList "inst" 22 parseInst         >>= \inst.
  parseList "ibag" 4  parseBag          >>= \ibag.
  parseList "imod" 10 parseModList      >>= \imod.
  parseList "igen" 4  parseGenList      >>= \igen.
  parseList "shdr" 46 parseSample       >>= \shdr.
  pure { Pdta | phdr, pbag, pmod, pgen, inst, ibag, imod, igen, shdr }

parsePresetHeader :: Parser PresetHeader
parsePresetHeader =
  takeP 20 >>= \presetName.
  binint Unsigned LE 2 >>= \preset.
  binint Unsigned LE 2 >>= \bank.
  binint Unsigned LE 2 >>= \presetBagNdx.
  binint Unsigned LE 4 >>= \library.
  binint Unsigned LE 4 >>= \genre.
  binint Unsigned LE 4 >>= \morphology.
  pure { PresetHeader
    | presetName = getName presetName
    , preset, bank, presetBagNdx, library, genre, morphology }

parseInst :: Parser Inst
parseInst =
  takeP 20 >>= \instName.
  binint Unsigned LE 2 >>= \instBagNdx.
  pure { Inst
    | instName = getName instName
    , instBagNdx }

parseBag :: Parser Bag
parseBag =
  binint Unsigned LE 2 >>= \genNdx.
  binint Unsigned LE 2 >>= \modNdx.
  pure { Bag | genNdx, modNdx }

parseModList :: Parser ModList
parseModList = Todo <$ takeP 10

parseGenList :: Parser GenList
parseGenList = Todo <$ takeP 4

parseSample :: Parser Sample
parseSample =
  takeP 20 >>= \sampleName.
  binint Unsigned LE 4 >>= \start.
  binint Unsigned LE 4 >>= \end.
  binint Unsigned LE 4 >>= \startLoop.
  binint Unsigned LE 4 >>= \endLoop.
  binint Unsigned LE 4 >>= \sampleRate.
  binint Unsigned LE 1 >>= \originalPitch.
  binint Signed   LE 1 >>= \pitchCorrection.
  binint Unsigned LE 2 >>= \sampleLink.
  Todo <$ takeP 2 >>= \sampleType.
  pure { Sample
    | sampleName = getName sampleName
    , start, end, startLoop, endLoop, sampleRate
    , originalPitch, pitchCorrection, sampleLink, sampleType }

parseListWithLength :: !String !Int !(Parser a) -> Parser (!Int, ![a])
parseListWithLength id subSize p =
  string id >>>
  binint Unsigned LE 4 >>= \size.
  when (size rem subSize <> 0) (fail
    ("unexpected chunk size " +++ toString size
    +++ ", expecting a multiple of " +++ toString subSize)) >>>
  let len = size / subSize in
  replicateM len p >>= \data.
  pure (len, data)

parseList :: !String !Int !(Parser a) -> Parser [a]
parseList id subSize p = snd <$> parseListWithLength id subSize p

getName :: ![Byte] -> String
getName bs = { b \\ b <- takeWhile ((<>) '\0') bs }
