definition module Input.SoundFont.Parse

import StdMaybe
import Util.Monad.Result
import Util.Byte

:: Todo = Todo

:: Info :== Todo

:: Sfbk =
  { info :: !Info
  , sdta :: !Sdta
  , pdta :: !Pdta
  }

:: Sdta =
  { smpl :: ![Int]
  , sm24 :: !Maybe [Int]
  }

:: Pdta =
  { phdr :: ![PresetHeader]
  , pbag :: ![Bag]
  , pmod :: ![ModList]
  , pgen :: ![GenList]
  , inst :: ![Inst]
  , ibag :: ![Bag]
  , imod :: ![ModList]
  , igen :: ![GenList]
  , shdr :: ![Sample]
  }

:: PresetHeader =
  { presetName   :: !String
  , preset       :: !Int
  , bank         :: !Int
  , presetBagNdx :: !Int
  , library      :: !Int
  , genre        :: !Int
  , morphology   :: !Int
  }

:: Inst =
  { instName   :: !String
  , instBagNdx :: !Int
  }

:: Bag =
  { genNdx :: !Int
  , modNdx :: !Int
  }

:: ModList :== Todo

:: GenList :== Todo

:: Sample =
  { sampleName      :: !String
  , start           :: !Int
  , end             :: !Int
  , startLoop       :: !Int
  , endLoop         :: !Int
  , sampleRate      :: !Int
  , originalPitch   :: !Int
  , pitchCorrection :: !Int
  , sampleLink      :: !Int
  , sampleType      :: !Todo
  }

parseSoundFont :: [Byte] -> Result Sfbk
