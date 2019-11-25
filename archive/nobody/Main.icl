module Main

import Control.Monad.World
import Test.Wav.Pcm
import Test.Wav.Monadic
import Control.Monad.Parser

// Start w = wavTest w

Start w = runWorldM wavTestM w
