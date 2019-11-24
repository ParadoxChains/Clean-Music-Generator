definition module Control.Monad.World

import Control.Monad
import Control.Monad.File

:: WorldM a =: WorldM (*World -> *(!a, !*World))

instance Monad WorldM

runWorldM :: !(WorldM a) !*World -> (a, !*World)

withFile :: !String !Int !(FileM a) -> WorldM a
