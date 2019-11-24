definition module Control.Monad.World

import Control.Monad
import Control.Monad.File

:: WorldM a =: WorldM (*World -> *(!a, !*World))

runWorldM :: !(WorldM a) !*World -> (a, !*World)

instance Monad WorldM

withFile :: !String !Int !(FileM a) -> WorldM a
