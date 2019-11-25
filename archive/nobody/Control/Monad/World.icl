implementation module Control.Monad.World

import StdFile
import Control.Monad
import Control.Monad.File

runWorldM :: !(WorldM a) !*World -> (a, !*World)
runWorldM (WorldM f) w = f w

instance Monad WorldM where
  pure a = WorldM \w. (a, w)
  (>>=) (WorldM f) g = WorldM \w0. let
    (a, w1) = f w0
    in runWorldM (g a) w1

withFile :: !String !Int !(FileM a) -> WorldM a
withFile fn mode (FileM f) = WorldM \w0. let
    (_, f0, w1) = fopen fn mode w0
    (a, f1) = f f0
    (_, w2) = fclose f1 w1
    in (a, w2)
