implementation module Control.Monad.File

import StdFile
import Control.Monad

runFileM :: !(FileM a) !*File -> (a, !*File)
runFileM (FileM f) file = f file

instance Monad FileM where
  pure a = FileM \f. (a, f)
  (>>=) :: !(FileM a) (a -> FileM b) -> FileM b
  (>>=) (FileM f) g = FileM \f0. let
    (a, f1) = f f0
    in runFileM (g a) f1

readChar :: FileM (!Bool, !Char)
readChar = FileM \f0. let
  (b, c, f1) = freadc f0
  in ((b, c), f1)

readFile :: FileM [Char]
readFile =
  readChar >>= \(b, c).
  if b (
    readFile >>= \cs.
    pure [c:cs]
  ) (pure [])

writeChar :: !Char -> FileM ()
writeChar c = FileM \f. ((), fwritec c f)

writeString :: !String -> FileM ()
writeString s = FileM \f. ((), fwrites s f)
