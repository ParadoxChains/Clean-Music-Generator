definition module Control.Monad.File

import Control.Monad

:: FileM a =: FileM (*File -> *(!a, !*File))

runFileM :: !(FileM a) !*File -> (a, !*File)

instance Monad FileM

readChar :: FileM (!Bool, !Char)
readFile :: FileM [Char]

writeChar   :: !Char   -> FileM ()
writeString :: !String -> FileM ()
