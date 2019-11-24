definition module Control.Monad.File

import Control.Monad

:: FileM a =: FileM (*File -> *(!a, !*File))

instance Monad FileM

runFileM :: !(FileM a) !*File -> (a, !*File)

readChar :: FileM (!Bool, !Char)
readFile :: FileM [Char]

writeChar   :: !Char   -> FileM ()
writeString :: !String -> FileM ()
