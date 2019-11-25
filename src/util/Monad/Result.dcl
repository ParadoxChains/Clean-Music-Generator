definition module util.Monad.Result

import util.Monad

:: Result a = Err !String | Ok !a

instance Monad Result
