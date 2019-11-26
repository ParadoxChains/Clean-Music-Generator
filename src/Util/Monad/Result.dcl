definition module util.Monad.Result

import Util.Monad

:: Result a = Err !String | Ok !a

instance Monad Result
