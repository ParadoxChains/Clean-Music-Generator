implementation module util.Monad.Result

import Util.Monad

instance Monad Result where
  pure a = Ok a
  (>>=) r f = case r of
    Err s -> Err s
    Ok a  -> f a
