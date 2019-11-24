definition module Control.Monad

class Monad m where
  pure :: a -> m a
  (>>=) infixl 1 :: !(m a) (a -> m b) -> m b

  (>>>) infixl 1 :: !(m a) (m b) -> m b
  (>>>) ma mb :== ma >>= \_. mb

when :: !Bool (m a) -> m () | Monad m

mapM  :: (a -> m b) ![a] -> m [b] | Monad m
mapM_ :: (a -> m b) ![a] -> m ()  | Monad m
