definition module util.Monad

import StdMaybe

class Monad m where
  pure :: a -> m a
  (>>=) infixl 1 :: !(m a) (a -> m b) -> m b

  (>>>) infixl 1 :: !(m a) (m b) -> m b
  (>>>) ma mb :== ma >>= \_. mb

  (<$>) infixl 4 :: (a -> b) !(m a) -> m b
  (<$>) f m :== m >>= \a. pure (f a)

  (<*>) infixl 4 :: !(m (a -> b)) (m a) -> m b
  (<*>) mf ma :== mf >>= \f. ma >>= \a. pure (f a)

  (<$) infixl 4 :: a !(m b) -> m a
  (<$) a m :== m >>= \_. pure a

  (<*) infixl 4 :: (m a) !(m b) -> m a
  (<*) ma mb :== ma >>= \a. mb >>= \_. pure a

when :: !Bool (m a) -> m () | Monad m

mapM  :: (a -> m b) ![a] -> m [b] | Monad m
mapM_ :: (a -> m b) ![a] -> m ()  | Monad m

replicateM :: !Int !(m a) -> m [a] | Monad m

instance Monad Maybe
