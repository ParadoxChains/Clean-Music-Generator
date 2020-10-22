implementation module Util.Monad

import StdEnv
import StdMaybe


when :: !Bool (m a) -> m () | Monad m
when b m
  | b = m >>> pure ()
  = pure ()

mapM :: (a -> m b) ![a] -> m [b] | Monad m
mapM f xs = go xs where
  go []     = pure []
  go [x:xs] =
    f x >>= \y.
    go xs >>= \ys.
    pure [y:ys]

mapM_ :: (a -> m b) ![a] -> m () | Monad m
mapM_ f xs = go xs where
  go []     = pure ()
  go [x:xs] =
    f x >>>
    go xs >>>
    pure ()

replicateM :: !Int !(m a) -> m [a] | Monad m
replicateM i m = go i where
  go i
    | i <= 0 = pure []
    = m >>= \x.
      go (i - 1) >>= \xs.
      pure [x:xs]

replicateM_ :: !Int !(m a) -> m () | Monad m
replicateM_ i m = go i where
  go i
    | i <= 0 = pure ()
    = m >>> go (i - 1)

instance Monad Maybe where
  pure a = Just a
  (>>=) m f = case m of
    Nothing -> Nothing
    Just a  -> f a
