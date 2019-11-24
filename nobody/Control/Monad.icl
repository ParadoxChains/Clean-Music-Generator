implementation module Control.Monad

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
