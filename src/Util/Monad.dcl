definition module Util.Monad

import StdMaybe


class Monad m where
  // Embed a pure expression.
  pure :: a -> m a

  // Sequentially compose two actions,
  // passing any value produced by the first as an argument to the second.
  (>>=) infixl 1 :: !(m a) (a -> m b) -> m b

  // Sequentially compose two actions,
  // discarding any value produced by the first,
  // like sequencing operators (such as the semicolon) in imperative languages.
  (>>>) infixl 1 :: !(m a) (m b) -> m b
  (>>>) ma mb :== ma >>= \_. mb

  // Given any types a and b lets you apply any function
  // from (a -> b) to turn an m a into an m b, preserving the structure of m.
  (<$>) infixl 4 :: (a -> b) !(m a) -> m b
  (<$>) f m :== m >>= \a. pure (f a)

  // Sequence computations and combine their results.
  (<*>) infixl 4 :: !(m (a -> b)) (m a) -> m b
  (<*>) mf ma :== mf >>= \f. ma >>= \a. pure (f a)

  // Replace all locations in the input with the same value.
  (<$) infixl 4 :: a !(m b) -> m a
  (<$) a m :== m >>= \_. pure a

  // Sequence actions, discarding the value of the second argument.
  (<*) infixl 4 :: !(m a) (m b) -> m a
  (<*) ma mb :== ma >>= \a. mb >>= \_. pure a

// Conditional execution of an expressions.
when :: !Bool (m a) -> m () | Monad m

// Map each element of a list to a monadic action,
// evaluate these actions from left to right, and collect the results.
mapM  :: (a -> m b) ![a] -> m [b] | Monad m

// Map each element of a structure to a monadic action,
// evaluate these actions from left to right, and ignore the results.
mapM_ :: (a -> m b) ![a] -> m ()  | Monad m

// replicateM n act performs the action n times, gathering the results.
replicateM :: !Int !(m a) -> m [a] | Monad m

// Like replicateM, but discards the result.
replicateM_ :: !Int !(m a) -> m () | Monad m

instance Monad Maybe
