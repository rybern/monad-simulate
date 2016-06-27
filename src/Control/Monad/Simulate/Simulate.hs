{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Simulate.Simulate (
    MonadSimulate (..)
  , whileExclusiveM
  , tryM
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity

class (Monad m, Monad m') => MonadSimulate m m' where
  -- Simulate m inside of m'. Do not modify the state of m'!
  simulate :: m a -> m' a
  -- Run m inside of m', including the effects.
  perform :: m a -> m' a

-- Useful stuff

-- If the state is valid after running altA, run altA, otherwise run altB.
tryM :: (MonadSimulate m m') =>
        m Bool ->
        m a ->
        m a ->
        m' a
tryM check altA altB = do
  isValid <- simulate (altA >> check)
  if isValid
    then perform altA
    else perform altB

-- Run op as many times as possible before the state becomes invalid.
whileExclusiveM :: MonadSimulate m m' =>
                   m Bool ->
                   m a ->
                   m' [a]
whileExclusiveM check op = do
  isValid <- simulate (op >> check)
  if isValid
    then do x <- perform op
            xs <- whileExclusiveM check op
            return (x : xs)
    else return []

instance (Monad m) => MonadSimulate Identity m where
  simulate (Identity a) = return a
  perform (Identity a) = return a
