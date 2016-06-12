{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Simulate (
    MonadSimulate
  , whileExclusiveM
  , backtrack
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
backtrack :: (MonadSimulate m m') =>
             m Bool ->
             m a ->
             m a ->
             m' a
backtrack check altA altB = do
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


-- Instances

instance (Monad m) => MonadSimulate Identity m where
  simulate (Identity a) = return a
  perform (Identity a) = return a

instance (MonadSimulate m m', MonadState a m') => MonadSimulate (StateT a m) m' where
  simulate s = do
    state <- get
    simulate $ evalStateT s state
  perform s = do
    state <- get
    (a, s) <- perform $ runStateT s state
    put s
    return a

instance (MonadSimulate m m', MonadWriter w m') => MonadSimulate (WriterT w m) m' where
  simulate w = do
    (a, _) <- simulate $ runWriterT w
    return a
  perform w = do
    (a, w) <- perform $ runWriterT w
    tell w
    return a

instance (MonadSimulate m m', MonadReader r m') => MonadSimulate (ReaderT r m) m' where
  simulate r = do
    env <- ask
    simulate $ runReaderT r env
  perform r = do
    env <- ask
    perform $ runReaderT r env
