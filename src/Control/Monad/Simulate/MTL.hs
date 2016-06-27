{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Simulate.MTL where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity

import Control.Monad.Simulate.Simulate

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
