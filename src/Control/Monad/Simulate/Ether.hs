{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Simulate.Ether where

import Control.Monad.Ether.State
import Control.Monad.Ether.Writer
import Control.Monad.Ether.Reader
import Control.Monad.Identity

import Data.Proxy

import Control.Monad.Simulate.Simulate

instance (MonadSimulate m m', MonadState tag a m') => MonadSimulate (StateT tag a m) m' where
  simulate s = do
    let tag' = Proxy :: Proxy tag
    state <- get tag'
    simulate $ evalStateT tag' s state
  perform s = do
    let tag' = Proxy :: Proxy tag
    state <- get tag'
    (a, s) <- perform $ runStateT tag' s state
    put tag' s
    return a

instance (MonadSimulate m m', MonadWriter tag w m') => MonadSimulate (WriterT tag w m) m' where
  simulate w = do
    let tag' = Proxy :: Proxy tag
    (a, _) <- simulate $ runWriterT tag' w
    return a
  perform w = do
    let tag' = Proxy :: Proxy tag
    (a, w) <- perform $ runWriterT tag' w
    tell tag' w
    return a

instance (MonadSimulate m m', MonadReader tag r m') => MonadSimulate (ReaderT tag r m) m' where
  simulate r = do
    let tag' = Proxy :: Proxy tag
    env <- ask tag'
    simulate $ runReaderT tag' r env
  perform r = do
    let tag' = Proxy :: Proxy tag
    env <- ask tag'
    perform $ runReaderT tag' r env
