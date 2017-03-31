{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Eff.Simulate (
    EffSimulate
  --, whileExclusiveM
  --, tryM
  ) where

import Control.Eff
import Control.Eff.State.Lazy
import Control.Eff.Writer.Lazy
import Control.Eff.Reader.Lazy
import Data.Void
import Data.Typeable

class EffSimulate r r' where
  -- Simulate m inside of m'. Do not modify the state of m'!
  simulate :: Eff r a -> Eff r' a
  -- Run m inside of m', including the effects.
  perform :: Eff r a -> Eff r' a

instance EffSimulate Void r where
  simulate voidEff = return $ run voidEff
  perform voidEff = return $ run voidEff

instance (EffSimulate r r', Member (State a) r', Typeable a) => EffSimulate (State a :> r) r' where
  simulate s = do
    state <- get
    simulate $ evalState state s
  perform s = do
    state <- get
    (s, a) <- perform $ runState state s
    put s
    return a

instance (EffSimulate r r', Member (Writer w) r', Typeable w) => EffSimulate (Writer w :> r) r' where
  simulate w = do
    (_, a) <- simulate $ runWriter undefined undefined w
    return a
  perform w = do
    (ws, a) <- perform $ runWriter (:) [] w
    mapM_ tell ws
    return a

instance (EffSimulate r r', Member (Reader s) r', Typeable s) => EffSimulate (Reader s :> r) r' where
  simulate r = do
    (env :: s) <- ask
    simulate $ runReader r env
  perform r = do
    (env :: s) <- ask
    perform $ runReader r env


-- Useful stuff

-- If the state is valid after running altA, run altA, otherwise run altB.
tryM :: (EffSimulate r r') =>
        Eff r Bool ->
        Eff r a ->
        Eff r a ->
        Eff r' a
tryM check altA altB = do
  isValid <- simulate (altA >> check)
  if isValid
    then perform altA
    else perform altB

-- Run op as many times as possible before the state becomes invalid.
whileExclusiveM :: EffSimulate r r' =>
                   Eff r Bool ->
                   Eff r a ->
                   Eff r' [a]
whileExclusiveM check op = do
  isValid <- simulate (op >> check)
  if isValid
    then do x <- perform op
            xs <- whileExclusiveM check op
            return (x : xs)
    else return []
