{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Twine.Guard where

import           Control.Concurrent
import           Control.Monad.Catch

import           Data.Text (Text)

import           Disorder.Core.IO

import           P

import           System.IO
import           System.IO.Error

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Twine.Guard

import           X.Control.Monad.Trans.Either

data HResult =
  HExplosion | HError Text | HGraceful deriving (Eq, Show)

handler v = TerminationHandler {
    onExplosion = const $ putMVar v HExplosion >> pure Die
  , onError = \t -> putMVar v (HError t) >> pure Die
  , onGraceful = putMVar v HGraceful >> pure Die
  }

prop_guard_graceful =
  testIO $ do
    v <- newEmptyMVar
    withThread v (pure ()) $ do
      r <- takeMVar v
      pure $ r === HGraceful

prop_guard_error t =
  testIO $ do
    v <- newEmptyMVar
    withThread v (left t) $ do
      r <- takeMVar v
      pure $ r === HError t

prop_guard_explosion =
  testIO $ do
    v <- newEmptyMVar
    withThread v (throwM $ userError "explodez") $ do
      r <- takeMVar v
      pure $ r === HExplosion

withThread :: MVar HResult -> EitherT Text IO () -> IO a -> IO a
withThread v action =
  bracket (forkIO $ guarded (handler v) action) (killThread) . const

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 3 })
