{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Twine.Async where

import           Control.Concurrent.Async (async, wait, cancel)
import           Control.Monad.Catch (catchAll)

import           Disorder.Core.IO

import           P

import           System.IO

import           Test.QuickCheck

import           Twine.Async
import           Twine.Data
import           Twine.Snooze (snooze)

import           X.Control.Monad.Trans.Either

prop_wait_no_timeout = testIO $ do
  a <- async . snooze $ microseconds 5
  e <- runEitherT $ waitWithTimeout a (microseconds 10)
  pure $ e === Right ()

prop_wait_timeout = testIO $ do
  a <- async $ (snooze $ milliseconds 20)
  e <- runEitherT $ waitWithTimeout a (milliseconds 5)
  pure $ e === Left (AsyncTimeout (milliseconds 5))

prop_cancel_outside = testIO $ do
  a <- async $ (snooze (milliseconds 20) >> pure True)
  e <- async $ runEitherT $ waitWithTimeout a (milliseconds 5)
  cancel a
  r <- wait e `catchAll` (\_ -> pure . Right $ False)
  pure $ r === Right False

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
