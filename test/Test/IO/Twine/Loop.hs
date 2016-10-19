{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Twine.Loop where

import           Control.Concurrent.Async (async)

import           Disorder.Core.IO

import           P

import           Test.QuickCheck

import           Twine.Async (waitWithTimeout)
import           Twine.Data.Gate
import           Twine.Data.Duration (milliseconds, seconds)
import           Twine.Loop

import           X.Control.Monad.Trans.Either (runEitherT)

prop_loop = testIO $ do
  g <- newGate
  x <- async $ loop (milliseconds 1) g (pure ()) >> pure True
  close g
  r <- runEitherT $ waitWithTimeout x (seconds 1)
  pure $ r === Right True

return []
tests = $quickCheckAll
