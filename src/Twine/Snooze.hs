{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Snooze (
    snooze
  , module Twine.Data.Duration
  ) where

import           Control.Concurrent

import           P

import           System.IO

import           Twine.Data.Duration


snooze :: Duration -> IO ()
snooze =
  threadDelay . toMicroseconds
