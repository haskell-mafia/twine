{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Loop (
    loop
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           P

import           Twine.Data.Gate
import           Twine.Data.Duration
import           Twine.Snooze


-- | Loop with a delay until the gate is closed
--
loop :: MonadIO m => Duration -> Gate -> m () -> m ()
loop d c action = do
  action
  liftIO (isOpen c) >>= \b ->
    case b of
      False ->
        return ()
      True -> do
        liftIO $ snooze d
        loop d c action
