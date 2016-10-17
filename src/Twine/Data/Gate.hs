{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Data.Gate (
    Gate
  , newGate
  , isOpen
  , close
  ) where

import           Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)

import           P

import           System.IO

-- |
-- A gate is an abstract type, representing a simple barrier
-- that can only have its state queried in a non-blocking
-- manner.
--
-- This useful for implementing things like passing in a gate
-- to terminate or stop a loop.
--
newtype Gate =
  Gate {
      gate :: IORef Bool
    }

newGate :: IO Gate
newGate =
  Gate <$> newIORef True

isOpen :: Gate -> IO Bool
isOpen =
  readIORef . gate

close :: Gate -> IO ()
close =
  flip atomicWriteIORef False . gate
