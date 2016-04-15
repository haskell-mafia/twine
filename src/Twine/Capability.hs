{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Capability (
    defaultCapability
  ) where


import           Control.Concurrent (getNumCapabilities, setNumCapabilities)

import           GHC.Conc (getNumProcessors)

import           P

import           System.IO



-- |
-- Default capabilities to number of processors if it is still at default of 1.
--
defaultCapability :: IO ()
defaultCapability = do
  x <- getNumCapabilities
  when (x == 1) $
    getNumProcessors >>= setNumCapabilities
