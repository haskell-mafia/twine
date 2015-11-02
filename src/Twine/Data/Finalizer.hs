{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Data.Finalizer (
    Finalizer (..)
  ) where

import           P

import           System.IO


-- |
-- A callback which can be invoked to cleanup a resource
--
newtype Finalizer =
  Finalizer {
      finalize :: IO ()
    }

instance Monoid Finalizer where
  mempty =
    Finalizer $ pure ()
  mappend (Finalizer a) (Finalizer b) =
    Finalizer $ a >> b
