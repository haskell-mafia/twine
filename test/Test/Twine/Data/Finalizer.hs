{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Twine.Data.Finalizer where

import           Disorder.Core.IO

import           P

import           System.IO

import           Test.QuickCheck

import           Twine.Data.Finalizer
import           Twine.Data.Pin


prop_simple = once . testIO $ do
  p <- newPin
  let f = Finalizer $ pullPin p
  finalize f
  checkPin p

prop_monoid = once . testIO $ do
  p1 <- newPin
  p2 <- newPin
  let f = Finalizer (pullPin p1) <> Finalizer (pullPin p2)
  finalize f
  (&&) <$> checkPin p1 <*> checkPin p2


return []
tests :: IO Bool
tests = $quickCheckAll
