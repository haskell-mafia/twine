{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Twine.Data.Pin where

import           Disorder.Core.IO

import           P

import           System.IO

import           Test.QuickCheck

import           Twine.Data.Pin


--
-- We are really just testing that our restricted use of MVar
-- is safe. The point of wrapping it up is preventing dead/live
-- locks, and generally make it more pleasant and less error-
-- prone
--


--
-- A new pin should not be pulled
--

prop_new = once . testIO $
  newPin >>= checkPin >>= pure . (===) False


--
-- A pulled pin should be pulled
--

prop_pull = once . testIO $
  newPin >>= \p -> pullPin p >> checkPin p >>= pure . (===) True


--
-- A pin can be pulled multiple tiumes without blocking
--

prop_pull_pull = once . testIO $
  newPin >>= \p -> pullPin p >> pullPin p >> checkPin p >>= pure . (===) True

return []
tests :: IO Bool
tests = $quickCheckAll
