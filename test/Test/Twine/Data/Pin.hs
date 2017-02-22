{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Twine.Data.Pin where

import           Control.Concurrent.Async (async, waitBoth)

import           Disorder.Core.IO (testIO)

import           P

import           Test.QuickCheck (quickCheckAll, once, (===))

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
-- A pin can be pulled multiple times without blocking
--

prop_pull_pull = once . testIO $
  newPin >>= \p -> pullPin p >> pullPin p >> checkPin p >>= pure . (===) True

--
-- A pin can be pulled without blocking
--

prop_wait = once . testIO $
  newPin >>= \p -> pullPin p >> waitForPin p >> pure True


--
-- Multiple things can be blocked on a pin
--
prop_multiple = once . testIO $
  newPin >>= \p -> do
   x <- async $ waitForPin p
   y <- async $ waitForPin p
   pullPin p
   r <- waitBoth x y
   pure $ r === ((), ())

--
-- Multiple things can be blocked on a pin
--
prop_multiple_inline = once . testIO $
  newPin >>= \p ->
   pullPin p >> waitForPin p >> waitForPin p >> pure True


return []
tests = $quickCheckAll
