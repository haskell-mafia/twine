{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Twine.Data.Queue where

import           Disorder.Core.IO

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Twine.Data.Queue

prop_read_write a = testIO $ do
  q <- newQueue 1
  writeQueue q a
  r <- readQueue q
  pure $ r === a

prop_empty = testIO $ do
  q <- newQueue 1
  e <- isQueueEmpty q
  pure $ e === True

prop_try_read_non_blocking = testIO $ do
  q <- newQueue 1
  m <- tryReadQueue q
  pure $ m === (Nothing :: Maybe Int)


return []
tests :: IO Bool
tests = $quickCheckAll
