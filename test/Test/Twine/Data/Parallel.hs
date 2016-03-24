{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Twine.Data.Parallel where

import           Disorder.Core.IO

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Twine.Data.Parallel

prop_short_circuit = testIO $ do
  r <- newResult (Right () :: Either Text ())
  _ <- addResult r (Left "fail")
  _ <- addResult r (Right ())
  z <- getResult r
  pure $ z === (Left "fail")

prop_read (a :: Int) = testIO $ do
  r <- newResult a
  z <- getResult r
  pure $ z === a

prop_read_read (a :: Int) = testIO $ do
  r <- newResult a
  _ <- getResult r
  z <- getResult r
  pure $ z === a

return []
tests :: IO Bool
tests = $quickCheckAll
