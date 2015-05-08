{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Twine.Snooze where

import           Data.Time

import           Disorder.Core.IO

import           P

import           System.IO

import           Test.QuickCheck

import           Twine.Snooze


prop_snooze = forAll (choose (1, 3)) $ \n -> testIO $ do
  s <- getCurrentTime
  snooze . seconds $ n
  e <- getCurrentTime
  let elapsed = diffUTCTime e s
  let expected = (fromInteger . toInteger) n
  pure . counterexample ("Elapsed: " <> show elapsed <> " , Snooze: " <> show expected) $
    elapsed >= expected && elapsed < expected + 1


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 3 })
