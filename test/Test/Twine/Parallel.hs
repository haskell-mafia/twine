{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Twine.Parallel where

import           Control.Concurrent.MVar
import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           Disorder.Core.IO

import           P

import           System.IO
import           System.IO.Error

import           Test.QuickCheck

import           Twine.Data
import           Twine.Parallel

import           X.Control.Monad.Trans.Either

data Fail =
  IFailed

prop_consume = forAll (choose (1, 10 :: Int)) $ \n -> testIO $ do
  r <- newMVar ([] :: [Int])

  let pro = \q -> forM_ [1..n] (writeQueue q)

      work :: Int -> EitherT Fail IO ()
      work i =
        liftIO $ modifyMVar_ r (\l -> pure $ i : l)

  _ <- runEitherT $ consume pro 1 work

  g <- readMVar r

  pure $ length g === n

prop_worker_fail = forAll (choose (1, 1000 :: Int)) $ \n -> testIO $ do
  let pro = \q -> forM_ [1..n] (writeQueue q)
      work = const $ left IFailed

  r <- runEitherT $ consume pro 1 work
  z <- pure $ case r of
    (Left (WorkerError IFailed)) ->
      True
    _ ->
      False
  pure $ z === True

prop_worker_blow_up_worker = forAll (choose (1, 1000 :: Int)) $ \n -> testIO $ do
  let pro = \q -> forM_ [1..n] (writeQueue q)
      work = const . throwM . userError $ "what is this?"

  r <- runEitherT $ consume pro 1 work
  z <- pure $ case r of
    (Left (BlowUpError _)) ->
      True
    _ ->
      False
  pure $ z === True

prop_worker_blow_up_producer = testIO $ do
  let pro = const . throwM . userError $ "producer"
      work = const $ pure ()

  r <- runEitherT $ consume pro 1 work
  z <- pure $ case r of
    (Left (BlowUpError _)) ->
      True
    _ ->
      False
  pure $ z === True

prop_empty_producer = testIO $ do
  let pro = const $ pure ()
      work = const $ pure ()

  r <- runEitherT $ consume pro 1 work
  pure $ (isRight r) === True


return []
tests :: IO Bool
tests = $quickCheckAll
