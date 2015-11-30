{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Data.Parallel (
    Workers
  , Result
  , newResult
  , emptyResult
  , addResult
  , getResult
  , emptyWorkers
  , failWorkers
  , getWorkers
  , addWorker
  , waitForWorkers
  , waitForWorkers'
  ) where

import           Control.Concurrent.Async (Async, cancel, wait)
import           Control.Concurrent.MVar

import           P

import           System.IO

newtype Result a =
  Result (MVar a)

newResult :: a -> IO (Result a)
newResult a =
  Result <$> newMVar a

-- fix pair with takeMVar
emptyResult :: IO (Result a)
emptyResult =
  Result <$> newEmptyMVar

addResult :: Monad m => Result (m a) -> m a -> IO (m a)
addResult (Result r) w =
  modifyMVar r (\x -> pure $ ((x >> w), x))

getResult :: Result a -> IO a
getResult (Result r) =
  readMVar r

newtype Workers a =
  Workers (MVar [Async a])

emptyWorkers :: IO (Workers a)
emptyWorkers =
  Workers <$> newMVar []

failWorkers :: Workers a -> IO ()
failWorkers (Workers w) =
  readMVar w >>=
    mapM_ cancel

getWorkers :: Workers a -> IO [Async a]
getWorkers (Workers w) =
  readMVar w

addWorker :: (Workers a) -> Async a -> IO ()
addWorker (Workers w) r =
  modifyMVar_ w $ pure . (:) r

waitForWorkers :: (Workers a) -> IO ()
waitForWorkers (Workers w) =
  readMVar w >>= mapM_ wait

waitForWorkers' :: (Workers a) -> IO [a]
waitForWorkers' (Workers w) =
  readMVar w >>= mapM wait
