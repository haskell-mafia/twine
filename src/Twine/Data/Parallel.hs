{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Data.Parallel (
    Workers
  , Result
  , newResult
  , addResult
  , getResult
  , emptyWorkers
  , failWorkers
  , getWorkers
  , addWorker
  , waitForWorkers
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

addResult :: Monad m => Result (m a) -> m a -> IO (m a)
addResult (Result r) w =
  modifyMVar r (\x -> pure $ ((x >> w), x))

getResult :: Result a -> IO a
getResult (Result r) =
  readMVar r

newtype Workers =
  Workers (MVar [Async ()])

emptyWorkers :: IO Workers
emptyWorkers =
  Workers <$> newMVar []

failWorkers :: Workers -> IO ()
failWorkers (Workers w) =
  readMVar w >>=
    mapM_ cancel

getWorkers :: Workers -> IO [Async ()]
getWorkers (Workers w) =
  readMVar w

addWorker :: Workers -> Async () -> IO ()
addWorker (Workers w) r =
  modifyMVar_ w $ pure . (:) r

waitForWorkers :: Workers -> IO ()
waitForWorkers (Workers w) =
  readMVar w >>= mapM_ wait
