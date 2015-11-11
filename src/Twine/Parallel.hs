{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Twine.Parallel (
    RunError (..)
  , renderRunError
  , consume
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, cancel, poll, waitBoth)
import           Control.Concurrent.MSem (new, signal)
import qualified Control.Concurrent.MSem as M
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Loops (untilM_)

import           Data.Text (Text)
import qualified Data.Text as T

import           P

import           Twine.Data.Parallel
import           Twine.Data.Queue

import           System.IO

import           X.Control.Monad.Trans.Either


-- | Provide a producer and an action to be run across the result
--   of that producer in parallel.
--
--
--   Common usage:
--   @
--     let producer :: Address -> Queue Address -> IO ()
--         producer prefix q =
--           list' prefix $$ writeQueue q
--
--     consume producer 100 (\(a :: Address) -> doThis)
--   @
--
consume :: MonadIO m => (Queue b -> IO a) -> Int -> (b -> EitherT e IO ()) -> EitherT (RunError e) m a
consume pro fork action = EitherT . liftIO $ do
  q <- newQueue fork

  producer <- async $ pro q

  workers <- emptyWorkers
  result <- newResult $ Right ()
  sem <- new fork

  let spawn = do
       m <- tryReadQueue q
       flip (maybe $ return ()) m $ \a -> do
         w <- do
           M.wait sem
           async $ (runEitherT (action a) >>= void . addResult result . first WorkerError) `finally` signal sem
         addWorker workers w

  let check = do
       threadDelay 1000 {-- 1 ms --}
       p <- poll producer
       e <- isQueueEmpty q
       pure $ (isJust p) && e

  submitter <- async $ untilM_ spawn check

  -- early termination
  void . async . forever $ do
    threadDelay 1000000 {-- 1 second --}
    getResult result >>=
      either (const $ cancel producer >> cancel submitter) pure

  let waiter = do
        (i, _) <- waitBoth producer submitter
        waitForWorkers workers
        pure i

  (waiter >>= \i -> getResult result >>= pure . second (const $ i))
    `catchAll` (\z ->
      failWorkers workers >>
        getResult result >>= \w ->
          pure (w >> Left (BlowUpError z)))


data RunError a =
    WorkerError a
  | BlowUpError SomeException
  deriving Show

renderRunError :: RunError a -> (a -> Text) -> Text
renderRunError r render =
  case r of
    WorkerError a ->
      "Worker failed: " <> render a
    BlowUpError e ->
      "An unknown exception was caught: " <> T.pack (show e)
