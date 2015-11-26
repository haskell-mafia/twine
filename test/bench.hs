{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Control.Concurrent
import           Control.Monad.IO.Class (liftIO)

import           Criterion.Main

import           Twine.Data
import           Twine.Parallel

import           P

import           System.IO

import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either

run :: EitherT (RunError ()) IO () -> IO ()
run =
  eitherT (const $ fail "fail") pure

main :: IO ()
main = do
  let pro n = \q -> forM_ [1..n] (writeQueue q)

      work :: Int -> EitherT () IO ()
      work _ = liftIO $ do
        threadDelay 10000 {-- 10 ms --}

  defaultMain [
      bgroup "sem-100" [
          bench "work-1000" (nfIO . run $ consume (pro 1000) 100 work)
        ]
    , bgroup "sem-1000" [
          bench "work-1000" (nfIO . run $ consume (pro 1000) 1000 work)
        , bench "work-10000" (nfIO . run $ consume (pro 10000) 1000 work)
        , bench "work-100000" (nfIO . run $ consume (pro 100000) 1000 work)
        ]
    ]
