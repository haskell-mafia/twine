import           Disorder.Core.Main

import           Test.Twine.Data.Duration
import           Test.Twine.Data.Finalizer
import           Test.Twine.Data.Parallel
import           Test.Twine.Data.Pin
import           Test.Twine.Data.Queue
import           Test.Twine.Parallel

main :: IO ()
main =
  disorderMain [
      Test.Twine.Data.Duration.tests
    , Test.Twine.Data.Finalizer.tests
    , Test.Twine.Data.Parallel.tests
    , Test.Twine.Data.Pin.tests
    , Test.Twine.Data.Queue.tests
    , Test.Twine.Parallel.tests
    ]
