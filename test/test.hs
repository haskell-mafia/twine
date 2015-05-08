import           Disorder.Core.Main

import           Test.Twine.Data.Duration
import           Test.Twine.Data.Pin

main :: IO ()
main =
  disorderMain [
      Test.Twine.Data.Duration.tests
    , Test.Twine.Data.Pin.tests
    ]
