import           Disorder.Core.Main

import qualified Test.IO.Twine.Guard
import qualified Test.IO.Twine.Snooze


main :: IO ()
main =
  disorderMain [
      Test.IO.Twine.Guard.tests
    , Test.IO.Twine.Snooze.tests
    ]
