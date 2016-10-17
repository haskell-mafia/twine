import           Disorder.Core.Main

import qualified Test.IO.Twine.Guard
import qualified Test.IO.Twine.Loop
import qualified Test.IO.Twine.Snooze


main :: IO ()
main =
  disorderMain [
      Test.IO.Twine.Guard.tests
    , Test.IO.Twine.Loop.tests
    , Test.IO.Twine.Snooze.tests
    ]
