import           Disorder.Core.Main

import           Test.IO.Twine.Snooze


main :: IO ()
main =
  disorderMain [
      Test.IO.Twine.Snooze.tests
    ]
