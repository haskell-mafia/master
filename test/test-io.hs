import           Disorder.Core.Main

import           Test.IO.Master.Runner

main :: IO ()
main =
  disorderMain [
      Test.IO.Master.Runner.tests
    ]
