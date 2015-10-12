import           Disorder.Core.Main

import           Test.Master.Serial.Toml

main :: IO ()
main =
  disorderMain [
      Test.Master.Serial.Toml.tests
    ]
