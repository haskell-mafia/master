import           Disorder.Core.Main

import           Test.Master.Data
import           Test.Master.Serial.Toml

main :: IO ()
main =
  disorderMain [
      Test.Master.Data.tests
    , Test.Master.Serial.Toml.tests
    ]
