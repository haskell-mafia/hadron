import           Disorder.Core.Main

import qualified Test.Hadron.Data.Version

main :: IO ()
main =
  disorderMain [
      Test.Hadron.Data.Version.tests
  ]
