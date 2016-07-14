import           Disorder.Core.Main

import qualified Test.IO.Hadron.Wai.Request

main :: IO ()
main =
  disorderMain [
    Test.IO.Hadron.Wai.Request.tests
  ]
