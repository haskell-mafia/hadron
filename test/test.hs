import           Disorder.Core.Main

import qualified Test.Hadron.Data.Version
import qualified Test.Hadron.Parser.Header
import qualified Test.Hadron.Parser.Request
import qualified Test.Hadron.Parser.Target
import qualified Test.Hadron.Request

main :: IO ()
main =
  disorderMain [
      Test.Hadron.Data.Version.tests
    , Test.Hadron.Parser.Header.tests
    , Test.Hadron.Parser.Request.tests
    , Test.Hadron.Parser.Target.tests
    , Test.Hadron.Request.tests
  ]
