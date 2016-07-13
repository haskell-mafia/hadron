import           Disorder.Core.Main

import qualified Test.Hadron.Core.Data.Version
import qualified Test.Hadron.Core.Parser.Header
import qualified Test.Hadron.Core.Parser.Request
import qualified Test.Hadron.Core.Parser.Target
import qualified Test.Hadron.Core.Request

main :: IO ()
main =
  disorderMain [
      Test.Hadron.Core.Data.Version.tests
    , Test.Hadron.Core.Parser.Header.tests
    , Test.Hadron.Core.Parser.Request.tests
    , Test.Hadron.Core.Parser.Target.tests
    , Test.Hadron.Core.Request.tests
  ]
