import           Disorder.Core.Main

main :: IO ()
main =
  disorderCliMain [
      "./dist/build/hadron/hadron"
    ]
