{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_hadron
import           DependencyInfo_ambiata_hadron

import           Options.Applicative

import           P

import           System.IO
import           System.Exit
import           X.Options.Applicative

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch (safeCommand parser) >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      DependencyCommand ->
        mapM_ putStrLn dependencyInfo
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

parser :: Parser Command
parser = subparser $
     command' "validate" "Verify that an HTTP request read from standard input is well-formed." (pure Validate)

run :: Command -> IO ()
run c = case c of
  Validate ->
    putStrLn "*implement me*" >> exitFailure

data Command =
  Validate
  deriving (Eq, Show)
