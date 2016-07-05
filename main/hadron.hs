{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           BuildInfo_ambiata_hadron

import           DependencyInfo_ambiata_hadron

import qualified Data.ByteString as BS
import qualified Data.Text.IO as T

import           Hadron

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
        putStrLn ("hadron: " <> buildInfoVersion)
      DependencyCommand ->
        mapM_ putStrLn dependencyInfo
      RunCommand DryRun c ->
        print c
      RunCommand RealRun c ->
        run c

parser :: Parser HadronCommand
parser = subparser $
     command' "validate" "Verify that an HTTP request read from standard input is well-formed." (pure Validate)

run :: HadronCommand -> IO ()
run c = case c of
  Validate ->
    fmap parseHTTPRequest BS.getContents >>= \case
      Left e -> do
        T.putStrLn $ renderRequestError e
        exitFailure
      Right _ ->
        pure ()

data HadronCommand =
  Validate
  deriving (Eq, Show)
