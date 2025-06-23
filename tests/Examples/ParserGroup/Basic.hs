{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

module Examples.ParserGroup.Basic (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString
import qualified System.OsString.IO as OIO

data LogGroup = LogGroup
  { logPath :: Maybe OsString,
    logVerbosity :: Maybe Int
  }
  deriving (Show)

data SystemGroup = SystemGroup
  { poll :: Bool,
    timeout :: Int
  }
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    logGroup :: LogGroup,
    quiet :: Bool,
    systemGroup :: SystemGroup,
    verbosity :: Int,
    cmd :: OsString
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseHello
    <*> parseLogGroup
    <*> parseQuiet
    <*> parseSystemGroup
    <*> parseVerbosity
    <*> parseCmd

  where
    parseHello =
      strOption
        ( long [osstr|hello|]
            <> metavar [osstr|TARGET|]
            <> help [osstr|Target for the greeting|]
        )

    parseLogGroup =
      parserOptionGroup [osstr|Logging|] $
        LogGroup
          <$> optional
            ( strOption
                ( long [osstr|file-log-path|]
                    <> metavar [osstr|PATH|]
                    <> help [osstr|Log file path|]
                )
            )
          <*> optional
            ( option
                auto
                ( long [osstr|file-log-verbosity|]
                    <> metavar [osstr|INT|]
                    <> help [osstr|File log verbosity|]
                )
            )

    parseQuiet =
      switch
        ( long [osstr|quiet|]
            <> short (OsString.unsafeFromChar 'q')
            <> help [osstr|Whether to be quiet|]
        )

    parseSystemGroup =
      parserOptionGroup [osstr|System Options|] $
        SystemGroup
          <$> switch
            ( long [osstr|poll|]
                <> help [osstr|Whether to poll|]
            )
          <*> ( option
                  auto
                  ( long [osstr|timeout|]
                      <> metavar [osstr|INT|]
                      <> help [osstr|Whether to time out|]
                  )
              )

    parseVerbosity =
      option
        auto
        ( long [osstr|verbosity|]
            <> short (OsString.unsafeFromChar 'v')
            <> help [osstr|Console verbosity|]
        )

    parseCmd = argument str (metavar [osstr|Command|])

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc [osstr|Shows parser groups|]
        <> header [osstr|parser_group.basic - a test for optparse-applicative|]
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
