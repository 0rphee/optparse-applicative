{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.ParserGroup.Duplicates (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified System.OsString as OsString

-- NOTE: This is the same structure as ParserGroup.Basic __except__
-- we have two (non-consecutive) "Logging" groups and two (consecutive)
-- System groups. This test demonstrates two things:
--
-- 1. Non-consecutive groups are not merged (i.e. we display two "Logging"
--    sections).
-- 2. Consecutive groups are merged (i.e. we display only one "System" group).
--
-- This is like command groups.

data LogGroup1 = LogGroup1
  { logPath :: Maybe OsString,
    logVerbosity :: Maybe Int
  }
  deriving (Show)

data LogGroup2 = LogGroup2
  { logNamespace :: OsString
  }
  deriving (Show)

data SystemGroup1 = SystemGroup1
  { poll :: Bool,
    timeout :: Int
  }
  deriving (Show)

newtype SystemGroup2 = SystemGroup2
  { sysFlag :: Bool
  }
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    logGroup1 :: LogGroup1,
    quiet :: Bool,
    systemGroup1 :: SystemGroup1,
    systemGroup2 :: SystemGroup2,
    logGroup2 :: LogGroup2,
    verbosity :: Int,
    cmd :: OsString
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseHello
    <*> parseLogGroup1
    <*> parseQuiet
    <*> parseSystemGroup1
    <*> parseSystemGroup2
    <*> parseLogGroup2
    <*> parseVerbosity
    <*> parseCmd

  where
    parseHello =
      strOption
        ( long [osstr|hello|]
            <> metavar [osstr|TARGET|]
            <> help [osstr|Target for the greeting|]
        )

    parseLogGroup1 =
      parserOptionGroup [osstr|Logging|] $
        LogGroup1
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

    parseSystemGroup1 =
      parserOptionGroup [osstr|System|] $
        SystemGroup1
          <$> switch
            ( long [osstr|poll|]
                <> help [osstr|Whether to poll|]
            )
          <*> option
                auto
                ( long [osstr|timeout|]
                    <> metavar [osstr|INT|]
                    <> help [osstr|Whether to time out|]
                )

    parseSystemGroup2 =
      parserOptionGroup [osstr|System|] $
        SystemGroup2
          <$> switch
            ( long [osstr|sysFlag|]
                <> help [osstr|Some flag|]
            )

    parseLogGroup2 =
      parserOptionGroup [osstr|Logging|] $
        LogGroup2
            <$>
              strOption
                ( long [osstr|log-namespace|]
                    <> metavar [osstr|STR|]
                    <> help [osstr|Log namespace|]
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
        <> progDesc [osstr|Duplicate consecutive groups consolidated|]
        <> header [osstr|parser_group.duplicates - a test for optparse-applicative|]
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r

