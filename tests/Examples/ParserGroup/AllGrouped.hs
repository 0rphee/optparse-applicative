{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.ParserGroup.AllGrouped (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)

-- Tests the help page when every option belongs to some group i.e. there are
-- no top-level options. Notice we put the helper (<**> helper) __inside__
-- one of the groups, so that it is not a top-level option.
--
-- Also notice that although we add cmdParser to the same group, it is __not__
-- rendered as part of this group. This is what we want, as it is an Argument
-- and should not be rendered with the Options.

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
  { logGroup :: LogGroup,
    systemGroup :: SystemGroup,
    cmd :: OsString
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseLogGroup
    <*> parseSystemGroup
    <*> parseCmd

  where
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
            <**> helper

    parseSystemGroup =
      parserOptionGroup [osstr|System Options|] $
        SystemGroup
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

    parseCmd = argument str (metavar [osstr|Command|])

opts :: ParserInfo Sample
opts =
  info
    sample
    ( fullDesc
        <> progDesc [osstr|Every option is grouped|]
        <> header [osstr|parser_group.all_grouped - a test for optparse-applicative|]
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
