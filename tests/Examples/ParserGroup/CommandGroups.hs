{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.ParserGroup.CommandGroups (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified System.OsString as OsString
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

data Command
  = Delete
  | List
  | Print
  | Query
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    logGroup :: LogGroup,
    quiet :: Bool,
    systemGroup :: SystemGroup,
    verbosity :: Int,
    cmd :: Command
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
    <*> parseCommand

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

    parseCommand =
      hsubparser
        ( command [osstr|list 2|] (info (pure List) $ progDesc [osstr|Lists elements|])
        )
        <|> hsubparser
        ( command [osstr|list|] (info (pure List) $ progDesc [osstr|Lists elements|])
            <> command [osstr|print|] (info (pure Print) $ progDesc [osstr|Prints table|])
            <> commandGroup [osstr|Info commands|]
        )
        <|> hsubparser
          ( command [osstr|delete|] (info (pure Delete) $ progDesc [osstr|Deletes elements|])
          )
        <|> hsubparser
          ( command [osstr|query|] (info (pure Query) $ progDesc [osstr|Runs a query|])
              <> commandGroup [osstr|Query commands|]
          )

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc [osstr|Option and command groups|]
        <> header [osstr|parser_group.command_groups - a test for optparse-applicative|]
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
