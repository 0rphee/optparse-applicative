{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

module Examples.ParserGroup.DuplicateCommandGroups (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString
import qualified System.OsString.IO as OIO

-- This test demonstrates that duplicate + consecutive groups are merged,
-- while duplicate + non-consecutive groups are not merged.

data Command
  = Delete
  | Insert
  | List
  | Print
  | Query
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    quiet :: Bool,
    verbosity :: Int,
    cmd :: Command
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseHello
    <*> parseQuiet
    <*> parseVerbosity
    <*> parseCommand

  where
    parseHello =
      strOption
        ( long [osstr|hello|]
            <> metavar [osstr|TARGET|]
            <> help [osstr|Target for the greeting|]
        )

    parseQuiet =
      switch
        ( long [osstr|quiet|]
            <> short (OsString.unsafeFromChar 'q')
            <> help [osstr|Whether to be quiet|]
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
        ( command [osstr|list|] (info (pure List) $ progDesc [osstr|Lists elements|])
            <> commandGroup [osstr|Info commands|]
        )
        <|> hsubparser
          ( command [osstr|delete|] (info (pure Delete) $ progDesc [osstr|Deletes elements|])
              <> commandGroup [osstr|Update commands|]
          )
        <|> hsubparser
          ( command [osstr|insert|] (info (pure Insert) $ progDesc [osstr|Inserts elements|])
              <> commandGroup [osstr|Update commands|]
          )
        <|> hsubparser
          ( command [osstr|query|] (info (pure Query) $ progDesc [osstr|Runs a query|])
          )
        <|> hsubparser
        ( command [osstr|print|] (info (pure Print) $ progDesc [osstr|Prints table|])
            <> commandGroup [osstr|Info commands|]
        )

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc [osstr|Duplicate consecutive command groups consolidated|]
        <> header [osstr|parser_group.duplicate_command_groups - a test for optparse-applicative|]
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
