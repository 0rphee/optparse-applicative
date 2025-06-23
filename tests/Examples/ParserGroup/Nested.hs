{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

module Examples.ParserGroup.Nested (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString

-- Nested groups. Demonstrates that group can nest.

data LogGroup = LogGroup
  { logPath :: Maybe OsString,
    systemGroup :: SystemGroup,
    logVerbosity :: Maybe Int
  }
  deriving (Show)

data SystemGroup = SystemGroup
  { poll :: Bool,
    deepNested :: Nested2,
    timeout :: Int
  }
  deriving (Show)

data Nested2 = Nested2
  { nested2Str :: OsString,
    nested3 :: Nested3
  }
  deriving (Show)

newtype Nested3 = Nested3
  { nested3Str :: OsString
  }
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    logGroup :: LogGroup,
    quiet :: Bool,
    verbosity :: Int,
    group2 :: (Int, Int),
    cmd :: OsString
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseHello
    <*> parseLogGroup
    <*> parseQuiet
    <*> parseVerbosity
    <*> parseGroup2
    <*> parseCmd

  where
    parseHello =
      strOption
        ( long [osstr|hello|]
            <> metavar [osstr|TARGET|]
            <> help [osstr|Target for the greeting|]
        )

    parseLogGroup =
      parserOptionGroup [osstr|First group|] $
      parserOptionGroup [osstr|Second group|] $
      parserOptionGroup [osstr|Logging|] $
        LogGroup
          <$> parseLogPath
          <*> parseSystemGroup
          <*> parseLogVerbosity

      where
        parseLogPath =
          optional
            ( strOption
                ( long [osstr|file-log-path|]
                    <> metavar [osstr|PATH|]
                    <> help [osstr|Log file path|]
                )
            )
        parseLogVerbosity =
          optional
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
          <$> switch (long [osstr|poll|] <> help [osstr|Whether to poll|])
          <*> parseNested2
          <*> option auto (long [osstr|timeout|] <> metavar [osstr|INT|] <> help [osstr|Whether to time out|])

    parseNested2 =
      parserOptionGroup [osstr|Nested2|] $
        Nested2
          <$> option str (long [osstr|double-nested|] <> metavar [osstr|STR|] <> help [osstr|Some nested option|])
          <*> parseNested3

    parseNested3 =
      parserOptionGroup [osstr|Nested3|] $
        Nested3 <$> option str (long [osstr|triple-nested|] <> metavar [osstr|STR|] <> help [osstr|Another option|])

    parseGroup2 :: Parser (Int, Int)
    parseGroup2 = parserOptionGroup [osstr|Group 2|] $
      (,)
        <$> parserOptionGroup [osstr|G 2.1|] (option auto (long [osstr|one|] <> help [osstr|Option 1|]))
        <*> parserOptionGroup [osstr|G 2.2|] (option auto (long [osstr|two|] <> help [osstr|Option 2|]))

    parseVerbosity =
      option auto (long [osstr|verbosity|] <> short (OsString.unsafeFromChar 'v') <> help [osstr|Console verbosity|])

    parseCmd =
      argument str (metavar [osstr|Command|])

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc [osstr|Nested parser groups|]
        <> header [osstr|parser_group.nested - a test for optparse-applicative|]
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
