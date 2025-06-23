{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
module Examples.Hello where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (replicateM_)

import System.OsString (OsString, osstr)
import qualified System.OsString.IO as OIO
import qualified "os-string" System.OsString as OsString

data Sample = Sample
  { hello  :: OsString
  , quiet  :: Bool
  , repeat :: Int }
  deriving Show

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long [osstr|hello|]
         <> metavar [osstr|TARGET|]
         <> help [osstr|Target for the greeting|] )
      <*> switch
          ( long [osstr|quiet|]
         <> short (OsString.unsafeFromChar 'q')
         <> help [osstr|Whether to be quiet|] )
      <*> option auto
          ( long [osstr|repeat|]
         <> help [osstr|Repeats for greeting|]
         <> showDefault
         <> value 1
         <> metavar [osstr|INT|] )

main :: IO ()
main = greet =<< execParser opts

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc [osstr|Print a greeting for TARGET|]
  <> header [osstr|hello - a test for optparse-applicative|] )

greet :: Sample -> IO ()
greet (Sample h False n) = replicateM_ n . OIO.putStrLn $ [osstr|Hello, |] <> h
greet _ = return ()
