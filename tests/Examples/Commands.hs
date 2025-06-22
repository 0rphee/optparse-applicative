{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Examples.Commands where

import Data.List
import Data.Monoid
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified System.OsString as OsString
import qualified System.OsString.IO as OIO

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Sample
  = Hello [OsString]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello = Hello <$> many (argument str (metavar [osstr|TARGET...|]))

sample :: Parser Sample
sample = subparser
       ( command [osstr|hello|]
         (info hello
               (progDesc [osstr|Print greeting|]))
      <> command [osstr|goodbye|]
         (info (pure Goodbye)
               (progDesc [osstr|Say goodbye|]))
       )
      <|> subparser
       ( command [osstr|bonjour|]
         (info hello
               (progDesc [osstr|Print greeting|]))
      <> command [osstr|au-revoir|]
         (info (pure Goodbye)
               (progDesc [osstr|Say goodbye|]))
      <> commandGroup [osstr|French commands:|]
      <> hidden
       )

run :: Sample -> IO ()
run (Hello targets) = OIO.putStrLn $ [osstr|Hello, |] <> OsString.intercalate [osstr|, |] targets <> [osstr|!|]
run Goodbye = OIO.putStrLn [osstr|Goodbye.|]

opts :: ParserInfo Sample
opts = info (sample <**> helper) idm

main :: IO ()
main = execParser opts >>= run
