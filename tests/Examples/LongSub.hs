{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Examples.LongSub where

import Data.Monoid
import Options.Applicative

import System.OsString (OsString, osstr)

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Sample
  = Hello [OsString]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello =
  Hello
    <$> many (argument str (metavar [osstr|TARGET...|]))
    <*  switch (long [osstr|first-flag|])
    <*  switch (long [osstr|second-flag|])
    <*  switch (long [osstr|third-flag|])
    <*  switch (long [osstr|fourth-flag|])

sample :: Parser Sample
sample = hsubparser
       ( command [osstr|hello-very-long-sub|]
         (info hello
               (progDesc [osstr|Print greeting|]))
       )

opts :: ParserInfo Sample
opts = info (sample <**> helper) idm
