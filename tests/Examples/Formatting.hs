{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
module Examples.Formatting where

import           Data.Monoid
import           Options.Applicative
import           Prelude

import System.OsString (osstr)
import qualified "os-string" System.OsString as OsString

opts :: Parser Int
opts = option auto $ mconcat
  [ long [osstr|test|]
  , short (OsString.unsafeFromChar 't')
  , value 0
  , metavar [osstr|FOO_BAR_BAZ_LONG_METAVARIABLE|]
  , help [osstr|This is an options with a very very long description.  Hopefully, this will be nicely formatted by the help text generator.|] ]
