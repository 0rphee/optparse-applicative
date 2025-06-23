{-# LANGUAGE Arrows, CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
module Examples.Cabal where

import Options.Applicative
import Options.Applicative.Arrows

import Data.Monoid

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString
import System.OsPath (OsPath)

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Args = Args CommonOpts Command
  deriving Show

data CommonOpts = CommonOpts
  { optVerbosity :: Int }
  deriving Show

data Command
  = Install ConfigureOpts InstallOpts
  | Update
  | Configure ConfigureOpts
  | Build BuildOpts
  deriving Show

data InstallOpts = InstallOpts
  { instReinstall :: Bool
  , instForce :: Bool }
  deriving Show

data ConfigureOpts = ConfigureOpts
  { configTests :: Bool
  , configFlags :: [OsString] }
  deriving Show

data BuildOpts = BuildOpts
  { buildDir :: OsPath }
  deriving Show


parser :: Parser Args
parser = runA $ proc () -> do
  opts <- asA commonOpts -< ()
  cmds <- (asA . hsubparser)
            ( command [osstr|install|]
              (info installParser
                    (progDesc [osstr|Installs a list of packages|]))
           <> command [osstr|update|]
              (info updateParser
                    (progDesc [osstr|Updates list of known packages|]))
           <> command [osstr|configure|]
              (info configureParser
                    (progDesc [osstr|Prepare to build the package|]))
           <> command [osstr|build|]
              (info buildParser
                    (progDesc [osstr|Make this package ready for installation|])) ) -< ()
  A (simpleVersioner [osstr|0.0.0|]) >>> A helper -< Args opts cmds

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
  <$> option auto
      ( short (OsString.unsafeFromChar 'v')
     <> long [osstr|verbose|]
     <> metavar [osstr|LEVEL|]
     <> help [osstr|Set verbosity to LEVEL|]
     <> value 0 )

installParser :: Parser Command
installParser = runA $ proc () -> do
  config <- asA configureOpts -< ()
  inst <- asA installOpts -< ()
  returnA -< Install config inst

installOpts :: Parser InstallOpts
installOpts = runA $ proc () -> do
  reinst <- asA (switch (long [osstr|reinstall|])) -< ()
  force <- asA (switch (long [osstr|force-reinstall|])) -< ()
  returnA -< InstallOpts
             { instReinstall = reinst
             , instForce = force }

updateParser :: Parser Command
updateParser = pure Update

configureParser :: Parser Command
configureParser = runA $ proc () -> do
  config <- asA configureOpts -< ()
  returnA -< Configure config

configureOpts :: Parser ConfigureOpts
configureOpts = runA $ proc () -> do
  tests <- (asA . switch)
             ( long [osstr|enable-tests|]
            <> help [osstr|Enable compilation of test suites|] ) -< ()
  flags <- (asA . many . strOption)
             ( short (OsString.unsafeFromChar 'f')
            <> long [osstr|flags|]
            <> metavar [osstr|FLAGS|]
            <> help [osstr|Enable the given flag|] ) -< ()
  returnA -< ConfigureOpts tests flags

buildParser :: Parser Command
buildParser = runA $ proc () -> do
  opts <- asA buildOpts -< ()
  returnA -< Build opts

buildOpts :: Parser BuildOpts
buildOpts = runA $ proc () -> do
  bdir <- (asA . strOption)
            ( long [osstr|builddir|]
           <> metavar [osstr|DIR|]
           <> value [osstr|dist|] ) -< ()
  returnA -< BuildOpts bdir

pinfo :: ParserInfo Args
pinfo = info parser
  ( progDesc [osstr|An example modelled on cabal|] )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) pinfo
  print r
