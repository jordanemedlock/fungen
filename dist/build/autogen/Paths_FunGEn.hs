module Paths_FunGEn (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,4,6,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jem/.cabal/bin"
libdir     = "/Users/jem/.cabal/lib/x86_64-osx-ghc-7.8.4/FunGEn-0.4.6.1"
datadir    = "/Users/jem/.cabal/share/x86_64-osx-ghc-7.8.4/FunGEn-0.4.6.1"
libexecdir = "/Users/jem/.cabal/libexec"
sysconfdir = "/Users/jem/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FunGEn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FunGEn_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "FunGEn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FunGEn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FunGEn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
