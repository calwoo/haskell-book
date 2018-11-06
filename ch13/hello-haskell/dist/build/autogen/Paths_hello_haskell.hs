{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hello_haskell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/mnt/c/Users/calvi/Dropbox/haskell/hffp/ch13/hello-haskell/.cabal-sandbox/bin"
libdir     = "/mnt/c/Users/calvi/Dropbox/haskell/hffp/ch13/hello-haskell/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/hello-haskell-0.1.0.0"
dynlibdir  = "/mnt/c/Users/calvi/Dropbox/haskell/hffp/ch13/hello-haskell/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/mnt/c/Users/calvi/Dropbox/haskell/hffp/ch13/hello-haskell/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/hello-haskell-0.1.0.0"
libexecdir = "/mnt/c/Users/calvi/Dropbox/haskell/hffp/ch13/hello-haskell/.cabal-sandbox/libexec"
sysconfdir = "/mnt/c/Users/calvi/Dropbox/haskell/hffp/ch13/hello-haskell/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hello_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hello_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hello_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hello_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hello_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hello_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
