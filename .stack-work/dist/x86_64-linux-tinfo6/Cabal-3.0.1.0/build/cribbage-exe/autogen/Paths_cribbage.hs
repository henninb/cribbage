{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cribbage (
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

bindir     = "/home/henninb/projects/cribbage/.stack-work/install/x86_64-linux-tinfo6/b858d31814868e6ef9ebdd3d413c9a72f5eefb7a533a6ab6a6d6c5cd7494f50c/8.8.4/bin"
libdir     = "/home/henninb/projects/cribbage/.stack-work/install/x86_64-linux-tinfo6/b858d31814868e6ef9ebdd3d413c9a72f5eefb7a533a6ab6a6d6c5cd7494f50c/8.8.4/lib/x86_64-linux-ghc-8.8.4/cribbage-0.1.0.0-2hPZa7NdiWGKli8XKq95ZZ-cribbage-exe"
dynlibdir  = "/home/henninb/projects/cribbage/.stack-work/install/x86_64-linux-tinfo6/b858d31814868e6ef9ebdd3d413c9a72f5eefb7a533a6ab6a6d6c5cd7494f50c/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/henninb/projects/cribbage/.stack-work/install/x86_64-linux-tinfo6/b858d31814868e6ef9ebdd3d413c9a72f5eefb7a533a6ab6a6d6c5cd7494f50c/8.8.4/share/x86_64-linux-ghc-8.8.4/cribbage-0.1.0.0"
libexecdir = "/home/henninb/projects/cribbage/.stack-work/install/x86_64-linux-tinfo6/b858d31814868e6ef9ebdd3d413c9a72f5eefb7a533a6ab6a6d6c5cd7494f50c/8.8.4/libexec/x86_64-linux-ghc-8.8.4/cribbage-0.1.0.0"
sysconfdir = "/home/henninb/projects/cribbage/.stack-work/install/x86_64-linux-tinfo6/b858d31814868e6ef9ebdd3d413c9a72f5eefb7a533a6ab6a6d6c5cd7494f50c/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cribbage_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cribbage_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cribbage_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cribbage_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cribbage_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cribbage_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
