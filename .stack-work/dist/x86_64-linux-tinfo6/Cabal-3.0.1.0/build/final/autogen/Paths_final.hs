{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_final (
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

bindir     = "/home/andres/itba/funcional/final/.stack-work/install/x86_64-linux-tinfo6/8aec9e55b6c2d255b924b014a218e098c44f2a0dc140476b67e01d9021619f46/8.8.4/bin"
libdir     = "/home/andres/itba/funcional/final/.stack-work/install/x86_64-linux-tinfo6/8aec9e55b6c2d255b924b014a218e098c44f2a0dc140476b67e01d9021619f46/8.8.4/lib/x86_64-linux-ghc-8.8.4/final-0.1.0.0-Ei9CeVlTG5R8gvo4COC1uo-final"
dynlibdir  = "/home/andres/itba/funcional/final/.stack-work/install/x86_64-linux-tinfo6/8aec9e55b6c2d255b924b014a218e098c44f2a0dc140476b67e01d9021619f46/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/andres/itba/funcional/final/.stack-work/install/x86_64-linux-tinfo6/8aec9e55b6c2d255b924b014a218e098c44f2a0dc140476b67e01d9021619f46/8.8.4/share/x86_64-linux-ghc-8.8.4/final-0.1.0.0"
libexecdir = "/home/andres/itba/funcional/final/.stack-work/install/x86_64-linux-tinfo6/8aec9e55b6c2d255b924b014a218e098c44f2a0dc140476b67e01d9021619f46/8.8.4/libexec/x86_64-linux-ghc-8.8.4/final-0.1.0.0"
sysconfdir = "/home/andres/itba/funcional/final/.stack-work/install/x86_64-linux-tinfo6/8aec9e55b6c2d255b924b014a218e098c44f2a0dc140476b67e01d9021619f46/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "final_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "final_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "final_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "final_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "final_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "final_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
