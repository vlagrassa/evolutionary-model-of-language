{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Final (
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

bindir     = "C:\\Users\\Vince\\UChicago\\Year 2 Quarter 1\\LING 26040-1\\Final\\.cabal-sandbox\\bin"
libdir     = "C:\\Users\\Vince\\UChicago\\Year 2 Quarter 1\\LING 26040-1\\Final\\.cabal-sandbox\\x86_64-windows-ghc-8.4.3\\Final-0.1.0.0-7vezDQQ7CWPJDFQfwZrX0O"
dynlibdir  = "C:\\Users\\Vince\\UChicago\\Year 2 Quarter 1\\LING 26040-1\\Final\\.cabal-sandbox\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\Vince\\UChicago\\Year 2 Quarter 1\\LING 26040-1\\Final\\.cabal-sandbox\\x86_64-windows-ghc-8.4.3\\Final-0.1.0.0"
libexecdir = "C:\\Users\\Vince\\UChicago\\Year 2 Quarter 1\\LING 26040-1\\Final\\.cabal-sandbox\\Final-0.1.0.0-7vezDQQ7CWPJDFQfwZrX0O\\x86_64-windows-ghc-8.4.3\\Final-0.1.0.0"
sysconfdir = "C:\\Users\\Vince\\UChicago\\Year 2 Quarter 1\\LING 26040-1\\Final\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Final_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Final_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Final_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Final_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Final_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Final_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
