{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_snake (
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

bindir     = "C:\\Users\\nicki\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\nicki\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\snake-0.1.0.0-inplace"
dynlibdir  = "C:\\Users\\nicki\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\nicki\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\snake-0.1.0.0"
libexecdir = "C:\\Users\\nicki\\AppData\\Roaming\\cabal\\snake-0.1.0.0-inplace\\x86_64-windows-ghc-9.0.1\\snake-0.1.0.0"
sysconfdir = "C:\\Users\\nicki\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "snake_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "snake_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "snake_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "snake_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "snake_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "snake_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
