{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_CallCplusplus (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/garrett/Documents/HaskellExperiments/CallCplusplus/.stack-work/install/x86_64-linux-tinfo6/143f160a58567dbd37acc8b485e3ccf28d714616308dfb684f5a67ff14881e90/9.4.5/bin"
libdir     = "/home/garrett/Documents/HaskellExperiments/CallCplusplus/.stack-work/install/x86_64-linux-tinfo6/143f160a58567dbd37acc8b485e3ccf28d714616308dfb684f5a67ff14881e90/9.4.5/lib/x86_64-linux-ghc-9.4.5/CallCplusplus-0.1.0.0-FTqazGTKTvGCMvcYdalftm-CallCplusplus-exe"
dynlibdir  = "/home/garrett/Documents/HaskellExperiments/CallCplusplus/.stack-work/install/x86_64-linux-tinfo6/143f160a58567dbd37acc8b485e3ccf28d714616308dfb684f5a67ff14881e90/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/garrett/Documents/HaskellExperiments/CallCplusplus/.stack-work/install/x86_64-linux-tinfo6/143f160a58567dbd37acc8b485e3ccf28d714616308dfb684f5a67ff14881e90/9.4.5/share/x86_64-linux-ghc-9.4.5/CallCplusplus-0.1.0.0"
libexecdir = "/home/garrett/Documents/HaskellExperiments/CallCplusplus/.stack-work/install/x86_64-linux-tinfo6/143f160a58567dbd37acc8b485e3ccf28d714616308dfb684f5a67ff14881e90/9.4.5/libexec/x86_64-linux-ghc-9.4.5/CallCplusplus-0.1.0.0"
sysconfdir = "/home/garrett/Documents/HaskellExperiments/CallCplusplus/.stack-work/install/x86_64-linux-tinfo6/143f160a58567dbd37acc8b485e3ccf28d714616308dfb684f5a67ff14881e90/9.4.5/etc"

getBinDir     = catchIO (getEnv "CallCplusplus_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "CallCplusplus_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "CallCplusplus_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "CallCplusplus_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CallCplusplus_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CallCplusplus_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
