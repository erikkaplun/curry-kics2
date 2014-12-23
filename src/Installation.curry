--------------------------------------------------------------------------
--- Definition of some global installation infos.
--- The current installation infos are taken from the
--- file `ROOT/runtime/Installation.hs` which is automatically generated.
---
--- @author  Michael Hanus, Bjoern Peemoeller, Fabian Skrlac
--- @version September 2014
--- --------------------------------------------------------------------------

module Installation where

compilerName :: String
compilerName external

installDir :: String
installDir external

majorVersion :: Int
majorVersion external

minorVersion :: Int
minorVersion external

revisionVersion :: Int
revisionVersion external

compilerDate :: String
compilerDate external

installDate :: String
installDate external

runtime :: String
runtime external

runtimeMajor :: Int
runtimeMajor external

runtimeMinor :: Int
runtimeMinor external

ghcExec :: String
ghcExec external

ghcOptions :: String
ghcOptions external

installGlobal :: Bool
installGlobal external

withProfiling :: Bool
withProfiling external
