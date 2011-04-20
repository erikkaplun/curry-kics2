import Installation
import qualified Curry_Prelude as CP

external_d_C_curryCompiler :: CP.C_String
external_d_C_curryCompiler = toCurry "kics2"

external_d_C_curryCompilerMajorVersion :: CP.C_Int
external_d_C_curryCompilerMajorVersion = toCurry (majorVersion :: Int)

external_d_C_curryCompilerMinorVersion :: CP.C_Int
external_d_C_curryCompilerMinorVersion = toCurry (minorVersion ::Int)

external_d_C_curryRuntime :: CP.C_String
external_d_C_curryRuntime = toCurry "ghc"

external_d_C_curryRuntimeMajorVersion :: CP.C_Int
external_d_C_curryRuntimeMajorVersion = toCurry (6::Int)

external_d_C_curryRuntimeMinorVersion :: CP.C_Int
external_d_C_curryRuntimeMinorVersion = toCurry (12::Int)

external_d_C_installDir :: CP.C_String
external_d_C_installDir = toCurry installDir
