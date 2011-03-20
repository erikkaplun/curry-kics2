import Installation

external_d_C_curryCompiler :: C_String
external_d_C_curryCompiler = toCurry "idc"

external_d_C_curryCompilerMajorVersion :: C_Int
external_d_C_curryCompilerMajorVersion = toCurry (majorVersion :: Int)

external_d_C_curryCompilerMinorVersion :: C_Int
external_d_C_curryCompilerMinorVersion = toCurry (minorVersion ::Int)

external_d_C_curryRuntime :: C_String
external_d_C_curryRuntime = toCurry "ghc"

external_d_C_curryRuntimeMajorVersion :: C_Int
external_d_C_curryRuntimeMajorVersion = toCurry (6::Int)

external_d_C_curryRuntimeMinorVersion :: C_Int
external_d_C_curryRuntimeMinorVersion = toCurry (12::Int)

external_d_C_installDir :: C_String
external_d_C_installDir = toCurry installDir
