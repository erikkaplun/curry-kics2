import qualified Installation as I

external_d_C_compilerName :: Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_compilerName _ _ = toCurry I.compilerName

external_d_C_installDir :: Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_installDir _ _ = toCurry I.installDir

external_d_C_majorVersion :: Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_majorVersion _ _ = toCurry I.majorVersion

external_d_C_minorVersion :: Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_minorVersion _ _ = toCurry I.minorVersion

external_d_C_revisionVersion :: Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_revisionVersion _ _ = toCurry I.revisionVersion

external_d_C_compilerDate :: Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_compilerDate _ _ = toCurry I.compilerDate

external_d_C_installDate :: Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_installDate _ _ = toCurry I.installDate

external_d_C_runtime :: Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_runtime _ _ = toCurry I.runtime

external_d_C_runtimeMajor :: Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_runtimeMajor _ _ = toCurry I.runtimeMajor

external_d_C_runtimeMinor :: Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_runtimeMinor _ _ = toCurry I.runtimeMinor

external_d_C_ghcExec :: Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_ghcExec _ _ = toCurry I.ghcExec

external_d_C_ghcOptions :: Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_ghcOptions _ _ = toCurry I.ghcOptions

external_d_C_installGlobal :: Cover -> ConstStore -> Curry_Prelude.C_Bool
external_d_C_installGlobal _ _ = toCurry I.installGlobal

external_d_C_withProfiling :: Cover -> ConstStore -> Curry_Prelude.C_Bool
external_d_C_withProfiling _ _ = toCurry I.withProfiling
