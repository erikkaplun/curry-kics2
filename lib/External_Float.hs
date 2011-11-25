import qualified Curry_Prelude as CP

external_d_C_prim_Float_plus :: CP.C_Float -> CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_Float_plus y x _ =
  toCurry ((fromCurry x + fromCurry y) :: Float)

external_d_C_prim_Float_minus :: CP.C_Float -> CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_Float_minus y x _ =
  toCurry ((fromCurry x - fromCurry y) :: Float)

external_d_C_prim_Float_times :: CP.C_Float -> CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_Float_times y x _ =
  toCurry ((fromCurry x * fromCurry y) :: Float)

external_d_C_prim_Float_div :: CP.C_Float -> CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_Float_div y x _ =
  toCurry ((fromCurry x / fromCurry y) :: Float)

external_d_C_prim_i2f :: CP.C_Int -> ConstStore -> CP.C_Float
external_d_C_prim_i2f x _ = toCurry (fromInteger (fromCurry x) :: Float)

external_d_C_prim_truncate :: CP.C_Float -> ConstStore -> CP.C_Int
external_d_C_prim_truncate x _ = toCurry (truncate (fromCurry x :: Float) :: Int)

external_d_C_prim_round :: CP.C_Float -> ConstStore -> CP.C_Int
external_d_C_prim_round x _ = toCurry (round (fromCurry x :: Float) :: Int)

external_d_C_prim_sqrt :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_sqrt x _ = toCurry (sqrt (fromCurry x :: Float))

external_d_C_prim_log :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_log x _ = toCurry (log (fromCurry x :: Float))

external_d_C_prim_exp :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_exp x _ = toCurry (exp (fromCurry x :: Float))

external_d_C_prim_sin :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_sin x _ = toCurry (sin (fromCurry x :: Float))

external_d_C_prim_cos :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_cos x _ = toCurry (cos (fromCurry x :: Float))

external_d_C_prim_tan :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_tan x _ = toCurry (tan (fromCurry x :: Float))

external_d_C_prim_atan :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_atan x _ = toCurry (atan (fromCurry x :: Float))

