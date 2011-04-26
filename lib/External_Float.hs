import qualified Curry_Prelude as CP

external_d_C_prim_Float_plus :: CP.C_Float -> CP.C_Float -> CP.C_Float
external_d_C_prim_Float_plus y x =
  toCurry ((fromCurry x + fromCurry y) :: Float)

external_d_C_prim_Float_minus :: CP.C_Float -> CP.C_Float -> CP.C_Float
external_d_C_prim_Float_minus y x =
  toCurry ((fromCurry x - fromCurry y) :: Float)

external_d_C_prim_Float_times :: CP.C_Float -> CP.C_Float -> CP.C_Float
external_d_C_prim_Float_times y x =
  toCurry ((fromCurry x * fromCurry y) :: Float)

external_d_C_prim_Float_div :: CP.C_Float -> CP.C_Float -> CP.C_Float
external_d_C_prim_Float_div y x =
  toCurry ((fromCurry x / fromCurry y) :: Float)

external_d_C_prim_i2f :: CP.C_Int -> CP.C_Float
external_d_C_prim_i2f x = toCurry (fromInteger (fromCurry x) :: Float)

external_d_C_prim_truncate :: CP.C_Float -> CP.C_Int
external_d_C_prim_truncate x = toCurry (truncate (fromCurry x :: Float) :: Int)

external_d_C_prim_round :: CP.C_Float -> CP.C_Int
external_d_C_prim_round x = toCurry (round (fromCurry x :: Float) :: Int)

external_d_C_prim_sqrt :: CP.C_Float -> CP.C_Float
external_d_C_prim_sqrt x = toCurry (sqrt (fromCurry x :: Float))

external_d_C_prim_log :: CP.C_Float -> CP.C_Float
external_d_C_prim_log x = toCurry (log (fromCurry x :: Float))

external_d_C_prim_exp :: CP.C_Float -> CP.C_Float
external_d_C_prim_exp x = toCurry (exp (fromCurry x :: Float))

external_d_C_prim_sin :: CP.C_Float -> CP.C_Float
external_d_C_prim_sin x = toCurry (sin (fromCurry x :: Float))

external_d_C_prim_cos :: CP.C_Float -> CP.C_Float
external_d_C_prim_cos x = toCurry (cos (fromCurry x :: Float))

external_d_C_prim_tan :: CP.C_Float -> CP.C_Float
external_d_C_prim_tan x = toCurry (tan (fromCurry x :: Float))

external_d_C_prim_atan :: CP.C_Float -> CP.C_Float
external_d_C_prim_atan x = toCurry (atan (fromCurry x :: Float))

