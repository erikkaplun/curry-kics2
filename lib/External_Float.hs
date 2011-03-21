external_d_C_prim_Float_plus :: C_Float -> C_Float -> C_Float
external_d_C_prim_Float_plus x1 x2 =
  toCurry ((fromCurry x1 + fromCurry x1) :: Float)

external_d_C_prim_Float_minus :: C_Float -> C_Float -> C_Float
external_d_C_prim_Float_minus x1 x2 =
  toCurry ((fromCurry x1 - fromCurry x1) :: Float)

external_d_C_prim_Float_times :: C_Float -> C_Float -> C_Float
external_d_C_prim_Float_times x1 x2 =
  toCurry ((fromCurry x1 * fromCurry x1) :: Float)

external_d_C_prim_Float_div :: C_Float -> C_Float -> C_Float
external_d_C_prim_Float_div x1 x2 =
  toCurry ((fromCurry x1 / fromCurry x1) :: Float)

external_d_C_prim_i2f :: C_Int -> C_Float
external_d_C_prim_i2f x = toCurry (fromInteger (fromCurry x) :: Float)

external_d_C_prim_truncate :: C_Float -> C_Int
external_d_C_prim_truncate x = toCurry (truncate (fromCurry x :: Float) :: Int)

external_d_C_prim_round :: C_Float -> C_Int
external_d_C_prim_round x = toCurry (round (fromCurry x :: Float) :: Int)

external_d_C_prim_sqrt :: C_Float -> C_Float
external_d_C_prim_sqrt x = toCurry (sqrt (fromCurry x :: Float))

external_d_C_prim_log :: C_Float -> C_Float
external_d_C_prim_log x = toCurry (log (fromCurry x :: Float))

external_d_C_prim_exp :: C_Float -> C_Float
external_d_C_prim_exp x = toCurry (exp (fromCurry x :: Float))

external_d_C_prim_sin :: C_Float -> C_Float
external_d_C_prim_sin x = toCurry (sin (fromCurry x :: Float))

external_d_C_prim_cos :: C_Float -> C_Float
external_d_C_prim_cos x = toCurry (cos (fromCurry x :: Float))

external_d_C_prim_tan :: C_Float -> C_Float
external_d_C_prim_tan x = toCurry (tan (fromCurry x :: Float))

external_d_C_prim_atan :: C_Float -> C_Float
external_d_C_prim_atan x = toCurry (atan (fromCurry x :: Float))

