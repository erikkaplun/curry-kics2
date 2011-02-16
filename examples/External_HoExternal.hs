-- external_c_C_problem :: (t0 -> t1) -> t0 -> t1
-- external_c_C_problem x1 x2 = x1 x2

external_c_C_problem :: Func t0 t1 -> t0 -> IDSupply -> t1
external_c_C_problem (Func f) x s = f x s
