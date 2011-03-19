import GHC.Exts (Char (C#))

external_d_C_prim_showTerm :: Show a => a -> C_String
external_d_C_prim_showTerm t = toCurry (show t)

external_d_C_prim_showQTerm :: Show a => a -> C_String
external_d_C_prim_showQTerm t = toCurry (show t)

external_d_C_prim_readsUnqualifiedTerm :: Read a => OP_List C_String
                                -> C_String -> OP_List (OP_Tuple2 a C_String)
external_d_C_prim_readsUnqualifiedTerm _ = external_d_C_prim_readsQTerm

external_d_C_prim_readsQTerm :: Read a => C_String
                                       -> OP_List (OP_Tuple2 a C_String)
external_d_C_prim_readsQTerm s = toCurryPairs (reads (fromCurry s))
  where 
   toCurryPairs [] = OP_List
   toCurryPairs ((v,s):xs) = OP_Cons (OP_Tuple2 v (toCurry s)) (toCurryPairs xs)
