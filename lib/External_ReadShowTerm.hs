import GHC.Exts (Char (C#))
import qualified Curry_Prelude as CP

external_d_C_prim_showTerm :: Show a => a -> ConstStore -> CP.C_String
external_d_C_prim_showTerm t _ = toCurry (show t)

external_d_C_prim_showQTerm :: Show a => a -> ConstStore -> CP.C_String
external_d_C_prim_showQTerm t _= toCurry (show t)

external_d_C_prim_readsUnqualifiedTerm ::
  Read a => CP.OP_List CP.C_String -> CP.C_String ->
         ConstStore -> CP.OP_List (CP.OP_Tuple2 a CP.C_String)
external_d_C_prim_readsUnqualifiedTerm _ = external_d_C_prim_readsQTerm

external_d_C_prim_readsQTerm :: Read a => CP.C_String -> 
                             ConstStore -> CP.OP_List (CP.OP_Tuple2 a CP.C_String)
external_d_C_prim_readsQTerm s _ = toCurryPairs (reads (fromCurry s))
  where 
   toCurryPairs [] = CP.OP_List
   toCurryPairs ((v,s):xs) = CP.OP_Cons (CP.OP_Tuple2 v (toCurry s))
                                        (toCurryPairs xs)
