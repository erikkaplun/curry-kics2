import GHC.Exts (Char (C#))

fromChar :: Char -> C_Char
fromChar (C# c) = C_Char c

toChar :: C_Char -> Char
toChar (C_Char c) = C# c
toChar _          = error "Curry_Prelude.toChar with no ground term"

fromString :: String -> C_String
fromString [] = OP_List
fromString (c:cs) = OP_Cons (fromChar c) (fromString cs)

toString :: C_String -> String
toString OP_List = []
toString (OP_Cons c cs) = toChar c : toString cs
toString _ = error "Curry_Prelude.toString with no ground term"

external_d_C_prim_showTerm :: Show a => a -> C_String
external_d_C_prim_showTerm t = fromString (show t)

external_d_C_prim_showQTerm :: Show a => a -> C_String
external_d_C_prim_showQTerm t = fromString (show t)

external_d_C_prim_readsUnqualifiedTerm :: Read a => OP_List C_String
                                -> C_String -> OP_List (OP_Tuple2 a C_String)
external_d_C_prim_readsUnqualifiedTerm _ = external_d_C_prim_readsQTerm

external_d_C_prim_readsQTerm :: Read a => C_String
                                       -> OP_List (OP_Tuple2 a C_String)
external_d_C_prim_readsQTerm s = toCurry (reads (toString s))
  where 
   toCurry [] = OP_List
   toCurry ((v,s):xs) = OP_Cons (OP_Tuple2 v (fromString s)) (toCurry xs)
