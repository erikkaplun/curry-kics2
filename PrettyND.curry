import FlatCurry
import PrettyFlat
import FlatCurryGoodies
import System
import StyledText

prettyNd :: Prog -> IO String
prettyNd prog = do
  let pragmas = "{-# LANGUAGE MagicHash, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}"
  let instances = concatMap genInstance (progTypes prog)
  foot <- readFile "./foot.hs"
  idsupply <- readFile "IDSupplyIORef.hs"
  return $ pragmas ++ '\n':showProg prog ++ '\n':instances ++ '\n':foot ++ '\n':idsupply

genInstance :: TypeDecl -> String
genInstance (Type (_,n) _ vs _) = inst
  (plainText $ prettyTypeExpr "" (TCons ("",n) (map TVar vs))) (drop 2 n)
genInstance (TypeSyn _ _ _ _)  = ""

inst :: String -> String -> String
inst s t = "instance NonDet (" ++ s ++ ") where\n" ++
           "  choiceCons = " ++ t ++ "_Choice\n" ++
           "  failCons   = " ++ t ++ "_Fail\n" ++
           "  guardCons  = " ++ t ++ "_Guard\n" ++
           "  try (" ++ t ++ "_Choice i x1 x2) = tryChoice i x1 x2\n" ++
           "  try " ++ t ++ "_Fail = Fail\n" ++
           "  try (" ++ t ++ "_Guard c e) = Guard c e\n" ++
           "  try v = Val v\n\n"