import FlatCurry
import PrettyFlat
import FlatCurryGoodies
import System
import StyledText

main = do
  (p:args) <- getArgs
  prog <- readFlatCurry p
  putStrLn "{-# LANGUAGE MagicHash, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}"
  putStrLn (showProg prog)
  mapIO printInstances (progTypes prog)
  readFile "./foot.hs" >>= putStrLn
  if null args
   then readFile "IDSupplyIORef.hs" >>= putStrLn
   else readFile (head args) >>= putStrLn

printInstances :: TypeDecl -> IO ()
printInstances (Type (_,n) _ vs _) = do
  putStrLn (inst (plainText $ prettyTypeExpr "" (TCons ("",n) (map TVar vs)))
                 (drop 2 n))
printInstances (TypeSyn _ _ _ _)  = return ()

inst s t = "instance NonDet (" ++ s ++ ") where\n" ++
           "  choiceCons = " ++ t ++ "_Choice\n" ++
           "  failCons   = " ++ t ++ "_Fail\n" ++
           "  guardCons  = " ++ t ++ "_Guard\n" ++
           "  try (" ++ t ++ "_Choice i x1 x2) = tryChoice i x1 x2\n" ++
           "  try " ++ t ++ "_Fail = Fail\n" ++
           "  try (" ++ t ++ "_Guard c e) = Guard c e\n" ++
           "  try v = Val v"