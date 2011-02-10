import FlatCurry
import PrettyFlat
import FlatCurryGoodies
import System
import StyledText

prettyNd :: Prog -> IO String
prettyNd prog = do
  let instances = concatMap genInstance (progTypes prog)
  return $ unlines [showProg prog, instances]

genInstance :: TypeDecl -> String
genInstance (Type (_, n) _ vs _) = inst
  (plainText $ prettyTypeExpr "" (TCons ("",n) (map TVar vs))) (drop 2 n)
genInstance (TypeSyn _ _ _ _)  = ""

inst :: String -> String -> String
inst s t = "\ninstance NonDet (" ++ s ++ ") where\n" ++
           "  choiceCons = " ++ t ++ "_Choice\n" ++
           "  failCons   = " ++ t ++ "_Fail\n" ++
           "  guardCons  = " ++ t ++ "_Guard\n" ++
           "  try (" ++ t ++ "_Choice i x1 x2) = tryChoice i x1 x2\n" ++
           "  try " ++ t ++ "_Fail = Fail\n" ++
           "  try (" ++ t ++ "_Guard c e) = Guard c e\n" ++
           "  try v = Val v"
