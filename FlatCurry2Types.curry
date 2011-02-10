------------------------------------------------------------------------
--- Generation of data and instance declarations for the Curry->Haskell
--- compiler.
--- The input is a FlatCurry program and the output is an AbstractHaskell
--- program with instance declarations that can be easily pretty printed
------------------------------------------------------------------------

import qualified FlatCurry as FC
import FlatCurryGoodies
import AbstractHaskell
--import PrettyAbstract
import AbstractHaskellPrinter
import AbstractHaskellGoodies
import System
import FileGoodies(stripSuffix)
import List
import Directory
import Time
import Distribution
import Names

banner = bannerLine ++ '\n' : bannerText ++ '\n' : bannerLine ++ "\n"
 where
   bannerText = "ID-based Curry->Haskell Compiler (Version of 10/02/11)"
   bannerLine = take (length bannerText) (repeat '=')

------------------------------------------------------------------------
-- Data type for compiler parameters
data CParam = CParam Bool -- quiet?

defaultCParam = CParam False

isQuiet (CParam q) = q

setQuiet (CParam _) = CParam True

------------------------------------------------------------------------

main = do
  args <- getArgs
  processArgs defaultCParam args
 where
  processArgs cparam args = case args of
     ("-q":moreargs) -> processArgs (setQuiet cparam) moreargs
     [mname]         -> transform cparam (stripSuffix mname)
     _ -> putStrLn $ banner++"ERROR: Illegal arguments for transformation: "++
                     unwords args ++ "\n" ++
                     "Usage: idc [-q] <module_name>\n"++
                     "-q : quiet mode\n"

-- Transform a qname of the source program into a qname of the target program
renType (mn,fn) = (mkModName mn, mkTypeName fn)
renCons (mn,fn) = (mkModName mn, mkConName fn)


mtest = transform defaultCParam "Test"

-- The main transformation function.
transform :: CParam -> String -> IO ()
transform cparam modname = do
  if isQuiet cparam then done else putStrLn banner
  prog <- FC.readFlatCurry modname
  let saveprog  = transProg cparam prog
      savefile  = mkModName modname ++ ".hs"
  putStrLn $ "Writing compiled module to '" ++ savefile ++ "'..."
  --putStr (showGeneratedTypes prog)
  writeFile savefile (showProg saveprog)

isPrimTypeDecl tdecl = case tdecl of
  FC.TypeSyn (_,n) _ _ _ -> n == "String"
  FC.Type (mn,tc) _ _ _ ->
        mn=="Prelude" &&
        (tc `elem` ["Int","Float","Char","Success","IO","IOError"])

showGeneratedTypes :: FC.Prog -> String
showGeneratedTypes (FC.Prog _ _ tdecls _ _) =
  showTypeDecls (concatMap genTypeDefinitions
                           (filter (not . isPrimTypeDecl) tdecls))


transProg :: CParam -> FC.Prog -> Prog
transProg _ (FC.Prog mname imps tdecls _ _) =
  Prog (mkModName mname)
       (nub ("ID":"Basics":map mkModName imps))
       (concatMap genTypeDefinitions
                  (filter (not . isPrimTypeDecl) tdecls))
       []
       []


------------------------------------------------------------------------
-- Generate code for user-defined types.

genTypeDefinitions :: FC.TypeDecl -> [TypeDecl]
genTypeDefinitions (FC.TypeSyn qf vis targs texp) =
  [TypeSyn (renType qf) (visibility2ac vis) (map tvar2ac targs) (texp2ac texp)]

genTypeDefinitions (FC.Type (mn,tc) vis tnums cdecls) =
  [Type (renType (mn,tc)) acvis targs
         (map tcons2ac cdecls ++
          [Cons choiceConsName 3 acvis [idType,ctype,ctype],
           Cons failConsName   0 acvis [],
           Cons guardConsName  3 acvis [constraintType,ctype]]),
   nondetInstance,
   generableInstance,
   showInstance]
 where
  acvis = visibility2ac vis
  targs = map tvar2ac tnums
  ctype = TCons (renType (mn,tc)) (map TVar targs)

  choiceConsName = (mn,"Choice_"++mkTypeName tc)
  failConsName   = (mn,"Fail_"++mkTypeName tc)
  guardConsName  = (mn,"Guard_"++mkTypeName tc) 

  -- Generate instance of NonDet class:
  nondetInstance =
   Instance (basics "NonDet") ctype []
     ([((basics "choiceCons"), Rule [] [noGuard (constF choiceConsName)] []),
       ((basics "failCons"),   Rule [] [noGuard (constF failConsName)] []),
       ((basics "guardCons"),  Rule [] [noGuard (constF guardConsName)] [])] ++
      map (\r->(basics "try",r)) tryRules)

  tryRules =
   [Rule [PComb choiceConsName [PVar (1,"i"),PVar (2,"x"),PVar (3,"y")]]
          [noGuard (applyF (basics "tryChoice")
                           [Var (1,"i"),Var (2,"x"),Var (3,"y")])] [],
    Rule [PComb guardConsName [PVar (1,"c"),PVar (2,"e")]]
          [noGuard (applyF (basics "Guard") [Var (1,"c"),Var (2,"e")])] [],
    Rule [PVar (1,"x")] [noGuard (applyF (basics "Val") [cvar "x"])] []]

  -- Generate instance of Generable class:
  generableInstance =
   Instance (basics "Generable") ctype
      (map (\tv -> Context (basics "Generable") [tv]) targs)
      [((basics "generate"),
        Rule [PVar (1,"i")] [noGuard genBody] [])]
 
  genBody =
    if null cdecls
    then applyF (pre "error")
                [string2ac $ "No constructors for "++tc]
    else foldr1 (\x y -> applyF choiceConsName
                                [applyF (basics "freeID") [Var (1,"i")], x, y])
                (cons2genCons 0 cdecls)

  cons2genCons _ [] = []
  cons2genCons i (FC.Cons qn _ _ texps : cs) = let ar = length texps in
    applyF (renCons qn) (map (\j -> applyF (basics "generate")
                                           [freshID (i+j) (Var (1,"i"))])
                   [0 .. ar-1])
     : cons2genCons (i+ar) cs

  -- Generate instance of Show class:
  showInstance =
   Instance (basics "Show") ctype
     (map (\tv -> Context (basics "Show") [tv]) targs)
     ([(pre "showsPrec",
        Rule [PVar (1,"d"),
               PComb choiceConsName
                      [PVar (2,"i"),PVar (3,"x"),PVar (4,"y")]]
              [noGuard (applyF (pre "showsChoice")
                               [Var (1,"d"),Var (2,"i"),
                                Var (3,"x"),Var (4,"y")])] []),
       (pre "showsPrec",
        Rule [PVar (1,"d"), PComb failConsName []]
              [noGuard (applyF (pre "showChar") [Lit (Charc '!')])] [])]
       ++ map showConsRule cdecls)

  -- Generate Show instance rule for a data constructor:
  showConsRule (FC.Cons qn _ _ texps) = let carity = length texps in
    (pre "showsPrec",
     Rule [PVar (0,"d"),
            PComb (renCons qn) (map (\i -> PVar (i,'x':show i)) [1..carity])]
           [noGuard (showBody carity)] [])
   where
     showBody ar =
      if ar==0
      then applyF (pre "showString") [string2ac (snd qn)]
      else applyF (pre ".")
                  [applyF (pre "showString") 
                          [string2ac ('(':snd qn)],
                   foldr (\x xs -> applyF (pre ".")
                                    [applyF (pre ":") [Lit (Charc ' ')],
                                     applyF (pre ".") [x,xs]])
                         (applyF (pre "showChar") [Lit (Charc ')')])
                         (map (\i->applyF (pre "shows") [Var (i,'x':show i)])
                              [1..ar])]

freshID n i =
  if n==0 then applyF (idmod "leftID") [i]
          else freshID (n-1) (applyF (idmod "rightID") [i])

idType = baseType (idmod "ID")

constraintType = baseType (pre "Constraint")

basics :: String -> QName
basics n = ("Basics",n)

idmod n = ("ID",n)

------------------------------------------------------------------------
-- Translating FlatCurry to AbstractHaskell
visibility2ac :: FC.Visibility -> Visibility
visibility2ac FC.Public  = AbstractHaskell.Public
visibility2ac FC.Private = AbstractHaskell.Private

tvar2ac :: FC.TVarIndex -> TVarIName
tvar2ac i = (i, "t"++show i)

tcons2ac :: FC.ConsDecl -> ConsDecl
tcons2ac (FC.Cons qf ar vis texps) =
   Cons (renCons qf) ar (visibility2ac vis) (map texp2ac texps)

texp2ac :: FC.TypeExpr -> TypeExpr
texp2ac (FC.TVar i) = TVar (tvar2ac i)
texp2ac (FC.FuncType t1 t2) = FuncType (texp2ac t1) (texp2ac t2)
texp2ac (FC.TCons qf texps) = TCons (renType qf) (map texp2ac texps)

------------------------------------------------------------------------
