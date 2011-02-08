------------------------------------------------------------------------
--- Generation of data and instance declarations for the Curry->Haskell
--- compiler.
--- The input is a FlatCurry program and the output is an AbstractCurry
--- program with instance declarations that can be easily pretty printed
------------------------------------------------------------------------

import qualified FlatCurry as FC
import FlatCurryGoodies
import AbstractCurry
--import PrettyAbstract
import AbstractCurryPrinter
import AbstractCurryGoodies
import System
import FileGoodies(stripSuffix)
import List
import Directory
import Time
import Distribution
import Names

banner = bannerLine ++ '\n' : bannerText ++ '\n' : bannerLine ++ "\n"
 where
   bannerText = "ID-based Curry->Haskell Compiler (Version of 04/02/11)"
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


transProg :: CParam -> FC.Prog -> CurryProg
transProg _ (FC.Prog mname imps tdecls _ _) =
  CurryProg (mkModName mname)
            (nub ("ID":"Basics":map mkModName imps))
            (concatMap genTypeDefinitions
                       (filter (not . isPrimTypeDecl) tdecls))
            []
            []


------------------------------------------------------------------------
-- Generate code for user-defined types.

genTypeDefinitions :: FC.TypeDecl -> [CTypeDecl]
genTypeDefinitions (FC.TypeSyn qf vis targs texp) =
  [CTypeSyn (renType qf) (visibility2ac vis) (map tvar2ac targs) (texp2ac texp)]

genTypeDefinitions (FC.Type (mn,tc) vis tnums cdecls) =
  [CType (renType (mn,tc)) acvis targs
         (map tcons2ac cdecls ++
          [CCons choiceConsName 3 acvis [idType,ctype,ctype],
           CCons failConsName   0 acvis [],
           CCons guardConsName  3 acvis [constraintType,ctype]]),
   nondetInstance,
   generableInstance,
   showInstance]
 where
  acvis = visibility2ac vis
  targs = map tvar2ac tnums
  ctype = CTCons (renType (mn,tc)) (map CTVar targs)

  choiceConsName = (mn,"Choice_"++mkTypeName tc)
  failConsName   = (mn,"Fail_"++mkTypeName tc)
  guardConsName  = (mn,"Guard_"++mkTypeName tc) 

  -- Generate instance of NonDet class:
  nondetInstance =
   CInstance (basics "NonDet") ctype []
     ([((basics "choiceCons"), CRule [] [noGuard (constF choiceConsName)] []),
       ((basics "failCons"),   CRule [] [noGuard (constF failConsName)] []),
       ((basics "guardCons"),  CRule [] [noGuard (constF guardConsName)] [])] ++
      map (\r->(basics "try",r)) tryRules)

  tryRules =
   [CRule [CPComb choiceConsName [CPVar (1,"i"),CPVar (2,"x"),CPVar (3,"y")]]
          [noGuard (applyF (basics "tryChoice")
                           [CVar (1,"i"),CVar (2,"x"),CVar (3,"y")])] [],
    CRule [CPComb guardConsName [CPVar (1,"c"),CPVar (2,"e")]]
          [noGuard (applyF (basics "Guard") [CVar (1,"c"),CVar (2,"e")])] [],
    CRule [CPVar (1,"x")] [noGuard (applyF (basics "Val") [cvar "x"])] []]

  -- Generate instance of Generable class:
  generableInstance =
   CInstance (basics "Generable") ctype
      (map (\tv -> CContext (basics "Generable") [tv]) targs)
      [((basics "generate"),
        CRule [CPVar (1,"i")] [noGuard genBody] [])]
 
  genBody =
    if null cdecls
    then applyF (pre "error")
                [string2ac $ "No constructors for "++tc]
    else foldr1 (\x y -> applyF choiceConsName
                                [applyF (basics "freeID") [CVar (1,"i")], x, y])
                (cons2genCons 0 cdecls)

  cons2genCons _ [] = []
  cons2genCons i (FC.Cons qn _ _ texps : cs) = let ar = length texps in
    applyF (renCons qn) (map (\j -> applyF (basics "generate")
                                           [freshID (i+j) (CVar (1,"i"))])
                   [0 .. ar-1])
     : cons2genCons (i+ar) cs

  -- Generate instance of Show class:
  showInstance =
   CInstance (basics "Show") ctype
     (map (\tv -> CContext (basics "Show") [tv]) targs)
     ([(pre "showsPrec",
        CRule [CPVar (1,"d"),
               CPComb choiceConsName
                      [CPVar (2,"i"),CPVar (3,"x"),CPVar (4,"y")]]
              [noGuard (applyF (pre "showsChoice")
                               [CVar (1,"d"),CVar (2,"i"),
                                CVar (3,"x"),CVar (4,"y")])] []),
       (pre "showsPrec",
        CRule [CPVar (1,"d"), CPComb failConsName []]
              [noGuard (applyF (pre "showChar") [CLit (CCharc '!')])] [])]
       ++ map showConsRule cdecls)

  -- Generate Show instance rule for a data constructor:
  showConsRule (FC.Cons qn _ _ texps) = let carity = length texps in
    (pre "showsPrec",
     CRule [CPVar (0,"d"),
            CPComb (renCons qn) (map (\i -> CPVar (i,'x':show i)) [1..carity])]
           [noGuard (showBody carity)] [])
   where
     showBody ar =
      if ar==0
      then applyF (pre "showString") [string2ac (snd qn)]
      else applyF (pre ".")
                  [applyF (pre "showString") 
                          [string2ac ('(':snd qn)],
                   foldr (\x xs -> applyF (pre ".")
                                    [applyF (pre ":") [CLit (CCharc ' ')],
                                     applyF (pre ".") [x,xs]])
                         (applyF (pre "showChar") [CLit (CCharc ')')])
                         (map (\i->applyF (pre "shows") [CVar (i,'x':show i)])
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
-- Translating FlatCurry to AbstractCurry
visibility2ac :: FC.Visibility -> CVisibility
visibility2ac FC.Public  = AbstractCurry.Public
visibility2ac FC.Private = AbstractCurry.Private

tvar2ac :: FC.TVarIndex -> CTVarIName
tvar2ac i = (i, "t"++show i)

tcons2ac :: FC.ConsDecl -> CConsDecl
tcons2ac (FC.Cons qf ar vis texps) =
   CCons (renCons qf) ar (visibility2ac vis) (map texp2ac texps)

texp2ac :: FC.TypeExpr -> CTypeExpr
texp2ac (FC.TVar i) = CTVar (tvar2ac i)
texp2ac (FC.FuncType t1 t2) = CFuncType (texp2ac t1) (texp2ac t2)
texp2ac (FC.TCons qf texps) = CTCons (renType qf) (map texp2ac texps)

------------------------------------------------------------------------
