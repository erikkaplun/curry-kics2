------------------------------------------------------------------------
--- Generation of data and instance declarations for the Curry->Haskell
--- compiler.
--- The input is a FlatCurry program and the output is an AbstractHaskell
--- program with instance declarations that can be easily pretty printed
---
--- @author Michael Hanus
--- @version February 2011
------------------------------------------------------------------------

module FlatCurry2Types where

import qualified FlatCurry as FC
import FlatCurryGoodies
import AbstractHaskell
import AbstractHaskellPrinter
import AbstractHaskellGoodies
import FlatCurry2AbstractHaskell
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


mtest = transform defaultCParam "Test"

-- The main transformation function.
transform :: CParam -> String -> IO ()
transform cparam modname = do
  if isQuiet cparam then done else putStrLn banner
  prog <- FC.readFlatCurry modname
  let saveprog  = transProg cparam prog
      savefile  = mkModName modname ++ ".hs"
  putStrLn $ "Writing compiled module to '" ++ savefile ++ "'..."
  putStr (showGeneratedTypes prog)
  writeFile savefile (showProg saveprog)

showGeneratedTypes :: FC.Prog -> String
showGeneratedTypes (FC.Prog _ _ tdecls _ _) =
  showTypeDecls (fcyTypes2abs tdecls)


transProg :: CParam -> FC.Prog -> Prog
transProg _ (FC.Prog mname imps tdecls _ _) =
  Prog (mkModName mname)
       (nub ("ID":"Basics":map mkModName imps))
       (fcyTypes2abs tdecls)
       []
       []


------------------------------------------------------------------------
-- Generate code for user-defined types.

--- Translate a list of FlatCurry type declarations into the
--- corresponding type and instance declarations for Haskell.
fcyTypes2abs :: [FC.TypeDecl] -> [TypeDecl]
fcyTypes2abs = concatMap genTypeDefinitions -- . filter (not . isPrimTypeDecl)

-- --- Is the type declaration primitive, i.e., should not be translated?
-- isPrimTypeDecl tdecl = case tdecl of
--   FC.TypeSyn _  _ _ _ -> True
--   FC.Type (mn,tc) _ _ _ ->
--         mn == renameModule "Prelude" &&
--         (tc `elem` map genRename ["Int","Float","Char","Success","IO"])

genTypeDefinitions :: FC.TypeDecl -> [TypeDecl]
genTypeDefinitions (FC.TypeSyn qf vis targs texp) =
  [TypeSyn qf (fcy2absVis vis) (map fcy2absTVar targs) (fcy2absTExp texp)]

genTypeDefinitions (FC.Type (mn,tc) vis tnums cdecls) =
  if null cdecls
    then [Type (mn, tc) acvis targs []]
    else [Type (mn,tc) acvis targs
           (map fcy2absCDecl cdecls ++
           [Cons choiceConsName 3 acvis [idType,ctype,ctype],
            Cons failConsName   0 acvis [],
            Cons guardConsName  3 acvis [constraintType,ctype]]),
   nondetInstance,
   generableInstance,
   showInstance,
   unifInstance,
   nfInstance,
   eqInstance,
   ordInstance]
 where
  acvis = fcy2absVis vis
  targs = map fcy2absTVar tnums
  ctype = TCons (mn,tc) (map TVar targs)
  choiceConsName = mkChoiceName (mn,tc)
  failConsName = mkFailName (mn,tc)
  guardConsName = mkGuardName (mn,tc)

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
    Rule [PComb failConsName []] [noGuard (applyF (basics "Fail") [])] [],
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
    applyF qn (map (\j -> applyF (basics "generate")
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
        Rule [PVar (1,"d"),
               PComb guardConsName [PVar (2,"c"),PVar (3,"e")]]
              [noGuard (applyF (pre "showsGuard")
                               [Var (1,"d"),Var (2,"c"),Var (3,"e")])] []),
       (pre "showsPrec",
        Rule [PVar (1,"d"), PComb failConsName []]
              [noGuard (applyF (pre "showChar") [Lit (Charc '!')])] [])]
       ++ map showConsRule cdecls)

  -- Generate Show instance rule for a data constructor:
  showConsRule (FC.Cons qn _ _ texps) =
    (pre "showsPrec",
     Rule [PVar (0,"d"),
           PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity])]
          [noGuard showBody] [])
   where
     carity = length texps

     showBody =
      if carity==0
      then applyF (pre "showString") [string2ac (unGenRename (snd qn))]
      else applyF (pre ".")
                  [applyF (pre "showString")
                          [string2ac ('(':unGenRename (snd qn))],
                   foldr (\x xs -> applyF (pre ".")
                                    [applyF (pre ":") [Lit (Charc ' ')],
                                     applyF (pre ".") [x,xs]])
                         (applyF (pre "showChar") [Lit (Charc ')')])
                         (map (\i->applyF (pre "shows") [Var (i,'x':show i)])
                              [1..carity])]

  -- Generate instance of Unifiable class:
  unifInstance =
   Instance (basics "Unifiable") ctype
     (map (\tv -> Context (basics "Unifiable") [tv]) targs)
     (map unifConsRule cdecls ++
      [(basics "=.=",
        Rule [PVar (1,"_"),PVar (2,"_")]
             [noGuard (constF (basics "Fail_C_Success"))] [])] ++
      -- TODO: bind rules for constructors
      [(basics "bind",
        Rule [PVar (1,"i"),
              PComb choiceConsName
                    [PAs (2,"j") (PComb (basics "FreeID") [PVar (3,"_")]),
                     PVar (4,"_"),PVar (5,"_")]]
             [noGuard (applyF (pre ":")
                              [applyF (basics ":=:")
                                 [Var (1,"i"),
                                  applyF (basics "BindTo") [Var (2,"j")]],
                               constF (pre "[]")])] [])])

  -- Generate Unifiable instance rule for a data constructor:
  unifConsRule (FC.Cons qn _ _ texps) =
    (basics "=.=",
     Rule [PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity]),
           PComb qn (map (\i -> PVar (i,'y':show i)) [1..carity])]
          [noGuard unifBody] [])
   where
     carity = length texps

     unifBody =
      if carity==0 then constF (basics "C_Success")
      else foldr1 (\x xs -> applyF (basics "&") [x,xs])
                  (map (\i -> applyF (basics "=:=")
                                     [Var (i,'x':show i),Var (i,'y':show i)])
                       [1..carity])

  -- Generate instance of NormalForm class:
  nfInstance =
   Instance (basics "NormalForm") ctype
     (map (\tv -> Context (basics "NormalForm") [tv]) targs)
     (map nfConsRule cdecls ++
      [(basics "$!!",
        Rule [PVar (1,"cont"),PVar (2,"x")]
           [noGuard (applyF (basics "$$!!") [Var (1,"cont"),Var (2,"x")])] [])])

  -- Generate NormalForm instance rule for a data constructor:
  nfConsRule (FC.Cons qn _ _ texps) =
    (basics "$!!",
     Rule [PVar (1,"cont"),
           PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity])]
          [noGuard nfBody] [])
   where
     carity = length texps

     nfBody =
      foldr (\i exp -> applyF (basics "$!!")
                        [Lambda [PVar (i,'y':show i)] exp,Var (i,'x':show i)])
            (applyV (1,"cont")
                    [applyF qn (map (\i -> Var (i,'y':show i)) [1..carity])])
            [1..carity]

  -- Generate instance of Eq class:
  eqInstance =
   Instance (pre "Eq") ctype
     (map (\tv -> Context (pre "Eq") [tv]) targs)
     (map eqConsRule cdecls ++
      [(pre "==",
        Rule [PVar (1,"_"),PVar (2,"_")] [noGuard (constF (pre "False"))] [])])

  -- Generate Unifiable instance rule for a data constructor:
  eqConsRule (FC.Cons qn _ _ texps) =
    (pre "==",
     Rule [PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity]),
           PComb qn (map (\i -> PVar (i,'y':show i)) [1..carity])]
          [noGuard eqBody] [])
   where
     carity = length texps

     eqBody =
      if carity==0 then constF (pre "True")
      else foldr1 (\x xs -> applyF (pre "&&") [x,xs])
                  (map (\i -> applyF (pre "==")
                                     [Var (i,'x':show i),Var (i,'y':show i)])
                       [1..carity])

  -- Generate instance of Ord class:
  ordInstance =
   Instance (pre "Ord") ctype
     (map (\tv -> Context (pre "Ord") [tv]) targs)
     ((ordConsRules cdecls) ++
      [(pre "<=",
        Rule [PVar (1,"_"),PVar (2,"_")] [noGuard (constF (pre "False"))] [])])

  -- Generate Unifiable instance rule for a data constructor:
  ordConsRules [] = []
  ordConsRules (FC.Cons qn _ _ texps : cds) =
    (pre "<=",
     Rule [PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity]),
           PComb qn (map (\i -> PVar (i,'y':show i)) [1..carity])]
          [noGuard (ordBody [1..carity])] []) :
    map (ordCons2Rule (qn,carity)) cds ++
    ordConsRules cds
   where
     carity = length texps

     ordBody [] = constF (pre "True")
     ordBody (i:is) =
      let xi = Var (i,'x':show i)
          yi = Var (i,'y':show i)
       in if null is
          then applyF (pre "<=") [xi,yi]
          else applyF (pre "||")
                 [applyF (pre "<")  [xi,yi],
                  applyF (pre "&&") [applyF (pre "==") [xi,yi], ordBody is]]

  ordCons2Rule (qn1,ar1) (FC.Cons qn2 _ _ texps2) =
    (pre "<=",
     Rule [PComb qn1 (map (\i -> PVar (i,"_")) [1..ar1]),
           PComb qn2 (map (\i -> PVar (i,"_")) [1..(length texps2)])]
          [noGuard (constF (pre "True"))] [])


freshID n i =
  if n==0 then applyF (idmod "leftSupply") [i]
          else freshID (n-1) (applyF (idmod "rightSupply") [i])

idType = baseType (idmod "ID")

constraintType = baseType (pre "Constraint")

basics :: String -> QName
basics n = ("Basics",n)

idmod n = ("ID",n)

------------------------------------------------------------------------
