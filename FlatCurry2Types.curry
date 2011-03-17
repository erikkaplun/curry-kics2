------------------------------------------------------------------------
--- Generation of data and instance declarations for the Curry->Haskell
--- compiler.
--- The input is a FlatCurry program and the output is an AbstractHaskell
--- program with instance declarations that can be easily pretty printed
---
--- @author Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version March 2011
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
     _ -> putStrLn $ "ERROR: Illegal arguments for transformation: "++
                     unwords args ++ "\n" ++
                     "Usage: idc [-q] <module_name>\n"++
                     "-q : quiet mode\n"


mtest = transform defaultCParam "Test"

-- The main transformation function.
transform :: CParam -> String -> IO ()
transform cparam modname = do
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
    then [ Type (mn, tc) acvis targs []]
    else [ Type (mn, tc) acvis targs
           (map fcy2absCDecl cdecls ++
           [ Cons choiceConsName 3 acvis [idType, ctype, ctype]
           , Cons failConsName   0 acvis []
           , Cons guardConsName  3 acvis [constraintType, ctype]
           ])
         , showInstance
         , readInstance
         , nondetInstance
         , generableInstance
         , normalformInstance
         , unifiableInstance
         , curryInstance
         ]
 where
  acvis = fcy2absVis vis
  targs = map fcy2absTVar tnums
  ctype = TCons (mn,tc) (map TVar targs)
  choiceConsName = mkChoiceName (mn,tc)
  failConsName = mkFailName (mn,tc)
  guardConsName = mkGuardName (mn,tc)

  
  -- Generate instance of Show class:
  showInstance =
   Instance (basics "Show") ctype
    (map (\tv -> Context (basics "Show") [tv]) targs)
    (if tc=="OP_List" then [showRule4List] else
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
       ++ map showConsRule cdecls))

  -- Generate specific show for lists (only for finite determ. lists!)
  showRule4List =
     (pre "showsPrec",
      Rule [PVar (1,"d"), PVar (2,"cl")]
           [noGuard (applyF (pre "showsPrec")
                            [Var (1,"d"),
                             applyF (mn,"transList") [Var (2,"cl")]])]
           [LocalFunc
             (ufunc (mn,"transList") 1 Private
               [Rule [PComb (mn,"OP_List") []]
                     [noGuard (constF (pre "[]"))] [],
                Rule [PComb (mn,"OP_Cons") [PVar (1,"x"),PVar (2,"xs")]]
                     [noGuard (applyF (pre ":")
                                      [Var (1,"x"),
                                       applyF (mn,"transList") [Var (2,"xs")]])]
                     [],
                Rule [PVar (1,"_")]
                     [noGuard
                       (applyF (pre "error")
                          [string2ac "ERROR: try to show non-standard list"])]
                     []]) ])

  -- Generate Show instance rule for a data constructor:
  showConsRule (FC.Cons qn _ _ texps) =
    (pre "showsPrec",
     Rule [PVar (0,"d"),
           PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity])]
          [noGuard (if snd qn == "OP_Cons" then showListCons else
                    if take 8 (snd qn) == "OP_Tuple" then showTupleCons else
                    showBody)] [])
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

     -- specific definition to show a tuple constructor
     showTupleCons =
       applyF (pre ".")
              [applyF (pre "showString") [string2ac "("],
               foldr (\x xs -> applyF (pre ".") [x,xs])
                     (applyF (pre "showChar") [Lit (Charc ')')])
                     (intersperse (applyF (pre ":") [Lit (Charc ',')])
                        (map (\i->applyF (pre "shows") [Var (i,'x':show i)])
                             [1..carity]))]

     -- specific definition to show a list constructor:
     showListCons =
       applyF (pre "showParen")
         [applyF (pre ">") [Var (0,"d"),Lit (Intc 5)],
          foldr1 (\f1 f2 -> applyF (pre ".") [f1,f2])
                 [applyF (pre "showsPrec") [Lit (Intc 6), Var (1,"x1")],
                  applyF (pre "showChar") [Lit (Charc ':')],
                  applyF (pre "showsPrec") [Lit (Intc 5), Var (2,"x2")]]]

  -- Generate instance of Read class:
  readInstance =
   Instance (pre "Read") ctype
     (map (\tv -> Context (pre "Read") [tv]) targs)
     [if tc == "OP_List" then readListRule else
      if take 8 tc == "OP_Tuple" then readTupleRule (head cdecls)
      else readRule cdecls]

  -- Generate Read instance rule for lists:
  readListRule =
     (pre "readsPrec",
      Rule [PVar (1,"d"), PVar (2,"s")]
           [noGuard (applyF (pre "map")
                            [constF (mn,"readList"),
                             applyF (pre "readsPrec")
                                    [Var (1,"d"),Var (2,"s")]])]
           [LocalFunc
             (ufunc (mn,"readList") 1 Private
               [Rule [tuplePat [PVar (3,"xs"), PVar (4,"s")]]
                     [noGuard
                        (tupleExpr [applyF (pre "foldr")
                                           [constF (mn,"OP_Cons"),
                                            constF (mn,"OP_List"),
                                            Var (3,"xs")],
                                    Var (4,"s")])]
                     []])
           ])

   --instance Read t0 => Read (OP_List t0) where
   --  readsPrec d s = map readList (readsPrec d s)
   --   where
   --     readList (xs,s) = (foldr OP_Cons OP_List xs,s)

  -- Generate Read instance rule for tuple constructor:
  readTupleRule (FC.Cons qn carity _ _) =
     (pre "readsPrec",
      Rule [PVar (1,"d"), PVar (2,"s")]
           [noGuard (applyF (pre "map")
                            [constF (mn,"readTup"),
                             applyF (pre "readsPrec")
                                    [Var (1,"d"),Var (2,"s")]])]
           [LocalFunc
             (ufunc (mn,"readTup") 1 Private
               [Rule [tuplePat [tuplePat (map (\i->PVar (i,'x':show i))
                                              [1..carity]),
                                PVar (0,"s")]]
                     [noGuard
                        (tupleExpr [applyF qn (map (\i->Var (i,'x':show i))
                                                   [1..carity]),
                                    Var (0,"s")])]
                     []])])
    --readTup ((x1,x2),s) = (OP_Tuple2 x1 x2,s)

  -- Generate Read instance rule for data constructors:
  readRule conss =
    (pre "readsPrec",
     Rule [PVar (0,"d"), PVar (1,"s")]
          [noGuard (foldr1 (\e1 e2 -> applyF (pre "++") [e1,e2])
                           (map readParen conss))]
          [])

  readParen (FC.Cons qn carity _ _) =
    applyF (pre "readParen")
           [if carity==0 then constF (pre "False") -- no parentheses required
                         else applyF (pre ">") [Var (0,"d"),Lit (Intc 10)],
            Lambda [PVar (2,"r")]
              (ListComp
                (tupleExpr
                  [applyF qn (map (\i->Var (i,'x':show i)) [1..carity]),
                   Var (carity,'r':show carity)])
                (SPat (tuplePat [PVar (0,"_"),PVar (1,"r0")])
                      (applyF (basics "readQualified")
                              [string2ac (unRenameModule mn),
                               string2ac (unGenRename (snd qn)),
                               Var (2,"r")])
                 : map genReadsPrec [1..carity])
              ),
            Var (1,"s")]
     where
      genReadsPrec i =
        SPat (tuplePat [PVar (i,'x':show i), PVar (i,'r':show i)])
             (applyF (pre "readsPrec") [Lit (Intc 11), Var (i-1,'r':show(i-1))])
   --instance (Read t0,Read t1) => Read (C_Either t0 t1) where
   --  readsPrec d r =
   --    (++) (readParen(d>10)
   --                   (\s -> [(C_Left x1,r1)
   --                        | (_,r0) <- readQualified "Prelude" "Left" s,
   --                          (x1,r1) <- readsPrec 11 r0])
   --                   r)
   --         (readParen(d>10)
   --                   (\s -> [(C_Right(x1),r1)
   --                        | (_,r0) <- readQualified "Prelude" "Right" s,
   --                          (x1,r1) <- readsPrec 11 r0])
   --                   r)
   --

  -- Generate instance of NonDet class:
  nondetInstance =
   Instance (basics "NonDet") ctype []
     ([((basics "choiceCons"), Rule [] [noGuard (constF choiceConsName)] []),
       ((basics "failCons"),   Rule [] [noGuard (constF   failConsName)] []),
       ((basics "guardCons"),  Rule [] [noGuard (constF  guardConsName)] [])] ++
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
        Rule [PVar (1,"i")] [noGuard (genBody (Var (1,"i")) cdecls)] [])]

  genBody idSupp cds=
    case cds of
      [] -> applyF (pre "error") [string2ac $ "No constructors for "++tc]
      [FC.Cons qn _ _ texps] -> applyF qn (consArgs2gen idSupp texps)
      c:cs                   -> applyF choiceConsName [applyF (idmod "freeID") [idSupp]
                                                      ,genBody (leftsupp idSupp)  [c]
                                                      ,genBody (rightsupp idSupp) cs]
  consArgs2gen idSupp texps =
   case texps of
    []  -> []
    [_] -> [applyF (basics "generate") [idSupp]]
    (_:xs) -> applyF (basics "generate") [leftsupp idSupp]
              : consArgs2gen (rightsupp idSupp) xs

  -- Generate instance of NormalForm class:
  normalformInstance =
   Instance (basics "NormalForm") ctype
     (map (\tv -> Context (basics "NormalForm") [tv]) targs)
     (map normalformConsRule cdecls ++
         [(basics "$!!",
           Rule [PVar (1,"cont"),
                 PComb (mkChoiceName (mn,tc)) [PVar (2,"i"), PVar (3,"x"), PVar (4,"y")]]
                     [noGuard (applyF (pre "nfChoice")
                                      [Var (1,"cont"),Var (2,"i"), Var (3,"x"),Var (4,"y")])] [])
      ,(basics "$!!", Rule [PVar (1,"cont"),
                            PComb (mkGuardName (mn,tc)) [PVar (2,"c"),PVar (3,"x")]]
                     [noGuard (applyF (pre "guardCons")
                                      [Var (2,"c")
                                      ,applyF (basics "$!!")[Var (1,"cont"),Var (3,"x")]])] [])
      ,(basics "$!!", Rule [PVar (1,"_"),PComb (mkFailName (mn,tc)) []]
                     [noGuard (Symbol (pre "failCons"))] [])])

  -- Generate NormalForm instance rule for a data constructor:
  normalformConsRule (FC.Cons qn _ _ texps) =
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

  -- Generate instance of Unifiable class:
  unifiableInstance =
   Instance (basics "Unifiable") ctype
     (map (\tv -> Context (basics "Unifiable") [tv]) targs)
     (map unifiableConsRule cdecls ++
      [(basics "=.=",
        Rule [PVar (1,"_"),PVar (2,"_")]
             [noGuard (constF (basics "Fail_C_Success"))] [])] ++
      bindConsRules cdecls id (const (constF (pre "[]"))) ++
      [(basics "bind",
        Rule [PVar (1,"i"),
              PComb choiceConsName
                    [PAs (2,"j") (PComb (idmod "FreeID") [PVar (3,"_")]),
                     PVar (4,"_"),PVar (5,"_")]]
             [noGuard (applyF (pre ":")
                              [applyF (basics ":=:")
                                 [Var (1,"i"),
                                  applyF (idmod "BindTo") [Var (2,"j")]],
                               constF (pre "[]")])] [])])

  -- Generate Unifiable instance rule for a data constructor:
  unifiableConsRule (FC.Cons qn _ _ texps) =
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
  -- Generate bindRules for a data constructor:
--  bindConsRules :: [FC.ConsDecl] -> (Expr -> Expr) -> (Expr -> Expr) -> [Rule]
  bindConsRules cds getSuppVar makeBinds =
    case cds of
      [FC.Cons qn _ _ texps]
        -> let cArgVars =  map (\i -> (i, 'x':show i)) [2..(length texps)+1] in
          [(basics "bind",
            Rule [PVar (1,"i")
                  ,PComb qn (map PVar cArgVars)]
              [noGuard  (applyF (pre "++")
                                [makeBinds (Var (1,"i"))
                                ,(bindConsArgs (map Var cArgVars)
                                               (getSuppVar (Var (1,"i"))))])][])]
      (c:cs) -> bindConsRules [c] (left . getSuppVar)
                (\bindID -> applyF (pre ":")
                           [applyF (basics ":=:")
                             [bindID
                             , constF (basics "ChooseLeft")]
                           ,makeBinds bindID])
                ++ bindConsRules cs (right . getSuppVar)
                   (\bindID -> applyF (pre ":")
                              [applyF (basics ":=:")
                                [bindID
                                ,constF (basics "ChooseRight")]
                              ,makeBinds bindID])

  bindConsArgs vars bindID =
   case vars of
    [] -> constF (pre "[]") -- TODO: omit generation of empty lists
    [v] -> applyF (pre "bind") [bindID, v]
    (v:vs) -> applyF (pre "++") [bindConsArgs [v] (left bindID)
                                , bindConsArgs vs (right bindID)]


  curryInstance =
    Instance (basics "Curry") ctype
      (map (\tv -> Context (basics "Curry") [tv]) targs)
      (extConsRules "=?=" ++ map eqConsRule cdecls ++
      (if multipleCons
       then [(pre "=?=", Rule [PVar (1,"_"),PVar (2,"_")]
                              [noGuard (constF (pre "C_False"))] [])]
       else [])
      ++ extConsRules "<?=" ++ ordConsRules cdecls ++
      (if multipleCons
       then [(pre "<?=", Rule [PVar (1,"_"),PVar (2,"_")]
                              [noGuard (constF (pre "C_False"))][])]
       else[]))
    where
     multipleCons = length cdecls > 1

  extConsRules name =
    [nameRule $ Rule [PComb (mkChoiceName (mn,tc)) [PVar (1,"i"), PVar (2,"x"), PVar (3,"y")]
                     ,PVar (4,"z")]
                     [noGuard (applyF narrow [Var (1,"i")
                                             ,applyF (pre name)[Var (2,"x"),Var (4,"z")]
                                             ,applyF (pre name) [Var (3,"y"),Var (4, "z")]])
                                             ] []
      ,nameRule $ Rule [PComb (mkGuardName (mn,tc)) [PVar (1,"c"),PVar (2,"x")], PVar(3,"y")]
                     [noGuard (applyF (pre "guardCons")
                                      [Var (1,"c")
                                      ,applyF (pre name) [Var (2,"x"),Var (3,"y")]])
                                      ] []
      ,nameRule $ Rule [PComb (mkFailName (mn,tc)) [],PVar (1,"_")]
                     [noGuard (Symbol (pre "failCons"))] []
      ,nameRule $ Rule [PVar (4,"z")
                      ,PComb (mkChoiceName (mn,tc)) [PVar (1,"i"), PVar (2,"x"), PVar (3,"y")]]
                     [noGuard (applyF narrow [Var (1,"i")
                                             ,applyF (pre name)[Var (4,"z"),Var (2,"x")]
                                             ,applyF (pre name) [Var (4,"z"),Var (3, "y")]])
                                             ] []
      ,nameRule $ Rule [PVar(3,"y"), PComb (mkGuardName (mn,tc)) [PVar (1,"c"),PVar (2,"x")]]
                     [noGuard (applyF (pre "guardCons")
                                      [Var (1,"c")
                                      ,applyF (pre name) [Var (3,"y"),Var (2,"x")]])
                                      ] []
      ,nameRule $ Rule [PVar (1,"_"),PComb (mkFailName (mn,tc)) []]
                     [noGuard (Symbol (pre "failCons"))] []
      ]
    where
      nameRule rule = (pre name, rule)

  -- Generate equality instance rule for a data constructor:
  eqConsRule (FC.Cons qn _ _ texps) =
    (pre "=?=",
     Rule [PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity]),
           PComb qn (map (\i -> PVar (i,'y':show i)) [1..carity])]
          [noGuard eqBody] [])
   where
     carity = length texps

     eqBody =
      if carity==0 then constF (pre "C_True")
      else foldr1 (\x xs -> applyF (pre "d_OP_ampersand_ampersand") [x,xs])
                  (map (\i -> applyF (pre "d_OP_eq_eq")
                                     [Var (i,'x':show i),Var (i,'y':show i)])
                       [1..carity])

  -- Generate Unifiable instance rule for a data constructor:
  ordConsRules [] = []
  ordConsRules (FC.Cons qn _ _ texps : cds) =
    (pre "<?=",
     Rule [PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity]),
           PComb qn (map (\i -> PVar (i,'y':show i)) [1..carity])]
          [noGuard (ordBody [1..carity])] []) :
    map (ordCons2Rule (qn,carity)) cds ++
    ordConsRules cds
   where
     carity = length texps

     ordBody [] = constF (pre "C_True")
     ordBody (i:is) =
      let xi = Var (i,'x':show i)
          yi = Var (i,'y':show i)
       in if null is
          then applyF (pre "d_OP_lt_eq") [xi,yi]
          else applyF (pre "d_OP_bar_bar")
                 [applyF (pre "d_OP_lt")  [xi,yi],
                  applyF (pre "d_OP_ampersand_ampersand") [applyF (pre "d_OP_eq_eq") [xi,yi], ordBody is]]

  ordCons2Rule (qn1,ar1) (FC.Cons qn2 _ _ texps2) =
    (pre "<?=",
     Rule [PComb qn1 (map (\i -> PVar (i,"_")) [1..ar1]),
           PComb qn2 (map (\i -> PVar (i,"_")) [1..(length texps2)])]
          [noGuard (constF (pre "C_True"))] [])


freshID n i =
  if n==0 then left i
          else freshID (n-1) (right i)

left  i = applyF (idmod "leftID") [i]
right i = applyF (idmod "rightID") [i]

leftsupp  s = applyF (idmod "leftSupply") [s]
rightsupp s = applyF (idmod "rightSupply") [s]

idType = baseType (idmod "ID")

constraintType = listType $ baseType (pre "Constraint")

basics :: String -> QName
basics n = ("Basics",n)

idmod n = ("ID",n)


narrow = basics "narrow"

------------------------------------------------------------------------
