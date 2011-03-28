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
    then [ Type (mn, tc) acvis targs [] ]
    else [ Type (mn, tc) acvis targs
           (map fcy2absCDecl cdecls ++
           [ Cons choiceConsName  3 acvis [idType, ctype, ctype]
           , Cons choicesConsName 2 acvis [idType, clisttype]
           , Cons failConsName    0 acvis []
           , Cons guardConsName   2 acvis [constraintType, ctype]
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
  clisttype = listType ctype
  choiceConsName = mkChoiceName (mn,tc)
  choicesConsName = mkChoicesName (mn,tc)
  failConsName = mkFailName (mn,tc)
  guardConsName = mkGuardName (mn,tc)


  -- Generate instance of Show class:
  showInstance = mkInstance (basics "Show") ctype targs
--     (if tc=="OP_List" then [showRule4List] else
    ([( pre "showsPrec"
      , simpleRule [PVar (1,"d"),
          PComb choiceConsName [PVar (2,"i"),PVar (3,"x"),PVar (4,"y")]]
          (applyF (pre "showsChoice")
                  [Var (1,"d"),Var (2,"i"), Var (3,"x"),Var (4,"y")])),
      (pre "showsPrec",
      simpleRule
        [PVar (1,"d"), PComb choicesConsName [PVar (2,"i"),PVar (3,"xs")]]
        (applyF (pre "showsChoices") [Var (1,"d"),Var (2,"i"),Var (3,"xs")])),
      (pre "showsPrec",
      simpleRule
        [PVar (1,"d"), PComb guardConsName [PVar (2,"c"),PVar (3,"e")]]
        (applyF (pre "showsGuard") [Var (1,"d"),Var (2,"c"),Var (3,"e")])),
      (pre "showsPrec",
      simpleRule [PVar (1,"d"), PComb failConsName []]
        (applyF (pre "showChar") [charc '!']))]
      ++ map showConsRule cdecls)
-- )

  -- Generate specific show for lists (only for finite determ. lists!)
  showRule4List =
     (pre "showsPrec",
      Rule [PVar (1,"d"), PVar (2,"cl")]
           [noGuard (applyF (pre "showsPrec")
                            [Var (1,"d"),
                             applyF (mn,"transList") [Var (2,"cl")]])]
           [LocalFunc
             (ufunc (mn,"transList") 1 Private
               [simpleRule [PComb (mn,"OP_List") []]
                     (constF (pre "[]")),
                simpleRule [PComb (mn,"OP_Cons") [PVar (1,"x"),PVar (2,"xs")]]
                     (applyF (pre ":")
                                      [Var (1,"x"),
                                       applyF (mn,"transList") [Var (2,"xs")]]),
                simpleRule [PVar (1,"_")]
                       (applyF (pre "error")
                          [string2ac "ERROR: try to show non-standard list"])]) ])

  -- Generate Show instance rule for a data constructor:
  showConsRule (FC.Cons qn _ _ texps) =
    (pre "showsPrec",
     simpleRule
      [PVar (0,"d"), PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity])]
        (case take 8 (snd qn) of
           "OP_Cons"  -> showListCons
           "OP_Tuple" -> showTupleCons
           _          -> showBody))
   where
     carity = length texps

     showBody =
      if carity==0
      then applyF (pre "showString") [string2ac (unGenRename (snd qn))]
      else applyF (pre ".")
                  [applyF (pre "showString")
                          [string2ac ('(':unGenRename (snd qn))],
                   foldr (\x xs -> applyF (pre ".")
                                    [applyF (pre "showChar") [Lit (Charc ' ')],
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
     showListCons = applyF (pre "showParen")
       [ applyF (pre ">") [Var (0,"d"), intc 5]
       , foldr1 (\f1 f2 -> applyF (pre ".") [f1, f2])
          [ applyF (pre "showsPrec") [intc 6, Var (1,"x1")]
          , applyF (pre "showChar")  [charc ':']
          , applyF (pre "showsPrec") [intc 5, Var (2,"x2")]
          ]
       ]

  -- Generate instance of Read class:
  readInstance = mkInstance (pre "Read") ctype targs
     [case take 8 tc of
        "OP_List"  -> readListRule
        "OP_Tuple" -> readTupleRule (head cdecls)
        _          -> readRule cdecls]

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
               [simpleRule [tuplePat [PVar (3,"xs"), PVar (4,"s")]]
                  (tupleExpr [applyF (pre "foldr")
                                [constF (mn,"OP_Cons"),
                                constF (mn,"OP_List"),
                                Var (3,"xs")],
                              Var (4,"s")])])
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
               [simpleRule [tuplePat [tuplePat (map (\i->PVar (i,'x':show i))
                                              [1..carity]),
                                PVar (0,"s")]]
                        (tupleExpr [applyF qn (map (\i->Var (i,'x':show i))
                                                   [1..carity]),
                                    Var (0,"s")])])])
    --readTup ((x1,x2),s) = (OP_Tuple2 x1 x2,s)

  -- Generate Read instance rule for data constructors:
  readRule conss =
    (pre "readsPrec",
     simpleRule [PVar (0,"d"), PVar (1,"s")]
      (foldr1 (\e1 e2 -> applyF (pre "++") [e1,e2]) (map readParen conss)))

  readParen (FC.Cons qn carity _ _) = applyF (pre "readParen")
    [ if carity == 0 then constF (pre "False") -- no parentheses required
                     else applyF (pre ">") [Var (0,"d"),intc 10]
    , Lambda [PVar (2,"r")]
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
        )
    , Var (1,"s")
    ]
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
  nondetInstance = mkInstance (basics "NonDet") ctype []
     (map mkSpecialConsRule [ ("choiceCons" , choiceConsName)
                            , ("choicesCons", choicesConsName)
                            , ("failCons"   , failConsName)
                            , ("guardCons"  , guardConsName)
                            ]
     ++ map (\r-> (basics "try", r)) tryRules)

  mkSpecialConsRule (name, fun) = (basics name, simpleRule [] (constF fun))

  tryRules =
   [ simpleRule
      [PComb choiceConsName [PVar (1,"i"),PVar (2,"x"),PVar (3,"y")]]
      (applyF (basics "tryChoice") [Var (1,"i"),Var (2,"x"),Var (3,"y")])
   , simpleRule [PComb choicesConsName [PVar (1,"i"), PVar (2,"xs")]]
      (applyF (basics "tryChoices") [Var (1,"i"), Var (2,"xs")])
   , simpleRule [PComb failConsName []] (applyF (basics "Fail") [])
   , simpleRule [PComb guardConsName [PVar (1,"c"),PVar (2,"e")]]
      (applyF (basics "Guard") [Var (1,"c"),Var (2,"e")])
   , simpleRule [PVar (1,"x")] (applyF (basics "Val") [cvar "x"])
   ]

  -- Generate instance of Generable class:
  -- TODO generators for constructor arguments can use the same idsupplies
  --      for different constructors; change bind accordingly

  generableInstance = mkInstance (basics "Generable") ctype targs
    [(basics "generate", simpleRule [PVar (1,"s")] (genBody (Var (1, "s"))))]

  genBody idSupp =
    applyF choicesConsName [(applyF (idmod "freeID") [idSupp]), list2ac $
      map (\(FC.Cons qn _ _ texps) -> applyF qn (consArgs2gen idSupp (length texps))) cdecls]

  consArgs2gen idSupp n = map (applyF (basics "generate") . (:[])) $ mkSuppList n idSupp

  mkSuppList n supp
    | n == 0    = []
    | n == 1    = [leftsupp supp]
    | otherwise = mkSuppList' n supp

  mkSuppList' n supp
    | n == 1    = [supp]
    | otherwise = mkSuppList' (n - half) (leftsupp supp) ++ mkSuppList' half (rightsupp supp)
    where
      half = n `div` 2


  -- Generate instance of NormalForm class:
  normalformInstance = mkInstance (basics "NormalForm") ctype targs
     (map normalformConsRule cdecls
      ++ [(basics "$!!",
           simpleRule [PVar (1,"cont"),
                 PComb choiceConsName [PVar (2,"i"), PVar (3,"x"), PVar (4,"y")]]
                     (applyF (pre "nfChoice")
                                      [Var (1,"cont"),Var (2,"i"), Var (3,"x"),Var (4,"y")]))
      ,(basics "$!!",
           simpleRule [PVar (1,"cont"),
                 PComb choicesConsName [PVar (2,"i"), PVar (3,"xs")]]
                     (applyF (pre "nfChoices")
                                      [Var (1,"cont"),Var (2,"i"), Var (3,"xs")]))
      ,(basics "$!!", simpleRule [PVar (1,"cont"),
                            PComb guardConsName [PVar (2,"c"),PVar (3,"x")]]
                     (applyF (pre "guardCons")
                                      [Var (2,"c")
                                      ,applyF (basics "$!!")[Var (1,"cont"),Var (3,"x")]]))
      ,(basics "$!!", simpleRule [PVar (1,"_"),PComb failConsName []]
                     (Symbol (pre "failCons")))]
      ++ map normalformIOConsRule cdecls
      ++ [(basics "$!<",
           simpleRule [PVar (1,"cont"),
                 PComb choiceConsName [PVar (2,"i"), PVar (3,"x"), PVar (4,"y")]]
                     (applyF (pre "nfChoiceIO")
                                      [Var (1,"cont"),Var (2,"i"), Var (3,"x"),Var (4,"y")]))
      ,(basics "$!<",
           simpleRule [PVar (1,"cont"),
                 PComb choicesConsName [PVar (2,"i"), PVar (3,"xs")]]
                     (applyF (pre "nfChoicesIO")
                                      [Var (1,"cont"),Var (2,"i"), Var (3,"xs")]))
      ,(basics "$!<", simpleRule [PVar (1,"cont"), PVar (2,"x")]
                     (applyV (1,"cont") [Var (2,"x")]))])

  -- Generate NormalForm instance rule for a data constructor:
  normalformConsRule (FC.Cons qn _ _ texps) =
    (basics "$!!",
     simpleRule [PVar (1,"cont"),
           PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity])]
          nfBody)
   where
     carity = length texps

     nfBody =
      foldr (\i exp -> applyF (basics "$!!")
                        [Lambda [PVar (i,'y':show i)] exp,Var (i,'x':show i)])
            (applyV (1,"cont")
                    [applyF qn (map (\i -> Var (i,'y':show i)) [1..carity])])
            [1..carity]

  -- Generate NormalForm instance rule for a data constructor:
  normalformIOConsRule (FC.Cons qn _ _ texps) =
    (basics "$!<",
     simpleRule [PVar (1,"cont"),
           PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity])]
          nfBody)
   where
     carity = length texps

     nfBody =
      foldr (\i exp -> applyF (basics "$!<")
                        [Lambda [PVar (i,'y':show i)] exp,Var (i,'x':show i)])
            (applyV (1,"cont")
                    [applyF qn (map (\i -> Var (i,'y':show i)) [1..carity])])
            [1..carity]

  -- Generate instance of Unifiable class:
  unifiableInstance = mkInstance (basics "Unifiable") ctype targs $ concat
       -- unification
     [ map (unifiableConsRule (basics "=.=") (basics "=:=")) cdecls
     , catchAllCase (basics "=.=") (basics "Fail_C_Success")
      -- lazy unification (function patterns)
     , map (unifiableConsRule (basics "=.<=") (basics "=:<=")) cdecls
     , catchAllCase (basics "=.<=") (basics "Fail_C_Success")
       -- bind
     , map (bindConsRule (basics "bind") (\ident arg -> applyF (basics "bind") [ident, arg]) (applyF (pre "concat"))) (zip [0 ..] cdecls)
     , [bindChoiceRule (basics "bind")
          (PComb choiceConsName [PVar (2,"j"), PVar (3,"_"), PVar (4,"_")])]
     , [bindChoiceRule (basics "bind")
          (PComb choicesConsName [PVar (2,"j"), PVar (3,"_")])]
       -- lazy bind (function patterns)
    , map (bindConsRule (basics "lazyBind") (\ident arg -> applyF (basics ":=:") [ident, applyF (basics "LazyBind") [applyF (basics "lazyBind") [ident, arg]]]) head) (zip [0 ..] cdecls)
    , [bindChoiceRule (basics "lazyBind")
          (PComb choiceConsName [PVar (2,"j"), PVar (3,"_"), PVar (4,"_")])]
     , [bindChoiceRule (basics "lazyBind")
          (PComb choicesConsName [PVar (2,"j"), PVar (3,"_")])]
     , [(basics "lazyBind",
          simpleRule [PVar (1, "_"), PComb failConsName []] (list2ac [constF (basics "Failed")]))]
     , [(basics "lazyBind",
          simpleRule [PVar (1, "i"), PComb guardConsName [PVar (2,"cs"), PVar (3,"e")]]
          (applyF (pre "++")
            [ Var (2, "cs")
            , list2ac [ applyF (basics ":=:")
              [Var (1, "i"), applyF (basics "LazyBind")
                 [ applyF (basics "lazyBind") [Var (1, "i"), Var (3, "e")]]]]]))
       ]
     ]

  -- Generate Unifiable instance rule for a data constructor
  unifiableConsRule consFunc genFunc (FC.Cons qn _ _ texps) =
    ( consFunc, simpleRule [consPattern qn "x" carity, consPattern qn "y" carity]
                (unifBody genFunc) )
   where
     unifBody funcName
       | carity == 0 = constF (basics "C_Success")
       | otherwise   = foldr1 (\x xs -> applyF (basics "&") [x, xs])
                          (map (\i -> applyF funcName
                            [Var (i,'x':show i), Var (i,'y':show i)])
                          [1 .. carity])
     carity = length texps

  -- Generate bindRules for a data constructor:
--  bindConsRules :: [FC.ConsDecl] -> (Expr -> Expr) -> (Expr -> Expr) -> [Rule]
  bindConsRule funcName bindArgs combine (num, (FC.Cons qn _ _ texps)) =
    (funcName,
      simpleRule [PVar (1, "i"), PComb qn $ map (\i -> PVar (i, 'x':show i)) [2 .. (length texps) + 1] ]
        ( applyF (pre ":")
                 [ applyF (basics ":=:")
                   [ Var (1, "i")
                   , applyF (basics "ChooseN") [intc num, intc $ length texps]
                   ]
                 , combine [list2ac (zipWith bindArgs
                    (mkIdList (length texps) (Var (1, "i")))
                    (map (\i -> Var (i, 'x':show i)) [2 ..(length texps) + 1]))]
                 ]))

  bindChoiceRule funcName choicePattern = (funcName,
    simpleRule [PVar (1,"i"), choicePattern]
         (applyF (pre ":")
                    [ applyF (basics ":=:") [Var (1,"i"), applyF (idmod "BindTo") [Var (2,"j")]]
                    , constF (pre "[]")
                    ])
         )


  curryInstance = mkInstance (basics "Curry") ctype targs $ concat
     [ extConsRules "=?=", map eqConsRule cdecls, catchAllPattern (pre "=?=")
     , extConsRules "<?=", ordConsRules cdecls  , catchAllPattern (pre "<?=")
     ]
    where
     catchAllPattern qn
       | length cdecls > 1 = catchAllCase qn (pre "C_False")
       | otherwise         = []

  extConsRules name =
    [nameRule $ simpleRule [PComb choiceConsName [PVar (1,"i"), PVar (2,"x"), PVar (3,"y")]
                     ,PVar (4,"z")]
                     (applyF narrow [Var (1,"i")
                                             ,applyF (pre name)[Var (2,"x"),Var (4,"z")]
                                             ,applyF (pre name) [Var (3,"y"),Var (4, "z")]])

      ,nameRule $ simpleRule [PComb choicesConsName [PVar (1,"i"), PVar (2,"xs")]
                     ,PVar (3,"y")]
                     (applyF narrows [Var (1,"i")
                                              ,applyF (pre "map")
                                                      [Lambda [PVar (4,"x")] (applyF (pre name) [Var (4,"x"),Var (3,"y")])
                                                      ,Var (2,"xs")]])

      ,nameRule $ simpleRule [PComb guardConsName [PVar (1,"c"),PVar (2,"x")], PVar(3,"y")]
                     (applyF (pre "guardCons")
                                      [Var (1,"c")
                                      ,applyF (pre name) [Var (2,"x"),Var (3,"y")]])

      ,nameRule $ simpleRule [PComb failConsName [],PVar (1,"_")]
                     (Symbol (pre "failCons"))
      ,nameRule $ simpleRule [PVar (4,"z")
                      ,PComb choiceConsName [PVar (1,"i"), PVar (2,"x"), PVar (3,"y")]]
                     (applyF narrow [Var (1,"i")
                                             ,applyF (pre name)[Var (4,"z"),Var (2,"x")]
                                             ,applyF (pre name) [Var (4,"z"),Var (3, "y")]])

      ,nameRule $ simpleRule [PVar (3,"y"), PComb choicesConsName [PVar (1,"i"), PVar (2,"xs")]]
                     (applyF narrows [Var (1,"i")
                                              ,applyF (pre "map")
                                                      [Lambda [PVar (4,"x")] (applyF (pre name) [Var (3,"y"),Var (4,"x")])
                                                      ,Var (2,"xs")]])

      ,nameRule $ simpleRule [PVar(3,"y"), PComb guardConsName [PVar (1,"c"),PVar (2,"x")]]
                     (applyF (pre "guardCons")
                                      [Var (1,"c")
                                      ,applyF (pre name) [Var (3,"y"),Var (2,"x")]])

      ,nameRule $ simpleRule [PVar (1,"_"),PComb failConsName []]
                     (Symbol (pre "failCons"))
      ]
    where
      nameRule rule = (pre name, rule)

  -- Generate equality instance rule for a data constructor:
  eqConsRule (FC.Cons qn _ _ texps) =
    (pre "=?=",
     simpleRule [PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity]),
           PComb qn (map (\i -> PVar (i,'y':show i)) [1..carity])]
          eqBody)
   where
     carity = length texps

     eqBody =
      if carity==0 then constF (pre "C_True")
      else foldr1 (\x xs -> applyF (pre "d_OP_ampersand_ampersand") [x,xs])
                  (map (\i -> applyF (pre "=?=")
                                     [Var (i,'x':show i),Var (i,'y':show i)])
                       [1..carity])

  -- Generate Unifiable instance rule for a data constructor:
  ordConsRules [] = []
  ordConsRules (FC.Cons qn _ _ texps : cds) =
    (pre "<?=",
     simpleRule [PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity]),
           PComb qn (map (\i -> PVar (i,'y':show i)) [1..carity])]
          (ordBody [1..carity])) :
    map (ordCons2Rule (qn,carity)) cds ++
    ordConsRules cds
   where
     carity = length texps

     ordBody [] = constF (pre "C_True")
     ordBody (i:is) =
      let xi = Var (i,'x':show i)
          yi = Var (i,'y':show i)
       in if null is
          then applyF (pre "<?=") [xi,yi]
          else applyF (pre "d_OP_bar_bar")
                 [applyF (pre "d_OP_lt")  [xi,yi],
                  applyF (pre "d_OP_ampersand_ampersand") [applyF (pre "=?=") [xi,yi], ordBody is]]

  ordCons2Rule (qn1,ar1) (FC.Cons qn2 _ _ texps2) =
    (pre "<?=",
     simpleRule [PComb qn1 (map (\i -> PVar (i,"_")) [1..ar1]),
           PComb qn2 (map (\i -> PVar (i,"_")) [1..(length texps2)])]
          (constF (pre "C_True")))

--freshID n i =
--  if n==0 then left i
--          else freshID (n-1) (right i)

mkInstance qn ctype targs
  = Instance qn ctype $ map (\tv -> Context qn [tv]) targs

consPattern qn varName carity
  = PComb qn $ map (\i -> PVar (i, varName ++ show i)) [1 .. carity]

catchAllCase qn retVal
  = [(qn, simpleRule [PVar (1,"_"), PVar (2,"_")] (constF retVal))]

simpleRule patterns body = Rule patterns [noGuard body] []

intc i = Lit $ Intc i

charc c = Lit $ Charc c

mkIdList n id
  | n == 0    = []
  | n == 1    = [left id]
  | otherwise = mkIdList' n id
  where
    mkIdList' n id
      | n == 1    = [id]
      | otherwise = mkIdList' (n - half) (left id) ++ mkIdList' half (right id)
      where
        half = n `div` 2

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
narrows = basics "narrows"

------------------------------------------------------------------------
