-- ---------------------------------------------------------------------------
--- Generation of data and instance declarations for the Curry->Haskell
--- compiler.
--- The input is a FlatCurry program and the output is an AbstractHaskell
--- program with instance declarations that can be easily pretty printed
---
--- @author Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version January 2011
-- ---------------------------------------------------------------------------
module TransTypes (transTypes) where

import qualified FlatCurry as FC
import FlatCurryGoodies
import AbstractHaskell
import AbstractHaskellGoodies
import FlatCurry2AbstractHaskell
import FiniteMap
import List (intersperse)
import Names
  ( mkChoiceName, mkChoicesName, mkFailName, mkGuardName, mkFoConsName
  , mkHoConsName, renameModule, unGenRename, unRenameModule, renameQName
  , unRenameQName)
import Analysis

-- ---------------------------------------------------------------------------
-- Generate code for user-defined types
-- ---------------------------------------------------------------------------

--- Translate a list of FlatCurry type declarations into the
--- corresponding type and instance declarations for Haskell.
transTypes :: HOResult -> [FC.TypeDecl] -> [TypeDecl]
transTypes hoResult = concatMap (genTypeDeclarations hoResult)


genTypeDeclarations :: HOResult -> FC.TypeDecl -> [TypeDecl]
genTypeDeclarations _        (FC.TypeSyn qf vis tnums texp)
  = [TypeSyn qf (fcy2absVis vis) (map fcy2absTVar tnums) (fcy2absTExp texp)]
genTypeDeclarations hoResult t@(FC.Type qf vis tnums cdecls)
  | null cdecls = Type qf Public  targs []   : []
  | otherwise   = Type qf Public targs decls : instanceDecls
    -- type names are always exported to avoid ghc type errors
  where
    decls = concatMap (fcy2absCDecl hoResult) cdecls ++
            [ Cons (mkChoiceName  qf) 3 acvis [coverType, idType, ctype, ctype]
            , Cons (mkChoicesName qf) 2 acvis [coverType, idType, clisttype]
            , Cons (mkFailName    qf) 2 acvis [coverType, failInfoType]
            , Cons (mkGuardName   qf) 2 acvis [coverType, constraintType, ctype]
            ]
    instanceDecls = map ($t) [ showInstance hoResult
                             , readInstance
                             , nondetInstance
                             , generableInstance
                             , normalformInstance hoResult
                             , unifiableInstance  hoResult
                             , curryInstance      hoResult
                             , coverableInstance  hoResult
                             ]
    acvis     = (fcy2absVis vis)
    targs     = map fcy2absTVar tnums
    ctype     = TCons qf (map TVar targs)
    clisttype = listType ctype


fcy2absCDecl :: HOResult -> FC.ConsDecl -> [ConsDecl]
fcy2absCDecl hoResult (FC.Cons qf ar vis texps)
  | isHigherOrder = [foCons, hoCons]
  | otherwise     = [foCons]
  where
    isHigherOrder = lookupFM hoResult qf == Just HO
    foCons = Cons (mkFoConsName qf) ar vis' (map fcy2absTExp   texps)
    hoCons = Cons (mkHoConsName qf) ar vis' (map fcy2absHOTExp texps)
    vis' = fcy2absVis vis


fcy2absTExp :: FC.TypeExpr -> TypeExpr
fcy2absTExp (FC.TVar i)         = TVar (fcy2absTVar i)
fcy2absTExp (FC.TCons qf texps) = TCons qf (map fcy2absTExp texps)
fcy2absTExp (FC.FuncType t1 t2) = FuncType (fcy2absTExp t1) (FuncType cStoreT (fcy2absTExp t2))
  where
 cStoreT = TCons (basics "ConstStore") []

fcy2absHOTExp :: FC.TypeExpr -> TypeExpr
fcy2absHOTExp (FC.TVar i)         = TVar (fcy2absTVar i)
fcy2absHOTExp (FC.TCons qf texps) = TCons qf (map fcy2absHOTExp texps)
fcy2absHOTExp (FC.FuncType t1 t2) = funcType (fcy2absHOTExp t1) (fcy2absHOTExp t2)
  where funcType ta tb = TCons (basics "Func") [ta, tb]


-- ---------------------------------------------------------------------------
-- Generate instance of Show class:
-- ---------------------------------------------------------------------------
showInstance :: HOResult -> FC.TypeDecl -> TypeDecl
showInstance hoResult (FC.Type qf _ tnums cdecls)
  = mkInstance (basics "Show") [] ctype targs $
  if isListType qf then [showRule4List] else
    [ ( pre "showsPrec"
      , simpleRule [PVar d, mkChoicePattern qf "i" ]
          (applyF (basics "showsChoice")  [Var d, Var cd, Var i, Var x, Var y])
      )
    , ( pre "showsPrec"
      , simpleRule [PVar d, mkChoicesPattern qf]
          (applyF (basics "showsChoices") [Var d, Var cd, Var i, Var xs])
      )
    , ( pre "showsPrec"
      , simpleRule [PVar d, mkGuardPattern qf]
          (applyF (basics "showsGuard")   [Var d, Var cd, Var c, Var e])
      )
    , ( pre "showsPrec"
      , simpleRule [PVar us, mkFailPattern qf]
          (applyF (pre "showChar")        [charc '!'])
      )
    ] ++ concatMap (showConsRule hoResult) cdecls
  where [cd, d,i,x,y,xs,c,e,us] = newVars ["cd", "d","i","x","y","xs","c","e","_"]
        targs = map fcy2absTVar tnums
        ctype = TCons qf (map TVar targs)

  -- Generate specific show for lists (only for finite lists!)
showRule4List =
    (pre "showsPrec",
    Rule [] [noGuard (constF (pre "showsPrec4CurryList"))] [])

-- Generate Show instance rule for a data constructor:
showConsRule hoResult (FC.Cons qn carity _ _)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons = lookupFM hoResult qn == Just HO

    rule name = ( pre "showsPrec"
                , case take 8 (snd name) of
                    "OP_Cons"  -> showListCons name
                    "OP_Tuple" -> showTupleCons name
                    _          -> showBody name
                )

     -- specific definition to show a list constructor:
    showListCons name = simpleRule [PVar d, consPattern name "x" carity]
      $ applyF (pre "showParen")
          [ applyF (pre ">") [Var d, intc 5]
          , foldr1 (\f1 f2 -> applyF (pre ".") [f1, f2])
              [ applyF (pre "showsPrec") [intc 6, Var x1]
              , applyF (pre "showChar")  [charc ':']
              , applyF (pre "showsPrec") [intc 5, Var x2]
              ]
          ]
          where [d,x1,x2] = newVars ["d","x1","x2"]

     -- specific definition to show a tuple constructor
    showTupleCons name = simpleRule [PVar (0,"_"), consPattern name "x" carity]
      $ applyF (pre ".")
          [ applyF (pre "showString") [string2ac "("]
          , foldr (\x xs -> applyF (pre ".") [x,xs])
                  (applyF (pre "showChar") [Lit (Charc ')')])
                  (intersperse (applyF (pre ":") [Lit (Charc ',')])
                      (map (\i->applyF (pre "shows") [mkVar "x" i])
                        [1..carity]))]

    showBody name = simpleRule [PVar (0,"_"), consPattern name "x" carity] $
      if carity == 0
      then applyF (pre "showString") [string2ac (unGenRename (snd qn))]
      else applyF (pre ".")
                  [applyF (pre "showString")
                          [string2ac ('(':unGenRename (snd qn))],
                   foldr (\x xs -> applyF (pre ".")
                                    [applyF (pre "showChar") [Lit (Charc ' ')],
                                     applyF (pre ".") [x,xs]])
                         (applyF (pre "showChar") [Lit (Charc ')')])
                         (map (\i -> applyF (pre "shows") [mkVar "x" i])
                              [1..carity])]

-- ---------------------------------------------------------------------------
-- Generate instance of Read class:
--
-- TODO: No instance for higher-order constructors
-- ---------------------------------------------------------------------------
readInstance :: FC.TypeDecl -> TypeDecl
readInstance (FC.Type qf _ tnums cdecls)
  = mkInstance (pre "Read") [] ctype targs [rule]
    where
      targs = map fcy2absTVar tnums
      ctype = TCons qf (map TVar targs)
      rule | isListType  qf = readListRule qf
           | isTupleType qf = readTupleRule (head cdecls)
           | otherwise      = readRule cdecls

-- Generate special Read instance rule for lists

--instance Read t0 => Read (OP_List t0) where
--  readsPrec d s = map readList (readsPrec d s)
--   where
--     readList (xs,s) = (foldr OP_Cons OP_List xs,s)
readListRule :: QName -> (QName, Rule)
readListRule (mn, _) =
  ( pre "readsPrec"
  , Rule [PVar d, PVar s]
      [noGuard $ applyF (pre "map") [ constF (mn,"readList")
                                    , applyF (pre "readsPrec") [Var d, Var s]
                                    ]]
      [LocalFunc (ufunc (mn, "readList") 1 Private
        [simpleRule [tuplePat [PVar xs, PVar s2]]
           (tupleExpr [applyF (pre "foldr") [ constF (mn,"OP_Cons")
                                            , constF (mn,"OP_List")
                                            , Var xs
                                            ] , Var s2])])
      ]
  ) where [d,s,xs,s2] = newVars ["d","s","xs","s2"]

-- Generate special Read instance rule for tuple constructors:

--readTup ((x1, x2), s) = (OP_Tuple2 x1 x2, s)
readTupleRule :: FC.ConsDecl -> (QName, Rule)
readTupleRule (FC.Cons qn@(mn,_) carity _ _) =
  ( pre "readsPrec"
  , Rule [PVar d, PVar s]
      [noGuard $ applyF (pre "map") [ constF (mn,"readTup")
                                    , applyF (pre "readsPrec") [Var d, Var s]
                                    ]]
      [LocalFunc (ufunc (mn,"readTup") 1 Private
        [simpleRule [tuplePat [ tuplePat $ map (mkPVar "x") [1 .. carity]
                              , PVar s2
                              ]]
          (tupleExpr [ applyF qn (map (mkVar "x") [1 .. carity])
                     , Var s2])])]
  ) where [d,s,s2] = newVars ["d","s","s2"]


-- Generate Read instance rule for data constructors:

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
readRule :: [FC.ConsDecl] -> (QName, Rule)
readRule cdecls =
  ( pre "readsPrec"
  ,  simpleRule [PVar maybeD, PVar (1,"s")]
       (foldr1 (\e1 e2 -> applyF (pre "++") [e1, e2]) $ map readParen cdecls)
  )
  where
    maybeD = if isDNeeded then (0,"d") else (0,"_")
    isDNeeded = or [ arity > 0 | (FC.Cons _ arity _ _) <- cdecls ]

readParen :: FC.ConsDecl -> Expr
readParen (FC.Cons qn@(mn,_) carity _ _) = applyF (pre "readParen")
  [ if carity == 0 then constF (pre "False") -- no parentheses required
                   else applyF (pre ">") [Var (0, "d"), intc 10]
  , Lambda [PVar (2,"r")]
      (ListComp
        (tupleExpr [ applyF qn (map (mkVar "x") [1 .. carity])
                   , mkVar "r" carity
                   ]
        )
        (SPat (tuplePat [PVar (0, "_"), PVar (1, "r0")])
              (applyF (basics "readQualified")
                      [ string2ac (unRenameModule mn)
                      , string2ac (unGenRename (snd qn))
                      , Var (2,"r")
                      ] )
              : map genReadsPrec [1 .. carity]
        )
      )
  , Var (1, "s")
  ]
    where
    genReadsPrec i =
      SPat (tuplePat [mkPVar "x" i, mkPVar "r" i])
           (applyF (pre "readsPrec") [Lit (Intc 11), mkVar "r" (i - 1) ])


-- ---------------------------------------------------------------------------
-- Generate instance of NonDet class:
-- ---------------------------------------------------------------------------
nondetInstance :: FC.TypeDecl -> TypeDecl
nondetInstance (FC.Type qf _ tnums _)
  = mkInstance (basics "NonDet") [] ctype []
  $ specialConsRules qf ++ tryRules qf ++ matchRules qf
  where
    targs = map fcy2absTVar tnums
    ctype = TCons qf (map TVar targs)

specialConsRules :: QName -> [(QName, Rule)]
specialConsRules qf = map nameRule
  [ ("choiceCons" , mkChoiceName  qf)
  , ("choicesCons", mkChoicesName qf)
  , ("failCons"   , mkFailName    qf)
  , ("guardCons"  , mkGuardName   qf)
  ]
  where nameRule (name, fun) = (basics name, simpleRule [] (constF fun))

tryRules :: QName -> [(QName, Rule)]
tryRules qf = map nameRule
  [ simpleRule [mkChoicePattern  qf "i"] $ applyF (basics "tryChoice")  [cd, i, x, y]
  , simpleRule [mkChoicesPattern qf    ] $ applyF (basics "tryChoices") [cd, i, xs]
  , simpleRule [mkFailPattern    qf    ] $ applyF (basics "Fail")       [cd, info]
  , simpleRule [mkGuardPattern   qf    ] $ applyF (basics "Guard")      [cd, c, e]
  , simpleRule [PVar (2,"x")           ] $ applyF (basics "Val")        [x]
  ]
  where [i,x,y,xs,c,e,cd,info] = map Var $ newVars ["i","x","y","xs","c","e", "cd","info"]
        nameRule rule  = (basics "try", rule)

{-
match f _ _ _ _ _ (Choice i x y) = f i x y
match _ f _ _ _ _ (Choices i@(NarrowedID _ _) xs) = f i xs
match _ f _ _ _ _ (Choices i@(CovNarrowedID _ _ _) xs) = f i xs
match _ _ f _ _ _ (Choices i@(FreeID _ _) xs) = f i xs
match _ _ f _ _ _ (Choices i@(CovFreeID _ _ _) xs) = f i xs
match _ _ _ _ _ _ (Choices (ChoiceID _ _) _) = error " ..."
match _ _ _ f _ _ Fail = f
match _ _ _ _ f _ (Guard c e) = f c e
match _ _ _ _ _ f x = f x
-}
matchRules :: QName -> [(QName, Rule)]
matchRules qf = map nameRule
  [ simpleRule (matchAt 0 ++ [mkChoicePattern  qf "i"])
    $ applyV f [cd, i, x, y]
  , simpleRule (matchAt 1 ++ [mkNarrowedChoicesPattern qf "i"])
    $ applyV f [cd, i, xs]
  , simpleRule (matchAt 2 ++ [mkFreeChoicesPattern qf "i"])
    $ applyV f [cd, i, xs]
  , simpleRule (PVar (0,"_") : underscores ++ [mkVarChoicesPattern qf])
    $ applyF (pre "error")
      [ applyF (pre "++")
        [ string2ac (showQName (unRenameQName qf) ++ ".match: Choices with ChoiceID ")
        , applyF (pre "show") [i]
        ]
      ]
  , simpleRule (matchAt 3 ++ [mkFailPattern qf])
    $ applyV f [cd,info]
  , simpleRule (matchAt 4 ++ [mkGuardPattern qf])
    $ applyV f [cd, c, e]
  , simpleRule (matchAt 5 ++ [PVar (7,"x")])
    $ applyV f [x]
  ]
  where underscores = map PVar $ newVars ["_","_","_","_","_"]
        [i,x,y,xs,e,c,cd,info] = map Var $ newVars ["i","x","y","xs","e","c","cd","info"]
        f = (1, "f")
        nameRule rule  = (basics "match", rule)
        matchAt n = take n underscores ++ PVar f : drop n underscores

-- ---------------------------------------------------------------------------
-- Generate instance of Generable class
-- ---------------------------------------------------------------------------
-- TODO generators for constructor arguments can pe the same idsupplies
--      for different constructors; change bind accordingly

generableInstance :: FC.TypeDecl -> TypeDecl
generableInstance (FC.Type qf _ tnums cdecls) =
  mkInstance (basics "Generable") [] ctype targs
    [(basics "generate", simpleRule [PVar s] (genBody (Var s)))]
  where
    targs = map fcy2absTVar tnums
    ctype = TCons qf (map TVar targs)
    [s] = newVars ["s"]

    genBody idSupp =
      applyF (mkChoicesName qf)
      [ defCover
      , applyF (basics "freeID") [arities, idSupp]
      , list2ac $ map (genCons idSupp) cdecls
      ]

    genCons idSupp (FC.Cons qn arity _ _) = applyF qn (consArgs2gen idSupp arity)

    arities = list2ac $ map (intc . consArity) cdecls

    consArgs2gen idSupp n = map (applyF (basics "generate") . (:[]))
                          $ mkSuppList n idSupp

-- ---------------------------------------------------------------------------
-- Generate instance of NormalForm class:
-- ---------------------------------------------------------------------------
normalformInstance :: HOResult -> FC.TypeDecl -> TypeDecl
normalformInstance hoResult (FC.Type qf _ tnums cdecls) =
  mkInstance (basics "NormalForm") [] ctype targs $ concat
    -- $!!
  [ concatMap (normalformConsRule hoResult (basics "$!!") True) cdecls
  , normalFormExtConsRules qf (basics "$!!")
      (basics "nfChoice") (basics "nfChoices")
    -- $##
  , concatMap (normalformConsRule hoResult (basics "$##") True) cdecls
  , normalFormExtConsRules qf (basics "$##")
      (basics "gnfChoice") (basics "gnfChoices")
  -- searchNF
  , concatMap (searchNFConsRule hoResult) cdecls
  , [searchNFCatchRule qf]
  ]
  where targs = map fcy2absTVar tnums
        ctype = TCons qf (map TVar targs)
        --[cd, cont,i,x,y,xs] = newVars ["cd", "cont","i","x","y","xs"]

-- Generate NormalForm instance rule for a data constructor
normalformConsRule :: HOResult -> QName -> Bool -> FC.ConsDecl -> [(QName, Rule)]
normalformConsRule hoResult funcName withCs (FC.Cons qn _ _ texps)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons = lookupFM hoResult qn == Just HO
    carity = length texps
    rule name = (funcName, simpleRule
      ([PVar (1,"cont"), PComb name (map (\i -> PVar (i,'x':show i)) [1..carity])] ++ csPVar)
          (nfBody name))

    nfBody name =
      foldr (\i exp -> applyF funcName
                        ([Lambda ([PVar (i,'y':show i)] ++ csPVar) exp,Var (i,'x':show i)] ++ csVar))
            (applyV (1,"cont")
                    ([applyF name (map (\i -> Var (i,'y':show i)) [1..carity])] ++ csVar))
            [1..carity]
    csPVar = if withCs then [PVar (3,"cs")] else []
    csVar  = if withCs then [Var (3,"cs")] else []

normalFormExtConsRules :: QName -> QName -> QName -> QName -> [(QName, Rule)]
normalFormExtConsRules qf funcName choiceFunc choicesFunc =
  [(funcName, simpleRule [PVar cont, mkChoicePattern qf "i", PVar cs]
        (applyF choiceFunc
                [Var cont , Var cd, Var i, Var x, Var y, Var cs]))
  , (funcName, simpleRule [PVar cont, mkChoicesPattern qf , PVar cs]
        (applyF choicesFunc
                [Var cont, Var cd, Var i, Var xs, Var cs]))
  , (funcName, simpleRule [PVar cont, mkGuardPattern qf, PVar cs]
        (applyF (basics "guardCons")
                [Var cd, Var c, 
                 applyF funcName [Var cont, Var e  
                                 ,applyF (basics "addCs") [Var c, Var cs]]]))
  , (funcName, simpleRule [PVar us, mkFailPattern qf, PVar us]
                (applyF (basics "failCons") [Var cd, Var info]))
  ]

 where [info, c, cs, cd, cont,i,x,y,e,xs ,us] 
          = newVars ["info", "c", "cs", "cd", "cont","i","x","y","e","xs", "_"]

-- Generate searchNF instance rule for a data constructor
searchNFConsRule :: HOResult -> FC.ConsDecl -> [(QName, Rule)]
searchNFConsRule hoResult (FC.Cons qn carity _ _)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons  = lookupFM hoResult qn == Just HO
    rule name = ( basics "searchNF"
                , simpleRule [PVar mbSearch, PVar cont, consPattern name "x" carity]
                  (nfBody name)
                )

    nfBody name = foldr (\i exp -> applyV search
                      [Lambda [mkPVar "y" i] exp, mkVar "x" i])
          (applyV cont
                  [applyF name (map (mkVar "y") [1 .. carity])])
          [1..carity]
    [search,cont,us] = newVars ["search","cont","_"]
    mbSearch = if carity == 0 then us else search

searchNFCatchRule :: QName -> (QName, Rule)
searchNFCatchRule qf
  = ( basics "searchNF"
    , simpleRule [PVar us1, PVar us2, PVar x]
      ( applyF (pre "error")
          [ applyF (pre "++")
            [ string2ac (showQName (unRenameQName qf) ++ ".searchNF: no constructor: ")
            , applyF (pre "show") [Var x]
            ]
          ])
    )
  where [us1,us2,x] = newVars ["_","_","x"]

-- ---------------------------------------------------------------------------
-- Generate instance of Unifiable class:
-- ---------------------------------------------------------------------------
unifiableInstance :: HOResult -> FC.TypeDecl -> TypeDecl
unifiableInstance hoResult (FC.Type qf _ tnums cdecls) =
  mkInstance (basics "Unifiable") [] ctype targs $ concat
    -- unification
  [ concatMap (unifiableConsRule hoResult (basics "=.=") (basics "=:=")) cdecls
  , catchAllCase (basics "=.=") newFail
    -- lazy unification (functional patterns)
  , concatMap (unifiableConsRule hoResult (basics "=.<=") (basics "=:<=")) cdecls
  , catchAllCase (basics "=.<=") newFail
    -- bind
  , concatMap (bindConsRule hoResult (basics "bind") (\ident arg -> applyF (basics "bind") [ident, arg]) (applyF (pre "concat"))) (zip [0 ..] cdecls)
  , [ bindChoiceRule   qf (basics "bind")
    , bindFreeRule     qf (basics "bind")
    , bindNarrowedRule qf (basics "bind")
    , bindChoicesRule  qf (basics "bind")
    , bindFailRule     qf (basics "bind")
    , bindGuardRule    qf False
    ]
    -- lazy bind (function patterns)
  , concatMap (bindConsRule hoResult (basics "lazyBind") (\ident arg -> applyF (basics ":=:") [ident, applyF (basics "LazyBind") [applyF (basics "lazyBind") [ident, arg]]]) head) (zip [0 ..] cdecls)
  , [ bindChoiceRule   qf (basics "lazyBind")
    , bindFreeRule     qf (basics "lazyBind")
    , bindNarrowedRule qf (basics "lazyBind")
    , bindChoicesRule  qf (basics "lazyBind")
    , bindFailRule     qf (basics "lazyBind")
    , bindGuardRule    qf True
    ]
  ]
  where targs = map fcy2absTVar tnums
        ctype = TCons qf (map TVar targs)
        newFail = applyF (basics "Fail_C_Success") [defCover, applyF (basics "defFailInfo")[]]

-- Generate Unifiable instance rule for a data constructor
unifiableConsRule :: HOResult -> QName -> QName -> FC.ConsDecl -> [(QName, Rule)]
unifiableConsRule hoResult consFunc genFunc (FC.Cons qn _ _ texps)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons = lookupFM hoResult qn == Just HO
    rule name = ( consFunc, simpleRule [consPattern name "x" carity, consPattern name "y" carity, PVar cs]
              (unifBody genFunc) )
    unifBody funcName
      | carity == 0 = constF (basics "C_Success")
      | otherwise   = foldr1 (\x xs -> applyF (basics "&") [x, xs, Var cs])
                        (map (\i -> applyF funcName
                          [Var (i,'x':show i), Var (i,'y':show i), Var cs])
                        [1 .. carity])
    carity = length texps
    cs = (carity +1, "cs")

-- Generate bindRules for a data constructor:
--  bindConsRules :: [FC.ConsDecl] -> (Expr -> Expr) -> (Expr -> Expr) -> [Rule]
bindConsRule :: HOResult -> QName -> (Expr -> Expr -> Expr)
             -> ([Expr] -> Expr) -> (Int, FC.ConsDecl) -> [(QName, Rule)]
bindConsRule hoResult funcName bindArgs combine (num, (FC.Cons qn _ _ texps))
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]
  where
    isHoCons = lookupFM hoResult qn == Just HO
    rule name = (funcName,
      simpleRule [PVar (1, "i"), PComb name $ map (\i -> PVar (i, 'x':show i)) [2 .. (length texps) + 1] ]
        ( applyF (pre ":")
                  [ applyF (basics ":=:")
                    [ Var (1, "i")
                    , applyF (basics "ChooseN") [intc num, intc $ length texps]
                    ]
                  , combine [list2ac (zipWith bindArgs
                    (mkIdList (length texps) (Var (1, "i")))
                    (map (\i -> Var (i, 'x':show i)) [2 ..(length texps) + 1]))]
                  ]))

-- bind i (Choice_TYPENAME j l r) = [ConstraintChoice j (bind i l) (bind i r)]
-- lazyBind i (Choice_TYPENAME j l r) = [ConstraintChoice j (lazyBind i l) (lazyBind i r)]
bindChoiceRule :: QName -> QName -> (QName, Rule)
bindChoiceRule qf funcName = (funcName,
  simpleRule [PVar i, mkChoicePattern qf "j"]
    ( list2ac [ applyF (basics "ConstraintChoice")
                [Var cd, Var j
                , applyF funcName [Var i, Var x]
                , applyF funcName [Var i, Var y]
                ]
              ]
    )) where [i,j,x,y,cd] = newVars ["i","j","x","y","cd"]

-- bind i (Choices_TYPENAME j@(FreeID _ _) xs) = [i :=: BindTo j]
-- lazyBind i (Choices_TYPENAME j@(FreeID _ _) xs) = [i :=: BindTo j]
bindFreeRule ::QName -> QName -> (QName, Rule)
bindFreeRule qf funcName@(_,bname) = (funcName,
  simpleRule
    [ PVar i, mkFreeChoicesPattern qf "j"]
    (applyF (pre (bname ++ "OrNarrow")) [Var i, Var cd, Var j, Var xs])) 
    where [i,j,cd, xs] = newVars  ["i","j", "cd", "xs"]

-- bind i (Choices_TYPENAME j@(NarrowedID _ _) xs) = [ConstraintChoices j (map (bind i) xs)]
-- lazyBind i (Choices_TYPENAME j@(NarrowedID _ _) xs) = [ConstraintChoices j (map (lazyBind i) xs)]
bindNarrowedRule :: QName -> QName -> (QName, Rule)
bindNarrowedRule qf funcName = (funcName,
  simpleRule
    [ PVar i, mkNarrowedChoicesPattern qf "j"]
    ( list2ac [ applyF (basics "ConstraintChoices")
                [ Var cd, Var j
                , applyF (pre "map") [applyF funcName [Var i], Var xs]
                ]
              ]
    ))
    where [i,j,xs, cd] = newVars ["i","j","xs", "cd"]

-- bind _ c@(Choices_TYPENAME (ChoiceID _) _) = error ("Choices with ChoiceID: " ++ show c)
-- lazyBind _ c@(Choices_TYPENAME (ChoiceID _) _) = error ("Choices with ChoiceID: " ++ show c)
bindChoicesRule :: QName -> QName -> (QName, Rule)
bindChoicesRule qf funcName = (funcName,
  simpleRule
    [ PVar us, mkVarChoicesPattern qf]
    ( applyF (pre "error")
      [ applyF (pre "++")
        [ string2ac (showQName (unRenameQName qf) ++ '.' : snd funcName
                                  ++ ": Choices with ChoiceID: ")
        , applyF (pre "show") [Var i]
        ]
      ]
    ))
  where [us,i] = newVars ["_","i"]

-- bind _ Fail_TYPENAME = [Unsolvable]
-- lazyBind _ Fail_TYPENAME = [Unsolvable]
bindFailRule :: QName -> QName -> (QName, Rule)
bindFailRule qf funcName = (funcName,
  simpleRule [PVar (1, "_"), mkFailPattern qf]
              (list2ac [applyF (basics "Unsolvable") [Var info]]))
 where [info] = newVars ["info"]

-- bind i (Guard_TYPENAME cs e) = cs ++ bind i e
-- lazyBind i (Guard_TYPENAME cs e) = cs ++ [i :=: LazyBind (lazyBind i e)]
bindGuardRule :: QName -> Bool -> (QName, Rule)
bindGuardRule qf lazy = (funcName,
  simpleRule [PVar i, mkGuardPattern qf]
    (applyF (pre "++") [applyF (basics "getConstrList") [Var c], bindings]))
  where
    [i,c,e] = newVars ["i","c","e"]
    funcName = basics $ if lazy then "lazyBind" else "bind"
    bindings = if lazy
      then list2ac [applyF (basics ":=:")
                    [ Var i
                    , applyF (basics "LazyBind")
                        [applyF funcName [Var i, Var e]]
                    ]
                  ]
      else applyF funcName [Var i, Var e]

-- ---------------------------------------------------------------------------
-- Generate instance of Curry class
-- ---------------------------------------------------------------------------

curryInstance :: HOResult -> FC.TypeDecl -> TypeDecl
curryInstance hoResult (FC.Type qf _ tnums cdecls) =
  mkInstance (curryPre "Curry") [] ctype targs $ concat
    [ -- rules for equality
      extConsRules (curryPre "=?=") qf
    , eqConsRules hoResult cdecls
    , catchAllPattern (curryPre "=?=")
      -- rules for less than
    , extConsRules (curryPre "<?=") qf
    , ordConsRules hoResult cdecls
    , catchAllPattern (curryPre "<?=")
    ]
  where
    targs = map fcy2absTVar tnums
    ctype = TCons qf (map TVar targs)
    catchAllPattern qn
      | length cdecls > 1 = catchAllCase qn (constF (curryPre "C_False"))
      | otherwise         = []

extConsRules :: QName -> QName -> [(QName,Rule)]
extConsRules funcName qf = map nameRule
  [ simpleRule [mkChoicePattern qf "i", PVar z, PVar cs]
    (applyF narrow [ Var cd, Var i
                    , applyF funcName [Var x, Var z, Var cs]
                    , applyF funcName [Var y, Var z, Var cs]
                    ])
  , simpleRule [mkChoicesPattern qf, PVar y, PVar cs]
    (applyF narrows [ Var cs, Var cd, Var i
                    , Lambda [PVar x] (applyF funcName [Var x ,Var y, Var cs])
                    , Var xs
                    ])
  , simpleRule [mkGuardPattern qf, PVar y, PVar cs]
    (applyF (basics "guardCons") [ Var cd, Var c, applyF funcName [Var e, Var y, applyF (basics "addCs") [Var c, Var cs]]])
  , simpleRule [mkFailPattern qf, PVar p, PVar p]
    (applyF (basics "failCons") [Var cd, Var info])
  , simpleRule [PVar z, mkChoicePattern qf "i", PVar cs]
    (applyF narrow [ Var cd, Var i
                    , applyF funcName [Var z, Var x, Var cs]
                    , applyF funcName [Var z, Var y, Var cs]
                    ])
  , simpleRule [PVar y, mkChoicesPattern qf, PVar cs]
    (applyF narrows [ Var cs, Var cd, Var i
                    , Lambda [PVar x] (applyF funcName [Var y, Var x, Var cs])
                    , Var xs
                    ])
  , simpleRule [PVar y, mkGuardPattern qf, PVar cs]
    (applyF (basics "guardCons") [Var cd, Var c, applyF funcName [Var y, Var e, applyF (basics "addCs")[Var c, Var cs]]])
  , simpleRule [PVar p, mkFailPattern qf, PVar p]
    (applyF (basics "failCons") [Var cd, Var info])
  ]
    where nameRule rule = (funcName, rule)
          [i,x,y,z,xs,c,e,cs,p,cd,info] = 
	    newVars ["i","x","y","z","xs","c","e","cs","_","cd","info"]

-- Generate equality instance rule for a data constructor
eqConsRules :: HOResult -> [FC.ConsDecl] -> [(QName, Rule)]
eqConsRules hoResult = concatMap (eqConsRule hoResult)

eqConsRule :: HOResult -> FC.ConsDecl -> [(QName, Rule)]
eqConsRule hoResult (FC.Cons qn carity _ _)
  | isHoCons  = [rule qn, rule $ mkHoConsName qn]
  | otherwise = [rule qn]
  where
    isHoCons  = lookupFM hoResult qn == Just HO
    rule name = ( curryPre "=?="
                , simpleRule [ consPattern name "x" carity
                             , consPattern name "y" carity
                             , PVar cs
                             ] eqBody
                )
    eqBody    = if carity == 0
      then constF (curryPre "C_True")
      else foldr1
          (\x xs -> applyF (curryPre "d_OP_ampersand_ampersand") [x, xs, Var cs])
          (map (\i -> applyF (curryPre "=?=") [mkVar "x" i, mkVar "y" i, Var cs])
                [1..carity])
    cs = (2 * carity + 1, "cs")

-- Generate <?= rule for data constructors
ordConsRules :: HOResult -> [FC.ConsDecl] -> [(QName, Rule)]
ordConsRules _        [] = []
ordConsRules hoResult (FC.Cons qn carity _ _ : cds)
  | isHoCons  = concatMap rule [qn, mkHoConsName qn] ++ ordConsRules hoResult cds
  | otherwise = rule qn ++ ordConsRules hoResult cds
  where
    isHoCons    = lookupFM hoResult qn == Just HO
    rule name   = firstRule name : concatMap (ordCons2Rule hoResult (name, carity)) cds
    firstRule n = ( curryPre "<?=", simpleRule
                    [consPattern n "x" carity, consPattern n "y" carity, PVar cs]
                    (ordBody [1..carity]))

    ordBody l = case l of
      []     -> constF (curryPre "C_True")
      [i]    -> applyF (curryPre "<?=") [mkVar "x" i, mkVar "y" i, Var cs]
      (i:is) -> applyF (curryPre "d_OP_bar_bar")
                  [ applyF (curryPre "d_OP_lt") xiyi
                  , applyF (curryPre "d_OP_ampersand_ampersand")
                      [applyF (curryPre "=?=") xiyi, ordBody is, Var cs]
                  , Var cs
                  ] where xiyi = [mkVar "x" i, mkVar "y" i, Var cs]
    cs = (1,"cs")

ordCons2Rule :: HOResult -> (QName, Int) -> FC.ConsDecl -> [(QName, Rule)]
ordCons2Rule hoResult (qn1, ar1) (FC.Cons qn2 carity2 _ _)
  | isHoCons2 = [rule qn2, rule $ mkHoConsName qn2]
  | otherwise = [rule qn2]
  where
    isHoCons2 = lookupFM hoResult qn2 == Just HO
    rule name = (curryPre "<?=", simpleRule
                    [consPattern qn1 "_" ar1, consPattern name "_" carity2, PVar (1,"_")]
                    (constF $ curryPre "C_True"))


------------------------------------------------------------------------------------------
--  Generate instance of Coverable class
------------------------------------------------------------------------------------------

coverableInstance :: HOResult -> FC.TypeDecl -> TypeDecl
coverableInstance hoResult (FC.Type qf _ tnums cdecls) =
  mkInstance (basics "Coverable") [] ctype targs $ coverRules hoResult qf cdecls
  where
    targs = map fcy2absTVar tnums
    ctype = TCons qf (map TVar targs)

coverRules :: HOResult -> QName -> [FC.ConsDecl] -> [(QName,Rule)]
coverRules hoResult qn decls =
  map (\ r -> (cover,r))
   (concatMap  (mkCoverConsRule hoResult) decls
    ++ [ simpleRule [mkChoicePattern qn "i"] (applyF (mkChoiceName qn) [ applyF incCover [cd]
                                                                   , i
                                                                   , applyF cover [x]
                                                                   , applyF cover [y]])  
       , simpleRule [mkChoicesPattern qn] (applyF (mkChoicesName qn) 
                                                  [ applyF incCover [cd]
                                                  , i
                                                  , applyF (pre "map") [constF cover, xs]])
       , simpleRule [mkFailPattern qn] (applyF (mkFailName qn) 
                                               [applyF incCover [cd],info])
       , simpleRule [mkGuardPattern qn] (applyF (mkGuardName qn)
                                                [applyF incCover [cd], c, applyF cover [e]])
       ])
 where
  [i,x,y,xs,c,e,cd,info] = map Var $ newVars ["i","x","y","xs","c","e","cd","info"]

         
mkCoverConsRule :: HOResult -> FC.ConsDecl -> [Rule] 
mkCoverConsRule hoResult (FC.Cons conName carity _ _)
    | isHoCons conName = map rule [conName, mkHoConsName conName]
    | otherwise     = [rule conName]
 where  
  isHoCons name = lookupFM hoResult name == Just HO
  rule name = simpleRule [consPattern name "x" carity] 
                  (applyF  name (map (\n -> applyF cover [Var (n,"x" ++ show n)]) [1..carity]))

-- ---------------------------------------------------------------------------
-- Auxiliary functions
-- ---------------------------------------------------------------------------

mkChoicePattern  qn idStr = PComb (mkChoiceName  qn) [PVar cd, PVar idVar , PVar x, PVar y]
  where [cd, idVar ,x,y] = newVars ["cd", idStr ,"x","y"]
mkChoicesPattern qn = PComb (mkChoicesName qn) [PVar cd, PVar i, PVar xs]
  where [cd, i,xs] = newVars ["cd", "i","xs"]
mkNarrowedChoicesPattern qn asName = PComb (mkChoicesName qn)
 [PVar cd, PAs i (PComb (basics "NarrowedID") [PVar u1, PVar u2]), PVar xs]
 where [i,cd, u1,u2,xs] = newVars [asName,"cd","_","_","xs"]
mkFreeChoicesPattern qn asName = PComb (mkChoicesName qn)
 [PVar cd, PAs i (PComb (basics "FreeID") [PVar u1, PVar u2]), PVar xs]
 where [i,cd,u1,u2,xs] = newVars [asName,"cd", "_","_","xs"]
mkVarChoicesPattern qn = PComb (mkChoicesName qn)
 [PVar cd, PVar i, PVar xs]
 where [cd,i,xs] = newVars ["cd","i","_"]
mkFailPattern    qn = PComb (mkFailName    qn) [PVar cd, PVar info]
  where [cd,info] = newVars ["cd","info"]
mkGuardPattern   qn = PComb (mkGuardName   qn) [PVar cd, PVar c, PVar e]
  where [cd, c,e] = newVars ["cd", "c","e"]

mkPVar n i = PVar $ mkVarName n i

mkVar n i = Var $ mkVarName n i

mkVarName n i
  | n == "_"  = (i, n)
  | otherwise = (i, n ++ show i)

newVars :: [String] -> [(Int, String)]
newVars = zip [1..]

mkInstance qn addContexts ctype targs
  = Instance qn ctype $ concatMap (\name -> map (\tv -> Context name [tv]) targs) (qn:addContexts)

consPattern qn varName carity
  = PComb qn $ map (PVar . mkVarName varName) [1 .. carity]

catchAllCase qn retVal
  = [(qn, simpleRule [PVar (1,"_"), PVar (2,"_"), PVar (3, "_")] retVal)]

simpleRule patterns body = Rule patterns [noGuard body] []

intc i = Lit $ Intc i

charc c = Lit $ Charc c

mkIdList num initid
  | num == 0    = []
  | num == 1    = [left initid]
  | otherwise   = mkIdList' num initid
  where
    mkIdList' n i
      | n == 1    = [i]
      | otherwise = mkIdList' (n - half) (left i) ++ mkIdList' half (right i)
      where
        half = n `div` 2

mkSuppList num supp
  | num == 0    = []
  | num == 1    = [leftsupp supp]
  | otherwise   = mkSuppList' num supp
  where
    mkSuppList' n s
      | n == 1    = [s]
      | otherwise = mkSuppList' (n - half) (leftsupp s) ++ mkSuppList' half (rightsupp s)
      where
        half = n `div` 2

isListType   q    = q == renameQName ("Prelude", "[]")

isTupleType (m,t) = m == renameModule "Prelude" && take 8 t == "OP_Tuple"

showQName :: QName -> String
showQName (m,t) = m ++ '.' : t

-- ---------------------------------------------------------------------------
-- Frequently used symbols
-- ---------------------------------------------------------------------------

left :: Expr -> Expr
left  i = applyF (basics "leftID" ) [i]

right :: Expr -> Expr
right i = applyF (basics "rightID") [i]

leftsupp :: Expr -> Expr
leftsupp  s = applyF (basics "leftSupply" ) [s]

rightsupp :: Expr -> Expr
rightsupp s = applyF (basics "rightSupply") [s]

idType :: TypeExpr
idType = baseType (basics "ID")

coverType :: TypeExpr
coverType = baseType (basics "Cover")

defCover :: Expr
defCover = applyF (basics "defCover") []

failInfoType :: TypeExpr
failInfoType = baseType (basics "FailInfo")

constraintType :: TypeExpr
constraintType = baseType (basics "Constraints")

basics :: String -> QName
basics n = ("Basics", n)

curryPre :: String -> QName
curryPre n = (renameModule "Prelude", n)

narrow :: QName
narrow = basics "narrow"

narrows :: QName
narrows = basics "narrows"

cover :: QName
cover = basics "cover"

incCover :: QName
incCover = basics "incCover"


