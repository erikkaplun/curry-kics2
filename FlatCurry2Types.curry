-- ---------------------------------------------------------------------------
--- Generation of data and instance declarations for the Curry->Haskell
--- compiler.
--- The input is a FlatCurry program and the output is an AbstractHaskell
--- program with instance declarations that can be easily pretty printed
---
--- @author Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version April 2011
-- ---------------------------------------------------------------------------
module FlatCurry2Types (fcyTypes2abs) where

import qualified FlatCurry as FC
import FlatCurryGoodies
import AbstractHaskell
import AbstractHaskellGoodies
import FlatCurry2AbstractHaskell
import List (intersperse)
import Names
  ( mkChoiceName, mkChoicesName, mkFailName, mkGuardName
  , renameModule, unGenRename, unRenameModule)

-- ---------------------------------------------------------------------------
-- Generate code for user-defined types
-- ---------------------------------------------------------------------------

--- Translate a list of FlatCurry type declarations into the
--- corresponding type and instance declarations for Haskell.
fcyTypes2abs :: [FC.TypeDecl] -> [TypeDecl]
fcyTypes2abs = concatMap genTypeDefinitions

genTypeDefinitions :: FC.TypeDecl -> [TypeDecl]
genTypeDefinitions (FC.TypeSyn qf vis targs texp) =
  [TypeSyn qf (fcy2absVis vis) (map fcy2absTVar targs) (fcy2absTExp texp)]
genTypeDefinitions (FC.Type (mn,tc) vis tnums cdecls) = if null cdecls
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
  acvis           = fcy2absVis vis
  targs           = map fcy2absTVar tnums
  ctype           = TCons (mn,tc) (map TVar targs)
  clisttype       = listType ctype
  choiceConsName  = mkChoiceName (mn,tc)
  choicesConsName = mkChoicesName (mn,tc)
  failConsName    = mkFailName (mn,tc)
  guardConsName   = mkGuardName (mn,tc)


-- ---------------------------------------------------------------------------
-- Generate instance of Show class:
-- ---------------------------------------------------------------------------
  showInstance = mkInstance (basics "Show") [] ctype targs $
    if tc=="OP_List" then [showRule4List] else
    ([( pre "showsPrec"
      , simpleRule [PVar (1,"d"),
          PComb choiceConsName [PVar (2,"i"),PVar (3,"x"),PVar (4,"y")]]
          (applyF (basics "showsChoice")
                  [Var (1,"d"),Var (2,"i"), Var (3,"x"),Var (4,"y")])),
      (pre "showsPrec",
      simpleRule
        [PVar (1,"d"), PComb choicesConsName [PVar (2,"i"),PVar (3,"xs")]]
        (applyF (basics "showsChoices") [Var (1,"d"),Var (2,"i"),Var (3,"xs")])),
      (pre "showsPrec",
      simpleRule
        [PVar (1,"d"), PComb guardConsName [PVar (2,"c"),PVar (3,"e")]]
        (applyF (basics "showsGuard") [Var (1,"d"),Var (2,"c"),Var (3,"e")])),
      (pre "showsPrec",
      simpleRule [PVar (1, "_"), PComb failConsName []]
        (applyF (pre "showChar") [charc '!']))]
      ++ map showConsRule cdecls)

  -- Generate specific show for lists (only for finite lists!)
  showRule4List =
     (pre "showsPrec",
      Rule [] [noGuard (constF (pre "showsPrec4CurryList"))] [])

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

-- ---------------------------------------------------------------------------
-- Generate instance of Read class:
-- ---------------------------------------------------------------------------
  readInstance = mkInstance (pre "Read") [] ctype targs
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

-- ---------------------------------------------------------------------------
-- Generate instance of NonDet class:
-- ---------------------------------------------------------------------------
  nondetInstance = mkInstance (basics "NonDet") [] ctype []
     (map mkSpecialConsRule [ ("choiceCons" , choiceConsName)
                            , ("choicesCons", choicesConsName)
                            , ("failCons"   , failConsName)
                            , ("guardCons"  , guardConsName)
                            ]
     ++ map (\r-> (basics "try", r)) tryRules)

  mkSpecialConsRule (name, fun) = (basics name, simpleRule [] (constF fun))

  tryRules =
   [ simpleRule
      [PComb choiceConsName [PVar (1,"i"), PVar (2,"x"), PVar (3,"y")]]
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

  generableInstance = mkInstance (basics "Generable") [] ctype targs
    [(basics "generate", simpleRule [PVar (1,"s")] (genBody (Var (1, "s"))))]

  genBody idSupp =
    applyF choicesConsName [(applyF (basics "freeID") [idSupp]), list2ac $
      map (\(FC.Cons qn _ _ texps) -> applyF qn (consArgs2gen idSupp (length texps))) cdecls]

  consArgs2gen idSupp n = map (applyF (basics "generate") . (:[])) $ mkSuppList n idSupp

-- ---------------------------------------------------------------------------
-- Generate instance of NormalForm class:
-- ---------------------------------------------------------------------------
  normalformInstance
    = mkInstance (basics "NormalForm") [] ctype targs $ concat
      -- $!!
      [ map (normalformConsRule (basics "$!!")) cdecls
      , normalFormExtConsRules (basics "$!!")
          (basics "nfChoice") (basics "nfChoices")
      -- $##
      , map (normalformConsRule (basics "$##")) cdecls
      , normalFormExtConsRules (basics "$##")
          (basics "gnfChoice") (basics "gnfChoices")
      -- $!<
      , map (normalformConsRule (basics "$!<")) cdecls
      , [ (basics "$!<", simpleRule [PVar (1,"cont"),
            PComb choiceConsName [PVar (2,"i"), PVar (3,"x"), PVar (4,"y")]]
              (applyF (basics "nfChoiceIO")
                      [Var (1,"cont"),Var (2,"i"), Var (3,"x"),Var (4,"y")]))
        , (basics "$!<", simpleRule [PVar (1,"cont"),
            PComb choicesConsName [PVar (2,"i"), PVar (3,"xs")]]
              (applyF (basics "nfChoicesIO")
                      [Var (1,"cont"),Var (2,"i"), Var (3,"xs")]))
        , (basics "$!<", simpleRule [PVar (1,"cont"), PVar (2,"x")]
              (applyV (1,"cont") [Var (2,"x")]))
        ]
      -- searchNF
      , map searchNFConsRule cdecls
      ]

  -- Generate NormalForm instance rule for a data constructor:
  normalformConsRule funcName (FC.Cons qn _ _ texps) =
    (funcName, simpleRule
      [PVar (1,"cont"), PComb qn (map (\i -> PVar (i,'x':show i)) [1..carity])]
          nfBody)
   where
     carity = length texps

     nfBody =
      foldr (\i exp -> applyF funcName
                        [Lambda [PVar (i,'y':show i)] exp,Var (i,'x':show i)])
            (applyV (1,"cont")
                    [applyF qn (map (\i -> Var (i,'y':show i)) [1..carity])])
            [1..carity]

  normalFormExtConsRules funcName choiceFunc choicesFunc =
    [ (funcName, simpleRule [PVar (1,"cont"),
        PComb choiceConsName [PVar (2,"i"), PVar (3,"x"), PVar (4,"y")]]
          (applyF choiceFunc
                  [Var (1,"cont"),Var (2,"i"), Var (3,"x"),Var (4,"y")]))
    , (funcName, simpleRule [PVar (1,"cont"),
        PComb choicesConsName [PVar (2,"i"), PVar (3,"xs")]]
          (applyF choicesFunc
                  [Var (1,"cont"),Var (2,"i"), Var (3,"xs")]))
    , (funcName, simpleRule [PVar (1,"cont"),
        PComb guardConsName [PVar (2,"c"),PVar (3,"x")]]
          (applyF (basics "guardCons")
                  [Var (2,"c"),applyF funcName [Var (1,"cont"),Var (3,"x")]]))
    , (funcName, simpleRule [PVar (1,"_"), PComb failConsName []]
                  (Symbol (basics "failCons")))
    ]

  -- Generate searchNF instance rule for a data constructor:
  searchNFConsRule  (FC.Cons qn _ _ texps) =
    ( basics "searchNF"
    , simpleRule [PVar (1,"search"), PVar (2,"cont"), consPattern qn "x" carity]
      nfBody)
   where
     carity = length texps

     nfBody =
      foldr (\i exp -> applyV (1, "search")
                        [Lambda [PVar (i,'y':show i)] exp,Var (i,'x':show i)])
            (applyV (1,"cont")
                    [applyF qn (map (\i -> Var (i,'y':show i)) [1..carity])])
            [1..carity]

-- ---------------------------------------------------------------------------
-- Generate instance of Unifiable class:
-- ---------------------------------------------------------------------------
  unifiableInstance
    = mkInstance (basics "Unifiable") [] ctype targs $ concat
       -- unification
      [ map (unifiableConsRule (basics "=.=") (basics "=:=")) cdecls
      , catchAllCase (basics "=.=") (basics "Fail_C_Success")
        -- lazy unification (function patterns)
      , map (unifiableConsRule (basics "=.<=") (basics "=:<=")) cdecls
      , catchAllCase (basics "=.<=") (basics "Fail_C_Success")
        -- bind
      , map (bindConsRule (basics "bind") (\ident arg -> applyF (basics "bind") [ident, arg]) (applyF (pre "concat"))) (zip [0 ..] cdecls)
      , [ bindChoiceRule   (basics "bind")
        , bindFreeRule     (basics "bind")
        , bindNarrowedRule (basics "bind")
        , bindFailRule     (basics "bind")
        , bindGuardRule    False
        ]
        -- lazy bind (function patterns)
      , map (bindConsRule (basics "lazyBind") (\ident arg -> applyF (basics ":=:") [ident, applyF (basics "LazyBind") [applyF (basics "lazyBind") [ident, arg]]]) head) (zip [0 ..] cdecls)
      , [ bindChoiceRule   (basics "lazyBind")
        , bindFreeRule     (basics "lazyBind")
        , bindNarrowedRule (basics "lazyBind")
        , bindFailRule     (basics "lazyBind")
        , bindGuardRule    True
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

  -- bind i (Choice_TYPENAME j l r) = [ConstraintChoice j (bind i l) (bind i r)]
  -- lazyBind i (Choice_TYPENAME j l r) = [ConstraintChoice j (lazyBind i l) (lazyBind i r)]
  bindChoiceRule funcName = (funcName,
    simpleRule [PVar (1,"i"), PComb choiceConsName [PVar (2,"j"), PVar (3,"l"), PVar (4,"r")]]
      ( list2ac [ applyF (basics "ConstraintChoice")
                  [ Var (2,"j")
                  , applyF funcName [Var (1, "i"), Var (3, "l")]
                  , applyF funcName [Var (1, "i"), Var (4, "r")]
                  ]
                ]
      ))

  -- bind i (Choices_TYPENAME j@(FreeID _) xs) = [i :=: BindTo j]
  -- lazyBind i (Choices_TYPENAME j@(FreeID _) xs) = [i :=: BindTo j]
  bindFreeRule funcName = (funcName,
    simpleRule
      [ PVar (1,"i")
      , PComb choicesConsName [PAs (2,"j") (PComb (basics "FreeID") [PVar (3,"_")])
      , PVar (4,"xs")]
      ]
      ( list2ac [ applyF (basics ":=:")
                  [ Var (1,"i")
                  , applyF (basics "BindTo") [Var (2,"j")]
                  ]
                ]
      ))

  -- bind i (Choices_TYPENAME j@(Narrowed _) xs) = [ConstraintChoices j (map (bind i) xs)]
  -- lazyBind i (Choices_TYPENAME j@(Narrowed _) xs) = [ConstraintChoices j (map (lazyBind i) xs)]
  bindNarrowedRule funcName = (funcName,
    simpleRule
      [ PVar (1,"i")
      , PComb choicesConsName [PAs (2,"j") (PComb (basics "Narrowed") [PVar (3,"_")])
      , PVar (4,"xs")]
      ]
      ( list2ac [ applyF (basics "ConstraintChoices")
                  [ Var (2,"j")
                  , applyF (pre "map")
                     [applyF funcName [Var (1,"i")]
                     , Var (4,"xs")]
                  ]
                ]
      ))

  -- bind _ Fail_TYPENAME = [Unsolvable]
  -- lazyBind _ Fail_TYPENAME = [Unsolvable]
  bindFailRule funcName = (funcName,
    simpleRule [PVar (1, "_"), PComb failConsName []]
               (list2ac [constF (basics "Unsolvable")]))

  -- bind i (Guard_TYPENAME cs e) = cs ++ bind i e
  -- lazyBind i (Guard_TYPENAME cs e) = cs ++ [i :=: LazyBind (lazyBind i e)]
  bindGuardRule lazy = (funcName,
    simpleRule [PVar (1, "i"), PComb guardConsName [PVar (2,"cs"), PVar (3,"e")]]
      (applyF (pre "++") [Var (2, "cs"), rest]))
    where
      funcName = basics $ if lazy then "lazyBind" else "bind"
      rest = if lazy
        then list2ac [applyF (basics ":=:")
                      [ Var (1, "i")
                      , applyF (basics "LazyBind")
                          [applyF funcName [Var (1, "i"), Var (3, "e")]]
                      ]
                    ]
        else applyF funcName [Var (1, "i"), Var (3, "e")]


-- ---------------------------------------------------------------------------
-- Generate instance of Curry class:
-- ---------------------------------------------------------------------------

  curryInstance = mkInstance (curryPre "Curry") [] ctype targs $ concat
     [ extConsRules (curryPre "=?="), map eqConsRule cdecls, catchAllPattern (curryPre "=?=")
     , extConsRules (curryPre "<?="), ordConsRules cdecls  , catchAllPattern (curryPre "<?=")
     ]
    where
     catchAllPattern qn
       | length cdecls > 1 = catchAllCase qn (curryPre "C_False")
       | otherwise         = []

  extConsRules name =
    [ nameRule $ simpleRule
      [PComb choiceConsName [PVar (1,"i"), PVar (2,"x"), PVar (3,"y")], PVar (4,"z")]
      (applyF narrow [ Var (1,"i")
                     , applyF name [Var (2, "x"), Var (4, "z")]
                     , applyF name [Var (3, "y"), Var (4, "z")]
                     ])
    , nameRule $ simpleRule
      [PComb choicesConsName [PVar (1,"i"), PVar (2,"xs")], PVar (3,"y")]
      (applyF narrows [ Var (1,"i")
                      , applyF (pre "map")
                               [ Lambda [PVar (4,"x")] (applyF name [Var (4,"x"),Var (3,"y")])
                               , Var (2,"xs")
                               ]])
    , nameRule $ simpleRule
      [PComb guardConsName [PVar (1,"c"),PVar (2,"x")], PVar(3,"y")]
      (applyF (basics "guardCons") [ Var (1,"c")
                                , applyF name [Var (2,"x"),Var (3,"y")]
                                ])
    , nameRule $ simpleRule [PComb failConsName [], PVar (1,"_")]
                            (Symbol (basics "failCons"))
    , nameRule $ simpleRule
      [PVar (4,"z"), PComb choiceConsName [PVar (1,"i"), PVar (2,"x"), PVar (3,"y")]]
      (applyF narrow [ Var (1,"i")
                     , applyF name [Var (4,"z"),Var (2,"x")]
                     , applyF name [Var (4,"z"),Var (3, "y")]
                     ])
    , nameRule $ simpleRule
      [PVar (3,"y"), PComb choicesConsName [PVar (1,"i"), PVar (2,"xs")]]
      (applyF narrows [ Var (1,"i")
                      , applyF (pre "map")
                               [ Lambda [PVar (4,"x")] (applyF name [Var (3,"y"),Var (4,"x")])
                               , Var (2,"xs")
                               ]])
    , nameRule $ simpleRule
      [PVar(3,"y"), PComb guardConsName [PVar (1,"c"),PVar (2,"x")]]
      (applyF (basics "guardCons")
        [Var (1,"c"), applyF name [Var (3,"y"),Var (2,"x")]])
    , nameRule $ simpleRule [PVar (1,"_"), PComb failConsName []]
                            (Symbol (basics "failCons"))
    ]
    where nameRule rule = (name, rule)

  -- Generate equality instance rule for a data constructor:
  eqConsRule (FC.Cons qn _ _ texps) =
    ( curryPre "=?="
    , simpleRule [consPattern qn "x" carity, consPattern qn "y" carity] eqBody
    )
    where
      carity = length texps

      eqBody = if carity == 0
        then constF (curryPre "C_True")
        else foldr1 (\x xs -> applyF (curryPre "d_OP_ampersand_ampersand") [x,xs])
                    (map (\i -> applyF (curryPre "=?=")
                                       [Var (i,'x':show i),Var (i,'y':show i)])
                    [1..carity])

  -- Generate Unifiable instance rule for a data constructor:
  ordConsRules [] = []
  ordConsRules (FC.Cons qn _ _ texps : cds) =
    (curryPre "<?=",
     simpleRule [consPattern qn "x" carity, consPattern qn "y" carity]
          (ordBody [1..carity])) : map (ordCons2Rule (qn,carity)) cds ++
    ordConsRules cds
   where
     carity = length texps

     ordBody [] = constF (curryPre "C_True")
     ordBody (i:is) =
      let xi = Var (i,'x':show i)
          yi = Var (i,'y':show i)
       in if null is
          then applyF (curryPre "<?=") [xi,yi]
          else applyF (curryPre "d_OP_bar_bar")
                 [applyF (curryPre "d_OP_lt")  [xi,yi],
                  applyF (curryPre "d_OP_ampersand_ampersand") [applyF (curryPre "=?=") [xi,yi], ordBody is]]

  ordCons2Rule (qn1,ar1) (FC.Cons qn2 _ _ texps2) =
    (curryPre "<?=", simpleRule
                [ PComb qn1 (map (\i -> PVar (i,"_")) [1..ar1])
                , PComb qn2 (map (\i -> PVar (i,"_")) [1..(length texps2)])]
                (constF (curryPre "C_True")))

-- ---------------------------------------------------------------------------
-- Auxiliary functions
-- ---------------------------------------------------------------------------

mkInstance qn addContexts ctype targs
  = Instance qn ctype $ concatMap (\name -> map (\tv -> Context name [tv]) targs) (qn:addContexts)

consPattern qn varName carity
  = PComb qn $ map (\i -> PVar (i, varName ++ show i)) [1 .. carity]

catchAllCase qn retVal
  = [(qn, simpleRule [PVar (1,"_"), PVar (2,"_")] (constF retVal))]

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

-- ---------------------------------------------------------------------------
-- Frequently used symbols
-- ---------------------------------------------------------------------------

left  i = applyF (basics "leftID" ) [i]
right i = applyF (basics "rightID") [i]

leftsupp  s = applyF (basics "leftSupply" ) [s]
rightsupp s = applyF (basics "rightSupply") [s]

idType = baseType (basics "ID")

constraintType = listType $ baseType (basics "Constraint")

basics :: String -> QName
basics n = ("Basics", n)

curryPre :: String -> QName
curryPre n = (renameModule "Prelude", n)

narrow = basics "narrow"
narrows = basics "narrows"
