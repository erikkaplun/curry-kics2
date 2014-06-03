--- --------------------------------------------------------------------------
--- This module forms the main part of the compiler as it defines
--- the translation of function declarations from FlatCurry into Haskell.
---
--- @author Bjoern Peemoeller, Fabian Skrlac
--- @version May 2014
--- --------------------------------------------------------------------------
{-# LANGUAGE Records #-}
module TransFunctions ( State (..), defaultState, trProg, runIOES ) where

import FiniteMap (lookupFM, plusFM, delListFromFM)
import Function  (first)

import qualified AbstractHaskell        as AH
import qualified AbstractHaskellGoodies as AH
import           Analysis
import           CompilerOpts  (Options (..), defaultOptions, OptimLevel (..))
import           FlatCurry
import           LiftCase      (isCaseAuxFuncName)
import           Message       (showAnalysis)
import           Names

-- ---------------------------------------------------------------------------
-- IO error state monad, like `EitherT (StateT IO)`
-- ---------------------------------------------------------------------------

data IOES s a = M (s -> IO (Either String (a, s)))

runIOES :: IOES s a -> s -> IO (Either String (a, s))
runIOES (M x) = x

returnM :: a -> IOES s a
returnM x = M $ \s -> return (Right (x, s))

(>+=) :: IOES s a -> (a -> IOES s b) -> IOES s b
f >+= g = M $ \s -> do
  eex <- runIOES f s
  case eex of
    Left  err     -> return (Left err)
    Right (x, s') -> runIOES (g x) s'

(>+) :: IOES s a -> IOES s b -> IOES s b
f >+ g = f >+= \_ -> g

failM :: String -> IOES s a
failM err = M $ \_ -> return (Left err)

getState :: IOES s s
getState = M $ \s -> return (Right (s, s))

putState :: s -> IOES s ()
putState s = M $ \ _ -> return (Right ((), s))

updState :: (s -> s) -> IOES s ()
updState f = getState >+= \s -> putState (f s)

liftIO :: IO a -> IOES s a
liftIO act = M $ \s -> do
  a <- act
  return (Right (a, s))

liftM :: (a -> b) -> IOES s a -> IOES s b
liftM f act = act >+= \x -> returnM (f x)

liftM2 :: (a -> b -> c) -> IOES s a -> IOES s b -> IOES s c
liftM2 f a b = a >+= \x -> b >+= \y -> returnM (f x y)

mapM :: (a -> IOES s b) -> [a] -> IOES s [b]
mapM _ [] = returnM []
mapM f (m:ms) = f m       >+= \m'  ->
                mapM f ms >+= \ms' ->
                returnM (m':ms')

foldM :: (a -> b -> IOES s a) -> a -> [b] -> IOES s a
foldM _ e []       = returnM e
foldM f e (x : xs) = f e x >+= \fex -> foldM f fex xs

-- ---------------------------------------------------------------------------
-- Internal state and access functions
-- ---------------------------------------------------------------------------

type State =
  { typeMap      :: TypeMap
  , ndResult     :: NDResult
  , hoResultType :: TypeHOResult
  , hoResultCons :: ConsHOResult
  , hoResultFunc :: FuncHOResult
  , nextID       :: VarIndex    -- index for fresh variable
  , detMode      :: Bool        -- determinism mode
  , compOptions  :: Options     -- compiler options
  }

defaultState :: State
defaultState =
  { typeMap      := initTypeMap
  , ndResult     := initNDResult
  , hoResultType := initTypeHOResult
  , hoResultCons := initHOResult
  , hoResultFunc := initHOResult
  , nextID       := 0
  , detMode      := False
  , compOptions  := defaultOptions
  }

type M a = IOES State a

-- type map

addTypeMap :: TypeMap -> M ()
addTypeMap newTypes =
 updState (\st -> { typeMap :=  st :> typeMap `plusFM` newTypes  | st })


getType :: QName -> M QName
getType qn = getState >+= \st ->
  maybe (failM $ show qn ++ " not in type map") returnM
  $ (flip lookupFM) qn (st :> typeMap)

-- NDResult

addNDAnalysis :: NDResult -> M ()
addNDAnalysis newRes = updState $ \st ->
  { ndResult := newRes `plusFM` st :> ndResult | st }

getNDClass :: QName -> M NDClass
getNDClass qn = getState >+= \st ->
  maybe (failM $ show qn ++ " not analysed") returnM
  $ (flip lookupFM) qn (st :> ndResult)

-- HOTypeResult

addHOTypeAnalysis :: TypeHOResult -> M ()
addHOTypeAnalysis newRes = updState$ \st ->
  { hoResultType := (newRes `plusFM` st :> hoResultType) | st }

getTypeHOClass :: QName -> M TypeHOClass
getTypeHOClass qn = getState >+= \st ->
  maybe (failM $ show qn ++ " not analysed") returnM
  $ (flip lookupFM) qn (st :> hoResultType)

-- HOConsResult

addHOConsAnalysis :: ConsHOResult -> M ()
addHOConsAnalysis newRes = updState$ \st ->
  { hoResultCons := (newRes `plusFM` st :> hoResultCons) | st }

getConsHOClass :: QName -> M ConsHOClass
getConsHOClass qn = getState >+= \st ->
  maybe (failM $ show qn ++ " not analysed") returnM
  $ (flip lookupFM) qn $ (st :> hoResultCons)

-- HOFunResult

addHOFuncAnalysis :: FuncHOResult -> M ()
addHOFuncAnalysis newRes = updState$ \st ->
  { hoResultFunc := newRes `plusFM` st :> hoResultFunc | st }

getFuncHOClass :: QName -> M FuncHOClass
getFuncHOClass qn = getState >+= \st ->
  maybe (failM $ show qn ++ " not analysed") returnM
  $ (flip lookupFM) qn $ (st :> hoResultFunc)

-- IDs

getNextID :: M Int
getNextID = getState >+= \st -> returnM (st :> nextID)

setNextID :: Int -> M ()
setNextID i = updState (\st -> { nextID := i | st })

takeNextID :: M Int
takeNextID =
  getState >+= \st ->
  let i = st :> nextID in
  putState ({ nextID := (i + 1) | st }) >+
  returnM i

takeNextIDs :: Int -> M [Int]
takeNextIDs n =
  getState >+= \st ->
  let i = st :> nextID in
  putState ({ nextID := (i + n) | st }) >+
  returnM [i .. i+n-1]

-- DetMode

isDetMode :: M Bool
isDetMode = getState >+= \st -> returnM (st :> detMode)

setDetMode :: Bool -> M ()
setDetMode dm = updState (\st -> { detMode := dm | st})

-- Perform an action in a given detMode and restore the original mode
-- afterwards
doInDetMode :: Bool -> M a -> M a
doInDetMode dm action =
  isDetMode      >+= \old ->
  setDetMode dm  >+
  action         >+= \res ->
  setDetMode old >+
  returnM res

isTraceFailure :: M Bool
isTraceFailure = getState >+= \st ->
                 returnM (st :> compOptions :> optTraceFailure)

-- Compiler options
getCompOptions :: M Options
getCompOptions = getState >+= \ st -> returnM (st :> compOptions)

getCompOption :: (Options -> a) -> M a
getCompOption select = getCompOptions >+= (returnM . select)

strictSupply :: M Bool
strictSupply = getCompOption $ \opts ->
  (opts :> optOptimization >= OptimStrictSupply)

-- ---------------------------------------------------------------------------
-- Program translation
-- ---------------------------------------------------------------------------

trProg :: Prog -> M (AH.Prog, AnalysisResult)
trProg p@(Prog m is ts fs _) =
  getState >+= \st ->
  let modNDRes     = analyseND     p (st :> ndResult)
      modHOResType = analyseHOType p (st :> hoResultType)
      modHOResCons = analyseHOCons p
      modHOResFunc = analyseHOFunc p (st :> hoResultType `plusFM` modHOResType)
      modTypeMap   = getTypeMap    ts
      visInfo      = analyzeVisibility p

      visNDRes     = modNDRes     `delListFromFM` getPrivateFunc visInfo

      visHOType    = modHOResType `delListFromFM` getPrivateType visInfo
      visHOCons    = modHOResCons `delListFromFM` getPrivateCons visInfo
      visHOFun     = modHOResFunc `delListFromFM` getPrivateFunc visInfo

      visType      = modTypeMap   `delListFromFM` getPrivateCons visInfo
      anaResult    = (visType, visNDRes, visHOType, visHOCons, visHOFun)
  in
  addNDAnalysis     modNDRes     >+
  addHOTypeAnalysis modHOResType >+
  addHOConsAnalysis modHOResCons >+
  addHOFuncAnalysis modHOResFunc >+
  addTypeMap        modTypeMap   >+
  -- translation of the functions
  mapM trFunc fs >+= \fss ->
  returnM $ (AH.Prog m is [] (concat fss) [], anaResult)

-- ---------------------------------------------------------------------------
-- Translation of function declarations
-- ---------------------------------------------------------------------------

trFunc :: FuncDecl -> M [AH.FuncDecl]
trFunc f@(Func qn _ _ _ _) =
  checkGlobal f  >+
  getCompOptions >+= \opts ->
  case opts :> optOptimization > OptimNone of
    -- translate all functions as non-deterministic by default
    False -> trNDFunc f >+= \ fn -> returnM [fn]
    True  ->
      getNDClass     qn >+= \ndCl ->
      getFuncHOClass qn >+= \hoCl ->
      liftIO (showAnalysis opts (snd qn ++ " is " ++ show (ndCl, hoCl))) >+
      case ndCl of
        ND -> trNDFunc f >+= \ fn -> returnM [fn]
        D  -> case hoCl of
          -- create both deterministic and non-deterministic function
          FuncHO                  -> trDetFunc f >+= \ fd ->
                                     trNDFunc  f >+= \ fn ->
                                     returnM [fd, fn]
          FuncHORes _             -> trDetFunc f >+= \ fd -> returnM [fd]
          FuncFO | isGlobalDecl f -> trGlobalDecl f
                 | otherwise      -> trDetFunc f >+= \ fn -> returnM [fn]

--- Check if a function representing a global variable
--- is first-order and determinismic.
checkGlobal :: FuncDecl -> M ()
checkGlobal f@(Func qn _ _ _ _)
  | isGlobalDecl f =
      getNDClass     qn >+= \ndCl ->
      getFuncHOClass qn >+= \hoCl ->
      case (ndCl, hoCl) of
        (ND, _     ) -> failM $ "Non-determinismic initial value for global `"
                            ++ show (unRenameQName qn) ++ "'"
        (D , FuncFO) -> returnM ()
        (D , _     ) -> failM $ "Higher-order type for global `"
                            ++ show (unRenameQName qn) ++ "'"
  | otherwise      = returnM ()

--- Compute if the function declaration is intended to represent
--- a global variable, i.e., has the form `fun = global val Temporary`.
isGlobalDecl :: FuncDecl -> Bool
isGlobalDecl (Func _ a _ _ r) = case r of
  (Rule [] e) -> a == 0 && isGlobalCall e
  _           -> False

isGlobalCall :: Expr -> Bool
isGlobalCall e = case e of
  Comb FuncCall fname [_, c] -> fname == globalGlobal
                             && c     == Comb ConsCall globalTemporary []
  _                          -> False

globalGlobal :: QName
globalGlobal = renameQName ("Global", "global")

globalTemporary :: QName
globalTemporary = renameQName ("Global", "Temporary")

--- Translate a global declaration of the form `fun = global val Temporary`.
--- This will be translated into
---
---     d_C_fun _ _  = global_C_fun
---     global_C_fun = d_C_global (let x3500 = emptyCs
---                                    x3250 = initCover
---                               in  (tr val))
---                               C_Temporary emptyCs
---
--- to make it a constant.
trGlobalDecl :: FuncDecl -> M [AH.FuncDecl]
trGlobalDecl (Func qn a v t r) = case r of
  (Rule _ (Comb _ _ [e, _]))  | a == 0  ->
    trCompleteExpr e       >+= \e'      ->
    renameFun qn           >+= \qn'     ->
    renameFun globalGlobal >+= \global' ->
    trDetType 0 t          >+= \t'      ->
    returnM $
      [ AH.Func "" qn' 2 (cvVisibility v) (toTypeSig t')
        (AH.simpleRule (map AH.PVar [coverName, constStoreName])
                    (AH.Symbol $ mkGlobalName qn))
      , AH.Func "" (mkGlobalName qn) 0 AH.Private
        (toTypeSig $ trHOTypeExpr AH.FuncType t)
        (AH.simpleRule [] (AH.applyF global'
              [ AH.Let (map (uncurry AH.declVar)
                            [ (constStoreName, emptyCs  )
                            , (coverName     , initCover)
                            ])
                       e'
              , AH.Symbol globalTemporary
              , initCover
              , emptyCs
              ]))
      ]
  _ -> failM "TransFunctions.trGlobalDecl: no global declaration"

cvVisibility :: Visibility -> AH.Visibility
cvVisibility Public  = AH.Public
cvVisibility Private = AH.Private

--- Translation into a deterministic function.
trDetFunc :: FuncDecl -> M AH.FuncDecl
trDetFunc (Func qn a v t r) = doInDetMode True $
  renameFun qn      >+= \qn' ->
  trDetType  a t >+= \t'  ->
  trRule qn' a r >+= \r'  ->
  returnM (AH.Func "" qn' (a + 1) (cvVisibility v) (toTypeSig t') r')

--- Translation into a non-deterministic function.
trNDFunc :: FuncDecl -> M AH.FuncDecl
trNDFunc (Func qn a v t r) = doInDetMode False $
  renameFun qn        >+= \qn' ->
  trNonDetType a t >+= \t'  ->
  trRule qn'   a r >+= \r'  ->
  returnM (AH.Func "" qn' (a + 2) (cvVisibility v) (toTypeSig t') r')

--- Rename a function w.r.t. its non-determinism
--- and higher-order classification.
renameFun :: QName -> M QName
renameFun qn@(q, n) =
  isDetMode         >+= \dm   ->
  getNDClass qn     >+= \ndCl ->
  getFuncHOClass qn >+= \hoCl ->
  returnM (q, funcPrefix dm ndCl hoCl ++ n)

--- Rename a constructor w.r.t. its higher-order classification.
renameCons :: QName -> M QName
renameCons qn@(q, n) =
  isDetMode         >+= \dm   ->
  getConsHOClass qn >+= \hoCl ->
  returnM (q, consPrefix dm hoCl ++ n)

-- -----------------------------------------------------------------------------
-- Translation of Types
-- -----------------------------------------------------------------------------

toTypeSig :: AH.TypeExpr -> AH.TypeSig
toTypeSig ty | null tyVars = AH.FType ty
             | otherwise   = AH.CType ctxt ty
  where
  tyVars = AH.tyVarsOf ty
  ctxt   = map (\tv -> AH.Context (curryPrelude, "Curry") [tv]) tyVars

trDetType :: Int -> TypeExpr -> M AH.TypeExpr
trDetType = trTypeExpr detFuncType
  (\t -> foldr1 AH.FuncType [coverType, storeType, t])

-- translate a type expression by replacing (->) with Funcs and inserting
-- additional IDSupply, ConstStore and EncapsulationDepth types
trNonDetType :: Int -> TypeExpr -> M AH.TypeExpr
trNonDetType = trTypeExpr nondetFuncType
  (AH.FuncType supplyType . AH.FuncType coverType . AH.FuncType storeType)

trExprType :: TypeExpr -> M AH.TypeExpr
trExprType ty =
  isDetMode >+= \dm ->
  returnM $ trHOTypeExpr (if dm then detFuncType else nondetFuncType) ty

trTypeExpr :: (AH.TypeExpr -> AH.TypeExpr -> AH.TypeExpr)
           -> (AH.TypeExpr -> AH.TypeExpr)
           -> Int -> TypeExpr -> M AH.TypeExpr
trTypeExpr combFunc addArgs n t
    -- all arguments are applied
  | n == 0 = returnM $ addArgs (trHOTypeExpr combFunc t)
  | n >  0 = case t of
              (FuncType t1 t2) ->
                trTypeExpr combFunc addArgs (n-1) t2 >+= \t2' ->
                returnM $ AH.FuncType (trHOTypeExpr combFunc t1) t2'
              _                -> failM $ "trTypeExpr: " ++ show (n, t)
  | n <  0 = failM $ "trTypeExpr: " ++ show (n, t)

--- Transform a higher order type expressions using a function
--- to combine the two type expressions of a functional type.
trHOTypeExpr :: (AH.TypeExpr -> AH.TypeExpr -> AH.TypeExpr)
             -> TypeExpr -> AH.TypeExpr
trHOTypeExpr _ (TVar         i) = AH.TVar (cvTVarIndex i)
trHOTypeExpr f (FuncType t1 t2) = f (trHOTypeExpr f t1) (trHOTypeExpr f t2)
trHOTypeExpr f (TCons    qn ts) = AH.TCons qn (map (trHOTypeExpr f) ts)

cvTVarIndex :: TVarIndex -> AH.TVarIName
cvTVarIndex i = (i, 't' : show i)

supplyType :: AH.TypeExpr
supplyType  = AH.TCons (basics, "IDSupply") []

coverType :: AH.TypeExpr
coverType = AH.TCons (basics, "Cover") []

storeType :: AH.TypeExpr
storeType = AH.TCons (basics, "ConstStore") []

detFuncType :: AH.TypeExpr -> AH.TypeExpr -> AH.TypeExpr
detFuncType t1 t2 = foldr1 AH.FuncType [t1, coverType, storeType, t2]

nondetFuncType :: AH.TypeExpr -> AH.TypeExpr -> AH.TypeExpr
nondetFuncType t1 t2 = AH.TCons (basics, "Func") [t1, t2]

-- -----------------------------------------------------------------------------
-- Translation of Rules and Expressions
-- -----------------------------------------------------------------------------

--- Translate a single rule of a function.
--- Adds a supply argument to non-deterministic versions and a constStore
--- and coder depth argument to all functions.
trRule :: QName -> Int -> Rule -> M AH.Rules
trRule qn _ (Rule vs e) =
  isDetMode         >+= \dm ->
  isTraceFailure    >+= \fc ->
  trBody qn vs e    >+= \e' ->
  let vs' = map cvVarIndex vs
            ++ [topSupplyName | not dm] ++ [coverName, constStoreName]
      e'' = if fc then failCheck qn (map cvVar vs) e' else e'
  in  returnM $ AH.simpleRule (map AH.PVar vs') e''
trRule qn a (External _) =
  isDetMode      >+= \dm ->
  isTraceFailure >+= \fc ->
  let vs  = [1 .. a]
      vs' = map cvVarIndex vs
            ++ [topSupplyName | not dm] ++ [coverName, constStoreName]
      e   = funcCall (externalFunc qn) (map AH.Var vs')
      e'  = if fc then failCheck qn (map cvVar vs) e else e
  in  returnM $ AH.simpleRule (map AH.PVar vs') e'

--- Translate a function body.
trBody :: QName -> [Int] -> Expr -> M AH.Expr
trBody qn vs e = case e of
  Case _ (Var i) bs ->
    getMatchedType (head bs)  >+= \ty  ->
    mapM trBranch bs          >+= \bs' ->
    let lbs = litBranches bs' in
    consBranches qn vs i ty   >+= \cbs ->
    returnM $ AH.Case (cvVar i) (bs' ++ lbs ++ cbs)
  _ -> trCompleteExpr e

getMatchedType :: BranchExpr -> M QName
getMatchedType (Branch (Pattern p _) _) = getType p
getMatchedType (Branch (LPattern  l) _) = returnM $ case l of
  Intc _   -> curryInt
  Floatc _ -> curryFloat
  Charc _  -> curryChar

trBranch :: BranchExpr -> M AH.BranchExpr
trBranch (Branch p e) = liftM2 AH.Branch (trPattern p) (trCompleteExpr e)

trPattern :: Pattern -> M AH.Pattern
trPattern (Pattern qn vs) =
  renameCons qn >+= \qn' ->
  returnM $ AH.PComb qn' $ map (AH.PVar . cvVarIndex) vs
trPattern (LPattern    l) = returnM $ AH.PLit $ cvLit l

--- During unification the internal representation of `Int` and `Char` values
--- is changed to an algebraic data type. Hence, we have to extend the pattern
--- matching on literals to also cope with this additional representation.
--- Therefore, this function collects all literal-matching branches and
--- creates a call to a dedicated matching function. Thus, the branches
---
---    <lit_1> -> <expr_1>
---    ...
---    <lit_n> -> <expr_n>
---
--- are extended with an additional branch
---
---    CurryInt l ->
---     matchInteger [(<lit_1>, <expr_1>), ..., (<lit_n>, <expr_n>)] l cd cs
---
--- which performs mathcing on the additional representation.
litBranches :: [AH.BranchExpr] -> [AH.BranchExpr]
litBranches bs = case branchPairs of
    (AH.Intc  _, _) : _ -> [mkBranch (renameQName (prelude, "CurryInt"))
                                     (basics, "matchInteger")]
    (AH.Charc _, _) : _ -> [mkBranch (curryPrelude, "CurryChar")
                                     (basics, "matchChar")]
    _                   -> []
  where
    branchPairs = [ (l, e) | AH.Branch (AH.PLit l) e <- bs ]
    mkBranch cons match = AH.Branch (AH.PComb cons [AH.PVar litVar])
                        $ funcCall match
                          [ AH.list2ac $ map pair2ac
                                       $ map (first AH.Lit) branchPairs
                          , AH.Var litVar, coverVar, constStoreVar
                          ]
    litVar = (5000, "l")

--- Generate additional case branches for added non-deterministic constructors.
---
--- For example, consider the function `not`:
---
---    not x = case x of
---      True  -> False
---      False -> True
---
--- This function will be translated to:
---
---    not x1 cd cs = case x1 of
---      C_True                  -> C_False
---      C_False                 -> C_True
---      Choice_C_Bool  d i l r  -> narrow d i (not l cd cs) (not r cd cs)
---      Choices_C_Bool d i xs   -> narrows cs d i (\z -> not z cd cs) xs
---      Guard_C_Bool   d   c x  -> guardCons d c (not x cd cs)
---      Fail_C_Bool    d   info -> failCons d info
---      _                       -> consFail (showCons x1)
---
--- where the last 5 branches are generated using this function.
---
--- @param qn'      : Qualified name of the function currently processed,
---                   used for recursive calls.
--- @param vs       : Function arguments
--- @param i        : Variable matched by case
--- @param pConsName: Name of the type of the matched variable,
---                   used to compute the names of the additional constructors.
consBranches :: QName -> [Int] -> Int -> QName -> M [AH.BranchExpr]
consBranches qn' vs v typeName =
  isDetMode      >+= \dm ->
  isTraceFailure >+= \fc ->
  let (vs1, _ : vs2) = break (== v) vs
      vars1     = map cvVar vs1
      vars2     = map cvVar vs2
      mbSuppVar = [topSupplyVar | not dm]

      recCall x = funcCall qn' $ concat
        [ vars1, x : vars2 , mbSuppVar, [coverVar, constStoreVar] ]

      -- pattern matching on guards will combine the new constraints
      -- with the given constraint store
      guardCall = strictCall
        (funcCall qn' $ concat [vars1, AH.Var e : vars2, mbSuppVar, [coverVar]])
        (funcCall addCs [AH.Var c, constStoreVar])

      lambdaExpr = AH.Lambda [AH.PVar z] $ AH.applyF qn' $ concat
        [vars1, (AH.Var z) : vars2, mbSuppVar, [coverVar, constStoreVar]]
  in returnM $
    [ AH.Branch (AH.PComb (mkChoiceName  typeName) (map AH.PVar [d, i, l, r]))
      (narrow [AH.Var d, AH.Var i, recCall (AH.Var l), recCall (AH.Var r)])
    , AH.Branch (AH.PComb (mkChoicesName typeName) (map AH.PVar [d, i, xs]))
      (narrows [constStoreVar, AH.Var d, AH.Var i, lambdaExpr, AH.Var xs])
    , AH.Branch (AH.PComb (mkGuardName   typeName) (map AH.PVar [d, c, e]))
      (liftGuard [AH.Var d, AH.Var c, guardCall])
    , AH.Branch (AH.PComb (mkFailName    typeName) (map AH.PVar [d, info]))
      (if fc then liftFail  [AH.Var d, AH.Var info]
             else traceFail (AH.Var d) qn' (map cvVar vs) (AH.Var info))
    , AH.Branch (AH.PVar us) (consFail qn' (cvVar v))
    ]
  where
  [d, i, l, r, xs, z, c, e, info, us]
    = newVars ["d", "i", "l", "r", "xs", "z", "c", "e", "info", "_"]

--- Translation of an expression where all newly introduced supply
--- variables are bound by nested let expressions.
trCompleteExpr :: Expr -> M AH.Expr
trCompleteExpr e =
  getNextID   >+= \i       -> -- save current variable id
  trExpr e    >+= \(g, e') ->
  setNextID i >+              -- and reset the variable id
  case g of
    []  -> returnM e'
    [v] -> letIdVar [(supplyName v, topSupplyVar)] e'
    _   -> failM "TransFunctions.trCompleteExpr"

--- Transform an expression and compute a list of new supply variables
--- to be bound.
trExpr :: Expr -> M ([VarIndex], AH.Expr)
trExpr (Var               i) = returnM ([], cvVar     i)
trExpr (Lit               l) = returnM ([], cvLitExpr l)
trExpr (Comb ConsCall qn es) =
  renameCons     qn               >+= \qn'      ->
  mapM trExpr es >+= unzipArgs >+= \(g, es') ->
  genIds g (AH.applyF qn' es')

-- fully applied functions
trExpr (Comb FuncCall qn es) =
  getCompOption (\opts -> opts :> optOptimization > OptimNone) >+= \opt ->
  getNDClass qn     >+= \ndCl ->
  getFuncHOClass qn >+= \hoCl ->
  isDetMode         >+= \dm   ->
  renameFun qn      >+= \qn'  ->
  mapM trExpr es >+= unzipArgs >+= \(g, es') ->
  if ndCl == ND || not opt || (hoCl == FuncHO && not dm)
   -- for non-deterministic functions and higher-order functions
   -- translated in non-determinism mode we just call the function
   -- with the additional arguments (idsupply, capsule nesting depth
   -- and the constraint store)
    then takeNextID >+= \i -> genIds (i:g)
          (AH.applyF qn' (es' ++ [supplyVar i, coverVar, constStoreVar]))
    -- for deterministic functions with higher-order result
    -- in non-determinism mode we need to wrap the result
    -- in order to accept the additional arguments
    else genIds g $ case hoCl of
      FuncHORes i | not dm -> wrapDHO i $
                              AH.applyF qn' (es' ++ [coverVar, constStoreVar])
      _                    -> AH.applyF qn' (es' ++ [coverVar, constStoreVar])

-- partially applied functions
trExpr (Comb (FuncPartCall i) qn es) =
  getCompOption (\opts -> opts :> optOptimization > OptimNone) >+= \opt ->
  getNDClass qn     >+= \ndCl ->
  getFuncHOClass qn >+= \hoCl ->
  isDetMode         >+= \dm   ->
  renameFun qn      >+= \qn'  ->
  mapM trExpr es >+= unzipArgs  >+= \(g, es') ->
  genIds g (wrapPartCall False dm opt ndCl hoCl i (AH.applyF qn' es'))

-- calls to partially applied constructors are treated like calls to partially
-- applied deterministic first order functions.
trExpr (Comb (ConsPartCall i) qn es) =
  isDetMode     >+= \dm  ->
  renameCons qn >+= \qn' ->
  mapM trExpr es >+= unzipArgs >+= \(g, es') ->
  genIds g (wrapPartCall True  dm True D FuncFO i (AH.applyF qn' es'))

trExpr (Let ds e) =
  let (vs, es) = unzip ds in
  mapM trExpr es >+= unzipArgs >+= \(g, es') ->
  trExpr e       >+=               \(ge, e') ->
  genIds (g ++ ge) (AH.Let (zipWith AH.declVar (map cvVarIndex vs) es') e')

trExpr (Or e1 e2) =
  trExpr e1  >+= \(vs1, e1') ->
  trExpr e2  >+= \(vs2, e2') ->
  takeNextID >+= \i          ->
  genIds (i : vs1 ++ vs2)
    (choice [e1', e2', supplyVar i, coverVar, constStoreVar])

trExpr (Free vs e) =
  takeNextIDs (length vs) >+= \is   ->
  trExpr e             >+= \(g, e') ->
  genIds (is ++ g) (AH.Let (zipWith mkFree vs is) e')
  where mkFree v i = AH.declVar (cvVarIndex v) (generate $ supplyVar i)

-- This case should not occur because:
--   * Nested case expressions have been lifted using LiftCase
--   * The outer case expression has been handled by trBody
trExpr (Case _ _ _) = failM "TransFunctions.trExpr: case expression"

trExpr (Typed e ty) =
  trExpr e      >+= \(g, e') ->
  trExprType ty >+= \ty'     ->
  genIds g (AH.Typed e' ty')

unzipArgs :: [([VarIndex], AH.Expr)] -> M ([VarIndex], [AH.Expr])
unzipArgs ises = returnM (concat is, es) where (is, es) = unzip ises

genIds :: [VarIndex] -> AH.Expr -> M ([VarIndex], AH.Expr)
genIds []       e = returnM ([], e)
genIds ns@(_:_) e =
  -- get next free variable id
  getNextID >+= \i ->
  -- create splitting of supply variables
  let (vroot, v', vs)    = splitSupply i ns
      supply (v, v1, v2) = [ (supplyName v1, leftSupply  [supplyVar v])
                           , (supplyName v2, rightSupply [supplyVar v])
                           ]
  in
  letIdVar (concatMap supply vs) e >+= \e' ->
  setNextID v' >+ returnM ([vroot], e')

--- Split up an identifier supply to saturate
--- the given list of requested supplies.
---  @param s : initial free supply variable
---  @param xs: non-empty list of fresh variables to be bound to an id supply
---
--- @Result (x, s', bindings)
---    - x is the root-level variable to be bound
---    - s' is the next free variable
---    - bindings: list of triples (s, x, y)
---               (let x = leftSupply s, y = rightSupply s)
splitSupply :: VarIndex -> [VarIndex]
            -> (VarIndex, VarIndex, [(VarIndex, VarIndex, VarIndex)])
splitSupply _ []         = error "splitSupply with empty list"
splitSupply s [x]        = (x, s, [])
splitSupply s xs@(_:_:_) = (s, nextr, (s, sl, sr) : spsl ++ spsr)
  where
  (sl, nextl, spsl) = splitSupply (s + 1) ys
  (sr, nextr, spsr) = splitSupply nextl zs
  (ys, zs)          = splitAt (div (length xs) 2) xs

cvVar :: VarIndex -> AH.Expr
cvVar = AH.Var . cvVarIndex

cvVarIndex :: VarIndex -> AH.VarIName
cvVarIndex i = (i, 'x' : show i)

cvLit :: Literal -> AH.Literal
cvLit (Intc   i) = AH.Intc   i
cvLit (Floatc f) = AH.Floatc f
cvLit (Charc  c) = AH.Charc  c

cvLitExpr :: Literal -> AH.Expr
cvLitExpr (Intc   i) = funcCall curryInt   [constant (prelude, show i ++ "#")]
cvLitExpr (Floatc f) = funcCall curryFloat [constant (prelude, show f ++ "#")]
cvLitExpr (Charc  c) = funcCall curryChar  [constant (prelude, show c ++ "#")]

topSupplyVar :: AH.Expr
topSupplyVar = AH.Var topSupplyName

topSupplyName ::AH.VarIName
topSupplyName = (3000, "s")

supplyVar :: VarIndex -> AH.Expr
supplyVar = AH.Var . supplyName

supplyName :: VarIndex -> AH.VarIName
supplyName i = (i, 's' : show i)

coverVar :: AH.Expr
coverVar = AH.Var coverName

coverName :: AH.VarIName
coverName = (3250, "cd")

constStoreVar :: AH.Expr
constStoreVar = AH.Var constStoreName

constStoreName :: AH.VarIName
constStoreName = (3500, "cs")

-- ---------------------------------------------------------------------------
-- Wrapping
-- ---------------------------------------------------------------------------

-- Wrapping surrounds a function call with additional constructs to make
-- the call fit into the compilation scheme.
-- This is necessary for the following cases:
--
--  1. A partially applied constructor or function call.
--     In this case, the partial call is extended to accept the additional
--     arguments for the IDSupply (only in nondeterministic context),
--     the cover depth and the constraint store *after each regular argument*.
--     This is required by the primitive apply function.
--  2. A call to a constructor or a deterministic function called from a
--     non-deterministic context. In this case, the deterministic function
--     is expected to accept an additional IDSupply (which is then ignored).

--- Wrapping the higher order result function of a deterministic function
--- called from nondeterministic context.
--- @param arity - arity of the result function
--- @param expr  - function call to wrap
wrapDHO :: Int -> AH.Expr -> AH.Expr
wrapDHO arity expr = newWrap True arity expr

--- Wrapping the result of partial applications
--- in order to accept the above mentioned aditional arguments.
--- @param dm    - True iff invoked in deterministic context
--- @param opt   - True iff optimization for deterministic functions
---                   should be applied
--- @param nd    - nondeterministic class of called function or constructor
---                  (constructors are always deterministic)
--- @param ho    - higher-order class of called function or constructor
--- @param arity - arity of partial call, i.e., number of arguments missing
---                to form a fully applied call
--- @param e     - expression (partial call) to transform
wrapPartCall :: Bool -> Bool -> Bool -> NDClass -> FuncHOClass -> Int
             -> AH.Expr -> AH.Expr
wrapPartCall cons dm opt nd ho arity e
  | dm        = wrapCs cons arity e
  | nd == ND  = newWrap False arity              (wrapCs cons arity e)
  | otherwise = newWrap useDX (arity + resArity) (wrapCs cons arity e)
  where
  useDX = opt && nd == D && not isHO
  isHO = case ho of
            FuncHO -> True
            _      -> False
  resArity = case ho of
                FuncHORes i -> i
                _           -> 0

--- Add  `ConstraintStore` arguments after every argument of a partially
--- called function.
wrapCs :: Bool -> Int -> AH.Expr -> AH.Expr
wrapCs cons n e
  | n == 1 = if cons then acceptCs [funId, e] else e
  | n >  1 = acceptCs [ mkWraps (n - 1) (if cons then acceptCs [funId]
                                                 else funId)
                      , e
                      ]
 where
  acceptCs = AH.applyF (basics, "acceptCs")
  mkWraps m expr | m < 2     = expr
                 | otherwise = mkWraps (m - 1) (acceptCs [expr])

-- TODO: simplify
newWrap :: Bool -> Int -> AH.Expr -> AH.Expr
newWrap useDX n e
  | n == 0 = e
  | n == 1 = innermostWrapper [funId, e]
  | n == 2 = wrapDX [innermostWrapper [funId], e]
  | n == 3 = wrapDX [wrapDX [innermostWrapper [funId]], e]
  | n == 4 = wrapDX [wrapDX [wrapDX [innermostWrapper [funId]]], e]
  | n >  4 = wrapDX [wraps (n - 1) (innermostWrapper [funId]), e]
  where
  wraps m expr = if m <= 1 then expr else wrapDX [wraps (m - 1) expr]
  innermostWrapper = if useDX then wrapDX else wrapNX

wrapDX = AH.applyF (basics, "wrapDX")
wrapNX = AH.applyF (basics, "wrapNX")
funId  = AH.applyF (prelude, "id") []

-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

-- Strict or lazy computation of supplies
letIdVar :: [(AH.VarIName, AH.Expr)] -> AH.Expr -> M AH.Expr
letIdVar ds e =
  strictSupply >+= \strict ->
  returnM $ AH.Let (map (uncurry AH.declVar) ds)
          $ if strict then foldr seqCall e (map (AH.Var . fst) ds) else e

curryInt :: QName
curryInt = renameQName (prelude, "Int")

curryFloat :: QName
curryFloat = renameQName (prelude, "Float")

curryChar :: QName
curryChar = renameQName (prelude, "Char")

addCs :: QName
addCs = (basics, "addCs")

-- expressions

pair2ac :: (AH.Expr, AH.Expr) -> AH.Expr
pair2ac (e1, e2) = consCall (prelude, "(,)") [e1, e2]

seqCall :: AH.Expr -> AH.Expr -> AH.Expr
seqCall e1 e2 = funcCall (prelude, "seq") [e1, e2]

strictCall :: AH.Expr -> AH.Expr -> AH.Expr
strictCall f e = funcCall (prelude, "$!") [f, e]

consCall = AH.applyF
funcCall = AH.applyF

constant qn = AH.applyF qn []

failCheck :: QName -> [AH.Expr] -> AH.Expr -> AH.Expr
failCheck qn vs e
  | isCaseAuxFuncName (snd $ unRenamePrefixedFunc qn) = e
  | otherwise                   = funcCall (basics, "failCheck")
    [ showQName $ unRenameQName qn
    , AH.list2ac (map (\v -> funcCall (prelude, "show") [v]) vs)
    , e
    ]

traceFail :: AH.Expr -> QName -> [AH.Expr] -> AH.Expr -> AH.Expr
traceFail cd qn args fail = liftFail
  [ cd
  , funcCall (basics, "traceFail")
    [ showQName qn
    , AH.list2ac (map (\a -> funcCall (prelude, "show") [a]) args)
    , fail
    ]
  ]

consFail :: QName -> AH.Expr -> AH.Expr
consFail qn arg = liftFail
  [ coverVar
  , funcCall (basics, "consFail")
    [ showQName $ unRenameQName qn
    , funcCall (basics, "showCons") [arg]
    ]
  ]

showQName :: QName -> AH.Expr
showQName qn = AH.string2ac (q ++ '.' : n)
  where (q, n) = unRenamePrefixedFunc qn

emptyCs     = funcCall (basics, "emptyCs") []
initCover   = funcCall (basics, "initCover") []

choice      = funcCall (basics, "choice")
narrow      = funcCall (basics, "narrow")
narrows     = funcCall (basics, "narrows")
liftGuard   = funcCall (basics, "guardCons")
liftFail    = funcCall (basics, "failCons")

leftSupply  = funcCall (basics, "leftSupply")
rightSupply = funcCall (basics, "rightSupply")

generate :: AH.Expr -> AH.Expr
generate s = funcCall (basics, "generate") [s, coverVar]

newVars :: [String] -> [AH.VarIName]
newVars = zip [1 ..]
