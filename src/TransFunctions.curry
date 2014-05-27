--- --------------------------------------------------------------------------
--- This module forms the main part of the compiler as it defines
--- the translation of function declarations from FlatCurry into Haskell.
---
--- @author Bjoern Peemoeller, Fabian Skrlac
--- @version May 2014
--- --------------------------------------------------------------------------
{-# LANGUAGE Records #-}
module TransFunctions
  ( State (..), defaultState, AnalysisResult, transProg, runIOErrorState
  ) where

import FiniteMap ( FM, addToFM, emptyFM, mapFM, filterFM, fmToList, listToFM
  , lookupFM, plusFM, delListFromFM)
import List (find, intersperse, isPrefixOf)
import Maybe

import qualified AbstractHaskell as AH
import AbstractHaskellPrinter
import Analysis
import CompilerOpts
import FlatCurry
import FlatCurryGoodies
import LiftCase
import Message
import Names
import Splits

-- The type map is used to lookup the type name for a given constructor
-- name to be able to add missing pattern matching alternatives like
-- Choice_<TypeName> etc.
-- This could also be done by inspecting the type signature of the respective
-- function, but it may not be accurate for various reasons.

type AnalysisResult = (TypeMap, NDResult, TypeHOResult, ConsHOResult, FuncHOResult)

-- ---------------------------------------------------------------------------
-- IO state monad, like StateT IO
-- ---------------------------------------------------------------------------

data IOErrorState s a = M (s -> IO (Either String (a, s)))

runIOErrorState :: IOErrorState s a -> s -> IO (Either String (a, s))
runIOErrorState (M x) = x

returnM :: a -> IOErrorState s a
returnM x = M $ \s -> return (Right (x, s))

(>+=) :: IOErrorState s a -> (a -> IOErrorState s b) -> IOErrorState s b
f >+= g = M $ \s -> do
  eex <- runIOErrorState f s
  case eex of
    Left  err     -> return (Left err)
    Right (x, s') -> runIOErrorState (g x) s'

(>+) :: IOErrorState s a -> IOErrorState s b -> IOErrorState s b
f >+ g = f >+= \_ -> g

failM :: String -> IOErrorState s a
failM err = M $ \_ -> return (Left err)

getState :: IOErrorState s s
getState = M $ \s -> return (Right (s, s))

putState :: s -> IOErrorState s ()
putState s = M $ \ _ -> return (Right ((), s))

updState :: (s -> s) -> IOErrorState s ()
updState f = getState >+= \s -> putState (f s)

liftIO :: IO a -> IOErrorState s a
liftIO act = M $ \s -> do
  a <- act
  return (Right (a, s))

mapM :: (a -> IOErrorState s b) -> [a] -> IOErrorState s [b]
mapM _ [] = returnM []
mapM f (m:ms) = f m       >+= \m'  ->
                mapM f ms >+= \ms' ->
                returnM (m':ms')

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

-- map a constructor name to the name of its defining type
type TypeMap = FM QName QName

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
  { typeMap      := listToFM (<) primTypes
  , ndResult     := initNDResult
  , hoResultType := initTypeHOResult
  , hoResultCons := initHOResult
  , hoResultFunc := initHOResult
  , nextID       := idVar
  , detMode      := False
  , compOptions  := defaultOptions
  }

type M a = IOErrorState State a

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
-- Program transformation
-- ---------------------------------------------------------------------------

transProg :: Prog -> M (Prog, AnalysisResult)
transProg p@(Prog m is ts fs _) =
  getState >+= \st ->
  let modNDRes     = analyseND     p (st :> ndResult)
      modHOResType = analyseHOType p (st :> hoResultType)
      modHOResCons = analyseHOCons p
      modHOResFunc = analyseHOFunc p (st :> hoResultType `plusFM` modHOResType)
      modTypeMap   = getConsMap    ts
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
  mapM transFunc fs >+= \fss ->
  returnM $ (Prog m is [] (concat fss) [], anaResult)

--- Register the types names of constructors to be able to retrieve
--- the types for constructors used in pattern matching.
--- May be needless now because the case lifting now also creates correct types.
getConsMap :: [TypeDecl] -> TypeMap
getConsMap ts = listToFM (<)
              $ concatMap (\ (Type qn _ _ cs) -> map (\c -> (consName c, qn)) cs)
              $ filter (not . isTypeSyn) ts

-- ---------------------------------------------------------------------------
-- Translation of Curry functions
-- ---------------------------------------------------------------------------

transFunc :: FuncDecl -> M [FuncDecl]
transFunc f@(Func qn _ _ _ _) =
  checkGlobal f  >+
  getCompOptions >+= \opts ->
  case opts :> optOptimization > OptimNone of
    -- translate all functions as non-deterministic by default
    False -> transNDFunc f >+= \ fn -> returnM [fn]
    True  ->
      getNDClass     qn >+= \ndCl ->
      getFuncHOClass qn >+= \hoCl ->
      liftIO (showAnalysis opts (snd qn ++ " is " ++ show (ndCl, hoCl))) >+
      case ndCl of
        ND -> transNDFunc f >+= \ fn -> returnM [fn]
        D  -> case hoCl of
          -- create both deterministic and non-deterministic function
          FuncHO                  -> transDetFunc f >+= \ fd ->
                                     transNDFunc  f >+= \ fn ->
                                     returnM [fd, fn]
          FuncHORes _             -> transDetFunc f >+= \ fd -> returnM [fd]
          FuncFO | isGlobalDecl f -> transGlobalDecl f
                 | otherwise      -> transDetFunc f >+= \ fn -> returnM [fn]

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
--- This will be translated into
---
---     d_C_fun _ _  = global_C_fun
---     global_C_fun = d_C_global (let x3500 = emptyCs
---                                    x3250 = initCover
---                               in  (tr val))
---                               C_Temporary emptyCs
---
--- to make it a constant.
isGlobalDecl :: FuncDecl -> Bool
isGlobalDecl (Func _ a _ _ r) = case r of
  (Rule [] e) -> a == 0 && isGlobalCall e
  _           -> False

isGlobalCall :: Expr -> Bool
isGlobalCall e = case e of
  Comb FuncCall fname [_, c] -> fname == renameQName ("Global", "global")
                             && c     == globalTemporary
  _                          -> False

globalTemporary :: Expr
globalTemporary = Comb ConsCall (renameQName ("Global", "Temporary")) []

transGlobalDecl :: FuncDecl -> M [FuncDecl]
transGlobalDecl (Func qn a vis t r) = case r of
  (Rule _ (Comb _ fname [val, _])) | a == 0 ->
    transCompleteExpr val >+= \trVal ->
    renameFun qn          >+= \qn' ->
    renameFun fname       >+= \newfname ->
    transTypeExpr 0 t     >+= \t'    ->
    returnM $
      [ Func qn' 2 vis t' (Rule [0, 1] (Comb FuncCall (mkGlobalName qn) []))
      , Func (mkGlobalName qn) 0 Private t
          (Rule [] (Comb FuncCall newfname
                [ Let [ (constStoreVarIdx, emptyCs  )
                      , (nestingIdx      , initCover)
                      ]
                    trVal
                , globalTemporary
                , initCover
                , emptyCs
                ]))
      ]
  _ -> failM "TransFunctions.transGlobalDecl: no global declaration"

--- Translation into a deterministic function.
transDetFunc :: FuncDecl -> M FuncDecl
transDetFunc (Func qn a v t r) = doInDetMode True $
  renameFun qn      >+= \qn' ->
  transRule qn' a r >+= \r'  ->
  transTypeExpr a t >+= \t'  ->
  returnM (Func qn' (a + 1) v t' r')

--- Translation into a non-deterministic function.
transNDFunc :: FuncDecl -> M FuncDecl
transNDFunc (Func qn a v t r) = doInDetMode False $
  renameFun qn        >+= \qn' ->
  transRule qn'   a r >+= \r'  ->
  transNDTypeExpr a t >+= \t'  ->
  returnM (Func qn' (a + 2) v t' r')

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

-- translate a type expressen by inserting an additional ConstStore and
-- EncapsulationDepth type

transExprType :: Bool -> TypeExpr -> TypeExpr
transExprType deterministic
  | deterministic = transHOTypeExprWith (\t1 t2 -> foldr1 FuncType [t1, coverType, storeType, t2])
  | otherwise     = transHOTypeExprWith funcType

transTypeExpr :: Int -> TypeExpr -> M TypeExpr
transTypeExpr = transTypeExprWith
  (\t1 t2 -> foldr1 FuncType [t1, coverType, storeType, t2])
  (\t     -> foldr1 FuncType [coverType, storeType, t])

-- translate a type expression by replacing (->) with Funcs and inserting
-- additional IDSupply, ConstStore and EncapsulationDepth types
transNDTypeExpr :: Int -> TypeExpr -> M TypeExpr
transNDTypeExpr  = transTypeExprWith funcType
                   (FuncType supplyType . FuncType coverType . FuncType storeType)

transTypeExprWith :: (TypeExpr -> TypeExpr -> TypeExpr)
                  -> (TypeExpr -> TypeExpr)
                  -> Int -> TypeExpr -> M TypeExpr
transTypeExprWith combFunc addArgs n t
    -- all arguments are applied
  | n == 0 = returnM $ addArgs (transHOTypeExprWith combFunc t)
  | n >  0 = case t of
              (FuncType t1 t2) ->
                transTypeExprWith combFunc addArgs (n-1) t2 >+= \t2' ->
                returnM $ FuncType (transHOTypeExprWith combFunc t1) t2'
              _                -> failM $ "transTypeExprWith: " ++ show (n, t)
  | n <  0 = failM $ "transTypeExprWith: " ++ show (n, t)

-- transforms higher order type expressions using a function that combines
-- the two type-expressions of FuncTypes.
transHOTypeExprWith :: (TypeExpr -> TypeExpr -> TypeExpr) -> TypeExpr -> TypeExpr
transHOTypeExprWith _    t@(TVar       _) = t
transHOTypeExprWith comb (FuncType t1 t2) = comb (transHOTypeExprWith comb t1)
                                                 (transHOTypeExprWith comb t2)
transHOTypeExprWith comb (TCons    qn ts) = TCons qn
                                            (map (transHOTypeExprWith comb) ts)

--- Translate a single rule of a function.
--- Adds a supply argument to non-deterministic versions and a constStore
--- and nesting index argument to all functions.
transRule :: QName -> Int -> Rule -> M Rule
transRule qn _ (Rule vs e) =
  isDetMode         >+= \dm ->
  transBody qn vs e >+= \e' ->
  isTraceFailure    >+= \fc ->
  let vs' = vs ++ [suppVarIdx | not dm] ++ [nestingIdx, constStoreVarIdx]
      e'' = if fc then failCheck qn vs e' else e'
  in  returnM $ Rule vs' e''
transRule qn a (External _) =
  isDetMode      >+= \dm ->
  isTraceFailure >+= \fc ->
  let vs  = [1 .. a]
      vs' = vs ++ [suppVarIdx | not dm] ++ [nestingIdx, constStoreVarIdx]
      e   = funcCall (externalFunc qn) (map Var vs')
      e'  = if fc then failCheck qn vs e else e
  in returnM $ Rule vs' e'

--- Translate a function body.
transBody :: QName -> [Int] -> Expr -> M Expr
transBody qn vs exp = case exp of
  -- case expression with variable
  (Case ct e@(Var i) bs) ->
    -- translate branches
    mapM transBranch bs >+= \bs' ->
    -- create branches for non-deterministic constructors
    let bs'' = addUnifIntCharRule bs bs'
        pConsName = consNameFromPattern $ head bs in
    newBranches qn vs i pConsName >+= \ns ->
    -- TODO: superfluous?
    transExpr e >+= \(_, e') ->
    returnM $ Case ct e' (bs'' ++ ns)
  _ ->transCompleteExpr exp

addUnifIntCharRule :: [BranchExpr] -> [BranchExpr] -> [BranchExpr]
addUnifIntCharRule bs bs' =
  case bs of
   (Branch (LPattern (Intc  _)) _ :_) -> addRule True  bs bs' []
   (Branch (LPattern (Charc _)) _ :_) -> addRule False bs bs' []
   _                                  -> bs'
  where
    addRule isInt bs1 bs2 rules = case (bs1, bs2) of
      (Branch (LPattern l) _ : nextBs, Branch p e : nextBs')
        -> Branch p e : addRule isInt nextBs nextBs' ((Lit l, e) : rules)

      -- TODO: magic number
      _ -> Branch (Pattern (constr isInt) [5000])
                  (funcCall (matchFun isInt)
                    [ list2FCList $ map pair2FCPair $ reverse rules
                    , Var 5000, Var nestingIdx, Var constStoreVarIdx
                    ])
          : bs2
    matchFun True  = (basics, "matchInteger")
    matchFun False = (basics, "matchChar")
    constr   True  = renameQName (prelude, "CurryInt")
    constr   False = (curryPrelude, "CurryChar")

consNameFromPattern :: BranchExpr -> QName
consNameFromPattern (Branch (Pattern p _) _) = p
consNameFromPattern (Branch (LPattern  l) _) = case l of
  Intc _   -> curryInt
  Floatc _ -> curryFloat
  Charc _  -> curryChar

-- translate case branch and return the name of the constructor
transBranch :: BranchExpr -> M BranchExpr
transBranch (Branch p e) =
  transPattern      p >+= \p' ->
  transCompleteExpr e >+= \e' ->
  returnM (Branch p' e')

transPattern :: Pattern -> M Pattern
transPattern (Pattern qn vs) =
  renameCons qn >+= \qn' ->
  returnM (Pattern qn' vs)
transPattern l@(LPattern  _) = returnM l

-- create new case branches for added non-deterministic constructors
-- qn'      : qualified name of the function currently processed
-- vs       : function arguments
-- i        : variable matched by case
-- pConsName: name of an arbitrary constructor of the type of the matched
--            variable
newBranches :: QName -> [Int] -> Int -> QName -> M [BranchExpr]
newBranches qn' vs i pConsName =
  isDetMode      >+= \dm ->
  isTraceFailure >+= \fc ->
  -- lookup type name to create appropriate constructor names
  getType pConsName >+= \ typeName ->
  let Just pos = find (==i) vs
      suppVar = if dm then [] else [suppVarIdx]
      (vs1, _ : vs2) = break (==pos) vs
      call v = funcCall qn' $
                map Var (vs1 ++ v : vs2
                             ++ suppVar
                             ++ [nestingIdx, constStoreVarIdx])
      -- pattern matching on guards will combine the new constraints with the given
      -- constraint store
      guardCall  cVar valVar   = strictCall (funcCall qn' $ map Var (vs1 ++ valVar : vs2 ++ suppVar ++ [nestingIdx]))
                                 (combConstr cVar constStoreVarIdx)
      combConstr cVar constVar = funcCall addCs [Var cVar, Var constVar]
      -- EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL
      -- TODO: This is probably the dirtiest hack in the compiler:
      -- Because FlatCurry does not allow lambda abstractions, we construct
      -- a call to a function like "\f x1 x2 x-42 x3" which is replaced to
      -- the expression (\z -> f x1 x2 z x3) in the module
      -- FlatCurry2AbstractHaskell.
      -- EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL
      lCall = lambdaCall qn'
            $ map Var (vs1 ++ (-42) : vs2 ++ suppVar ++ [nestingIdx,constStoreVarIdx]) in
  returnM $
    [ Branch (Pattern (mkChoiceName typeName) [1000, 1001, 1002, 1003])
             (narrow [Var 1000, Var 1001, call 1002, call 1003])
    , Branch (Pattern (mkChoicesName typeName) [1000, 1001, 1002])
             (narrows [Var constStoreVarIdx, Var 1000, Var 1001, lCall, Var 1002])
    , Branch (Pattern (mkGuardName typeName) [1000, 1001, 1002])
             (liftGuard [Var 1000, Var 1001, guardCall 1001 1002])
    , Branch (Pattern (mkFailName typeName) [1000, 1001])
              (if fc then liftFail [Var 1000, Var 1001]
                     else traceFail (Var 1000) qn' (map Var vs) (Var 1001))
    , Branch (Pattern ("", "_") [])
             (consFail qn' (Var i))
    ] -- TODO Magic numbers?

-- Complete translation of an expression where all newly introduced supply
-- variables are already bound by nested let expressions
transCompleteExpr :: Expr -> M Expr
transCompleteExpr e =
  strictSupply      >+= \s       ->
  getNextID         >+= \i       -> -- save current variable id
  transExpr e       >+= \(g, e') ->
  mkLetIdVar s g e' >+= \e''     ->
  setNextID i       >+              -- and reset it variable id
  returnM e''
    where
    mkLetIdVar strict g e' = case g of
      []  -> returnM e'
      [v] -> returnM (letIdVar strict [(v, Var suppVarIdx)] e')
      _   -> failM "TransFunctions.transCompleteExpr"

-- transform an expression into a list of new supply variables to be bound
-- and the new expression
transExpr :: Expr -> M ([VarIndex], Expr)
-- variables
transExpr e@(Var _)        = returnM ([], e)

-- literals
transExpr (Lit (Intc   i)) = returnM ([], int   i)
transExpr (Lit (Floatc f)) = returnM ([], float f)
transExpr (Lit (Charc  c)) = returnM ([], char  c)

-- constructors
transExpr (Comb ConsCall qn es) =
  renameCons     qn               >+= \qn'      ->
  mapM transExpr es >+= unzipArgs >+= \(g, es') ->
  genIds g (Comb ConsCall qn' es')

-- calls to partially applied constructors are treated like calls to partially
-- applied deterministic first order functions.
transExpr (Comb (ConsPartCall i) qn es) =
  isDetMode     >+= \dm  ->
  renameCons qn >+= \qn' ->
  mapM transExpr es >+= unzipArgs >+= \(g, es') ->
  genIds g (wrapPartCall dm True D FuncFO i (Comb (ConsPartCall i) qn' es'))

-- fully applied functions
transExpr (Comb FuncCall qn es) =
  getCompOption (\opts -> opts :> optOptimization > OptimNone) >+= \opt ->
  getNDClass qn     >+= \ndCl ->
  getFuncHOClass qn >+= \hoCl ->
  isDetMode         >+= \dm   ->
  renameFun qn      >+= \qn'  ->
  mapM transExpr es >+= unzipArgs >+= \(g, es') ->
  if ndCl == ND || not opt || (hoCl == FuncHO && not dm)
   -- for non-deterministic functions and higher-order functions in non-determinism mode
   -- we just call the function with the additional arguments (idsupply, capsule nesting depth
   -- and the constraint store
    then takeNextID >+= \i ->
         genIds (i:g) (Comb FuncCall qn' (es' ++ [Var i, Var nestingIdx, Var constStoreVarIdx]))
    -- for deterministic functions with higher-order result in non-determinism mode
    -- we need to wrap the result in order to accept the additional arguments
    else genIds g $ case hoCl of
      FuncHORes i | not dm -> wrapDHO i (Comb FuncCall qn' (es' ++ [Var nestingIdx, Var constStoreVarIdx]))
      _                    -> (Comb FuncCall qn' (es' ++ [Var nestingIdx, Var constStoreVarIdx]))

-- partially applied functions
transExpr (Comb (FuncPartCall i) qn es) =
  getCompOption (\opts -> opts :> optOptimization > OptimNone) >+= \opt ->
  getNDClass qn     >+= \ndCl ->
  getFuncHOClass qn >+= \hoCl ->
  isDetMode         >+= \dm   ->
  renameFun qn      >+= \qn'  ->
  mapM transExpr es >+= unzipArgs  >+= \(g, es') ->
  genIds g (wrapPartCall dm opt ndCl hoCl i (Comb (FuncPartCall i) qn' es'))

-- let expressions
transExpr (Let ds e) =
  let (vs, es) = unzip ds in
  mapM transExpr es >+= unzipArgs >+= \(g, es') ->
  transExpr e       >+=               \(ge, e') ->
  genIds (g ++ ge) (Let (zip vs es') e')

-- non-determinism
transExpr (Or e1 e2) =
  transExpr e1 >+= \(vs1, e1') ->
  transExpr e2 >+= \(vs2, e2') ->
  takeNextID   >+= \i          ->
  genIds (i : vs1 ++ vs2)
         (choice [e1', e2', Var i, Var nestingIdx, Var constStoreVarIdx])

-- free variable
transExpr (Free vs e) =
  transExpr e             >+= \(g, e') ->
  takeNextIDs (length vs) >+= \is      ->
  genIds (g ++ is) (Let (zipWith (\ v i -> (v, generate (Var i))) vs is) e')

transExpr (Case _ _ _) = failM "TransFunctions.transExpr: case expression"

transExpr (Typed e ty) =
  isDetMode   >+= \dm      ->
  transExpr e >+= \(g, e') ->
  genIds g (Typed e' (transExprType dm ty))

genIds :: [VarIndex] -> Expr -> M ([VarIndex], Expr)
genIds []       e = returnM ([], e)
genIds ns@(_:_) e =
  strictSupply >+= \strict ->
  -- get next free variable id
  getNextID    >+= \i      ->
  -- create splitting of supply variables
  let (vroot, v', vs) = mkSplits i ns
      addSplit (v, v1, v2) e = letIdVar strict
        [(v1, leftSupply [Var v]), (v2, rightSupply [Var v])] e
  in  setNextID v' >+ returnM ([vroot], foldr addSplit e vs)

-- TODO magic numbers
idVar      = 2000

-- Variable index for supply variable
suppVarIdx = 3000

-- Variable index for nesting level
nestingIdx = 3250

-- Variable index for constraint store
constStoreVarIdx = 3500

unzipArgs :: [([VarIndex], e)] -> M ([VarIndex], [e])
unzipArgs ises = returnM (concat is, es) where (is, es) = unzip ises

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
wrapDHO :: Int -> Expr -> Expr
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
wrapPartCall :: Bool -> Bool -> NDClass -> FuncHOClass -> Int -> Expr -> Expr
wrapPartCall dm opt nd ho arity e
  | dm        = wrapCs arity e
  | nd == ND  = newWrap False arity              (wrapCs arity e)
  | otherwise = newWrap useDX (arity + resArity) (wrapCs arity e)
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
wrapCs :: Int -> Expr -> Expr
wrapCs n e | n == 1 = if isConsCall then acceptCs [funId, e] else e
           | n >  1 = acceptCs
           [mkWraps (n - 1)
                              (if isConsCall then acceptCs [funId] else funId)
                            ,e]
 where
  mkWraps m expr | m < 2     = expr
                 | otherwise = mkWraps (m - 1) (acceptCs [expr])
  isConsCall = case e of
    (Comb (ConsPartCall _) _ _) -> True
    _                           -> False

acceptCs = fun 2 (basics, "acceptCs")

-- TODO: simplify
newWrap :: Bool -> Int -> Expr -> Expr
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

wrapDX = fun 2 (basics, "wrapDX")
wrapNX = fun 2 (basics, "wrapNX")
funId  = fun 1 (prelude, "id") []

-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

-- Strict or lazy computation of supplies
letIdVar :: Bool -> [(Int, Expr)] -> Expr -> Expr
letIdVar strict ds e
  | strict    = Let ds $ foldr seqCall e $ map (Var . fst) ds
  | otherwise = Let ds e

curryInt :: QName
curryInt = renameQName (prelude, "Int")

curryFloat :: QName
curryFloat = renameQName (prelude, "Float")

curryChar :: QName
curryChar = renameQName (prelude, "Char")

addCs :: QName
addCs = (basics, "addCs")

-- type expressions

supplyType :: TypeExpr
supplyType  = TCons (basics, "IDSupply") []

coverType :: TypeExpr
coverType = TCons (basics, "Cover") []

storeType :: TypeExpr
storeType = TCons (basics, "ConstStore") []

funcType :: TypeExpr -> TypeExpr -> TypeExpr
funcType t1 t2 = TCons (basics, "Func") [t1, t2]

-- expressions

list2FCList :: [Expr] -> Expr
list2FCList []     = consCall (prelude, "[]") []
list2FCList (e:es) = consCall (prelude, ":") [e,list2FCList es]

pair2FCPair :: (Expr,Expr) -> Expr
pair2FCPair (e1,e2) = consCall (prelude, "(,)") [e1,e2]

seqCall :: Expr -> Expr -> Expr
seqCall e1 e2 = funcCall (prelude, "seq") [e1, e2]

strictCall :: Expr -> Expr -> Expr
strictCall f e = funcCall (prelude, "$!") [f,e]

consCall n xs = Comb ConsCall n xs
funcCall n xs = Comb FuncCall n xs
lambdaCall (q,n) xs = Comb FuncCall (q, '\\' : n) xs

constant qn = consCall qn []

fun :: Int -> QName -> [Expr] -> Expr
fun i n xs | length xs == i = funcCall n xs
           | otherwise      = Comb (FuncPartCall (length xs - i)) n xs

int :: Int -> Expr
int i = funcCall curryInt [constant (prelude, showInt i ++ "#")]

char :: Char -> Expr
char c = funcCall curryChar charExpr
  where
  charExpr
    | ord c < 127 = [constant (prelude, showLiteral (AH.Charc c) ++ "#")]
      -- due to problems with non-ASCII characters in ghc
    | otherwise   = [funcCall (basics, "nonAsciiChr")
                              [constant (prelude, show (ord c) ++ "#")]]

float :: Float -> Expr
float f = funcCall curryFloat [constant (prelude, showFloat f ++ "#")]

failCheck qn vars e
  | isCaseAuxFuncName (snd $ unRenamePrefixedFunc qn) = e
  | otherwise                   = funcCall (basics, "failCheck")
    [ showQName $ unRenameQName qn
    , list2FCList (map (\v -> funcCall (prelude, "show") [Var v]) vars)
    , e
    ]

traceFail cd qn args fail = liftFail
  [ cd
  , funcCall (basics, "traceFail")
    [ showQName qn
    , list2FCList (map (\a -> funcCall (prelude, "show") [a]) args)
    , fail
    ]
  ]

consFail qn arg = liftFail
  [ initCover
  , funcCall (basics, "consFail")
    [ showQName $ unRenameQName qn
    , funcCall (basics, "showCons") [arg]
    ]
  ]

showQName qn = list2FCList $ map (Lit . Charc) (q ++ '.' : n)
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
generate i  = funcCall (basics, "generate") [i, Var nestingIdx]

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

--- List of constructors of known primitive types.
primTypes :: [(QName, QName)]
primTypes = map (\ (x, y) -> ( renameQName (prelude, x)
                             , renameQName (prelude, y))) $
  [ ("Success","Success"), ("True", "Bool"), ("False", "Bool")
  , ("Int", "Int")  , ("Float", "Float"), ("Char", "Char")
  ]
