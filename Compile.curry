--- --------------------------------------------------------------------------
--- ID based curry compiler
---
--- @author  Bernd Braßel, Michael Hanus, Björn Peemöller, Fabian Reck
--- @version February 2011
--- --------------------------------------------------------------------------
module Compile where

import Prelude hiding (lookup)
import FiniteMap (FM, addToFM, emptyFM, mapFM, filterFM, fmToList, listToFM, lookupFM, plusFM)
import Maybe (fromJust, fromMaybe, isJust)
import List (intersperse, find)
import FileGoodies
import FlatCurry
import FlatCurryGoodies (funcName, consName, updQNamesInProg)

import qualified AbstractHaskell as AH
import AbstractHaskellPrinter (showProg)
import CallGraph
import CompilerOpts
import FlatCurry2AbstractHaskell (fcy2abs)
import qualified FlatCurry2Types as FC2T (fcyTypes2abs)
import LiftCase (liftCases)
import ModuleDeps (ModuleIdent, deps)
import Names
  ( renameModule, renameFile, renameQName, detPrefix, mkChoiceName
  , mkGuardName, externalFunc, externalModule )
import Splits (mkSplits)
import Utils (foldIO)

main :: IO ()
main = do
  (opts, files) <- compilerOpts
  mapIO_ (compile opts) files

compile :: Options -> String -> IO ()
compile opts fn = do
  (mods, errs) <- deps (stripSuffix fn)
  if null errs
    then foldIO (compileModule (length mods))
         initState (zip mods [1 .. ]) >> done
    else mapIO_ putStrLn errs
    where initState = { compOptions := opts
                      | defaultState
                      }

compileModule :: Int -> State -> ((ModuleIdent, Prog), Int) -> IO State
compileModule total state ((mid, fcy), current) = do
  let opts = state -> compOptions
  putStrLn $ compMessage current total mid

  let fcy' = filterPrelude opts fcy
  dumpLevel DumpFlat opts fcyName (show fcy')

  status opts "Lifting case expressions"
  let pLifted = liftCases True fcy'
  dumpLevel DumpLifted opts liftedName (show pLifted)

  status opts "Renaming symbols"
  let renamed@(Prog _ _ ts _ _)  = rename pLifted
  dumpLevel DumpRenamed opts renamedName (show renamed)

  status opts "Transforming functions"
  (tProg, s') <- unM (transProg renamed) state
  info opts $ unlines $ reverse $ s' -> report
  let state' = { report := [] | s'} -- TODO hacky

  let ahsFun@(AH.Prog n imps _ ops funs) = fcy2abs tProg
  dumpLevel DumpFunDecls opts funDeclName (show ahsFun)

  status opts "Transforming type declarations"
  let typeDecls = FC2T.fcyTypes2abs ts
  dumpLevel DumpTypeDecls opts typeDeclName (show typeDecls)

  status opts "Combining to Abstract Haskell"
  let ahs = (AH.Prog n ("ID":"Basics":imps) typeDecls ops funs)
  dumpLevel DumpAbstractHs opts abstractHsName (show ahs)

  status opts "Integrating external declarations"
  integrated <- integrateExternals opts ahs mid

  status opts $ "Generating Haskell module " ++ destFile
  writeFile destFile integrated

  return state'

    where
    fcyName        = fcyFile $ withBaseName (++ "Dump")      mid
    liftedName     = fcyFile $ withBaseName (++ "Lifted")    mid
    renamedName    = fcyFile $ withBaseName (++ "Renamed")   mid
    funDeclName    = ahsFile $ withBaseName (++ "FunDecls")  mid
    typeDeclName   = ahsFile $ withBaseName (++ "TypeDecls") mid
    abstractHsName = ahsFile mid
    destFile       =  hsFile $ withBaseName renameFile mid
    fcyFile f = withExtension (const ".fcy") f
    ahsFile f = withExtension (const ".ahs") f
    hsFile  f = withExtension (const ".hs")  f

compMessage :: Int -> Int -> String -> String
compMessage curNum maxNum mod = '[' : fill max (show curNum) ++ " of "
  ++ show maxNum  ++ "]" ++ " Compiling " ++ mod
    where
      max = length $ show maxNum
      fill n s = take n $ s ++ repeat ' '

filterPrelude :: Options -> Prog -> Prog
filterPrelude opts p@(Prog m imps td fd od) =
  if (opts -> optXNoImplicitPrelude)
    then Prog m (filter (/= "Prelude") imps) td fd od
    else p

--
integrateExternals :: Options -> AH.Prog -> String -> IO String
integrateExternals opts (AH.Prog m imps td fd od) fn = do
  exts <- lookupExternals opts fn
  let (pragma, extimps, extdecls) = splitExternals exts
      prog' = AH.Prog m (imps ++ extimps) td fd od
  return $ unlines $ filter (not . null) [pragma, showProg prog', unlines extdecls]

-- lookup an external file for a module and return either the content or an
-- empty String
lookupExternals :: Options -> String -> IO String
lookupExternals opts fn = do
  info opts $ "Looking for external file: " ++ extName ++ ".hs"
  mExternal <- lookupFileInPath extName [".hs"] ["."]
  maybe (info opts "No External file found" >> return "")
        (\ ext -> info opts "External file found" >> readFile ext)
        mExternal
    where extName = path ++ separatorChar : externalModule ++ '_' : bareName
          (path,file) = splitDirectoryBaseName fn
          bareName = stripSuffix file

-- Split an external file into a pragma String, a list of imports and the rest
-- TODO: This is a bloody hack
splitExternals :: String -> (String, [String], [String])
splitExternals content = se (lines content) ([], [], []) where
  se [] res = res
  se (ln:lns) res
    | take 3 ln == "{-#"     = (ln, imps, decls)
    | take 7 ln == "import " = (pragma, drop 7 ln : imps, decls)
    | otherwise              = (pragma, imps, ln : decls)
      where (pragma, imps, decls) = se lns res

-- Show an info message unless the quiet flag is set
info :: Options -> String -> IO ()
info opts msg = if opts -> optQuiet then done else putStrLn msg

status :: Options -> String -> IO ()
status opts msg = info opts $ msg ++ " ..."

-- Dump an intermediate result to a file
dumpLevel :: Dump -> Options -> String -> String -> IO ()
dumpLevel level opts file src = if level `elem` opts -> optDump
  then info opts ("Dumping " ++ file) >> writeFile file src
  else return ()

rename :: Prog -> Prog
rename p@(Prog name imports _ _ _) =
  Prog (renameModule name) (map renameModule imports) td fd od where
  (Prog _ _ td fd od) = updQNamesInProg renameQName p

-- ---------------------------------------------------------------------------
-- IO state monad, like StateT IO
-- ---------------------------------------------------------------------------
data Mo s a = M (s -> IO (a, s))

unM :: Mo s a -> s -> IO (a, s)
unM (M x) = x

returnM :: a -> Mo s a
returnM x = M $ \s -> return (x, s)

bindM :: Mo s a -> (a -> Mo s b) -> Mo s b
bindM f g = M $ \s -> do
  (x, s') <- unM f s
  unM (g x) s'

bindM_ :: Mo st a -> Mo st b -> Mo st b
bindM_ f g = f `bindM` \_ -> g

getState :: Mo s s
getState = M $ \s -> return (s, s)

putState :: s -> Mo s ()
putState s = M $ \ _ -> return ((), s)

updState :: (s -> s) -> Mo s ()
updState f = getState `bindM` \s -> putState (f s)

liftIO :: IO a -> Mo s a
liftIO act = M $ \s -> do
  a <- act
  return (a, s)

mapM :: (a -> Mo s b) -> [a] -> Mo s [b]
mapM _ [] = returnM []
mapM f (m:ms) = f m       `bindM` \m' ->
                mapM f ms `bindM` \ms' ->
                returnM (m':ms')

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

type TypeMap = FM QName QName

type State =
  { typeMap     :: TypeMap
  , ndResult    :: NDResult
  , nextID      :: VarIndex    -- index for fresh variable
  , detMode     :: Bool        -- determinism mode
  , report      :: [String]
  , compOptions :: Options     -- compiler options
  }

defaultState :: State
defaultState =
  { typeMap     = (listToFM (<) primTypes)
  , ndResult    = (emptyFM (<))
  , nextID      = idVar
  , detMode     = False
  , report      = []
  , compOptions = defaultOptions
  }

type M a = Mo State a

-- type map

updTypeMap :: (TypeMap -> TypeMap) -> M ()
updTypeMap f = updState (\st -> { typeMap := f $ st -> typeMap | st })

getType :: QName -> M QName
getType qn = getState `bindM` \st ->
  returnM $ fromMaybe (error $ show qn ++ " not in type map" )
  $ (flip lookupFM) qn $ (st -> typeMap)

-- NDResult

addNDAnalysis :: NDResult -> M ()
addNDAnalysis nd = updState $
  \s -> { ndResult := (s -> ndResult) `plusFM` nd | s }

getNDClass :: QName -> M NDClass
getNDClass qn = getState `bindM` \st ->
  returnM $ fromMaybe (error $ show qn ++ " not analysed" )
  $ (flip lookupFM) qn $ (st -> ndResult)

-- IDs

getNextID :: M Int
getNextID = getState `bindM` \st -> returnM (st -> nextID)

setNextID :: Int -> M ()
setNextID i = updState (\st -> { nextID := i | st })

takeNextID :: M Int
takeNextID =
  getState `bindM` \st ->
  let i = st -> nextID in
  putState ({ nextID := (i + 1) | st }) `bindM_`
  returnM i

takeNextIDs :: Int -> M [Int]
takeNextIDs n =
  getState `bindM` \st ->
  let i = st -> nextID in
  putState ({ nextID := (i + n) | st }) `bindM_`
  returnM [i .. i+n-1]

-- DetMode

isDetMode :: M Bool
isDetMode = getState `bindM` \st -> returnM (st -> detMode)

setDetMode :: Bool -> M ()
setDetMode dm = updState (\st -> { detMode := dm | st})

-- Perform an action in a given detMode and restore the original mode
-- afterwards
doInDetMode :: Bool -> M a -> M a
doInDetMode dm action =
  isDetMode `bindM` \ oldDm ->
  setDetMode dm `bindM_`
  action `bindM` \ retVal ->
  setDetMode oldDm `bindM_`
  returnM retVal

-- add a message to the transformation report
addToReport :: String -> M ()
addToReport msg = updState (\st -> {report := (msg : st -> report) | st})

-- Compiler options
getCompOptions :: M Options
getCompOptions = getState `bindM` \ st -> returnM (st -> compOptions)

getCompOption :: (Options -> a) -> M a
getCompOption select = getCompOptions `bindM` (returnM . select)

-- ---------------------------------------------------------------------------
-- Program transformation
-- ---------------------------------------------------------------------------
transProg :: Prog -> M Prog
transProg p@(Prog m is ts fs _) =
  addNDAnalysis (analyseNd p) `bindM_`
  -- register constructors
  mapM registerCons ts `bindM_`
  -- translation of the functions
  mapM transFunc fs `bindM` \fss ->
  returnM $ Prog m is [] (concat fss) []

-- Register the types of constructors to be able to retrieve the types for
-- constructors used in case patterns.
-- TODO: This becomes needless if the type could be computed from the
-- function's type expression, which in turn requires the case lifting to
-- provide correct types for lifted case expressions instead of TVar (-42).
registerCons :: TypeDecl -> M ()
registerCons (Type qn _ _ cs) = mapM addToMap cs `bindM_` returnM ()
  where addToMap c = updTypeMap (\fm -> addToFM fm (consName c) qn)
registerCons (TypeSyn _ _ _ _) = returnM ()

-- ---------------------------------------------------------------------------
-- Translation of Curry functions
-- ---------------------------------------------------------------------------

transFunc :: FuncDecl -> M [FuncDecl]
transFunc f@(Func qn _ _ _ _) =
  getCompOption (\opts -> opts -> optDetOptimization) `bindM` \ opt ->
  case opt of
    -- translate all functions as non-deterministic by default
    False -> transNDFunc f `bindM` \ fn -> returnM [fn]
    True  ->
      getNDClass qn `bindM` \ndCl ->
      addToReport (snd qn ++ " is " ++ show ndCl) `bindM_`
      case ndCl of
        DFO ->
          -- create deterministic function
          transPureFunc f `bindM` \ fd ->
          returnM [fd]
        DHO ->
          -- create deterministic as well as non-deterministic function
          transPureFunc f `bindM` \ fd ->
          transNDFunc   f `bindM` \ fn ->
          returnM [fd, fn]
        ND ->
          -- create non-deterministic function
          transNDFunc   f `bindM` \ fn ->
          returnM [fn]

-- translate into deterministic function
transPureFunc :: FuncDecl -> M FuncDecl
transPureFunc (Func qn a v t r) = doInDetMode True $
  renameFun qn `bindM` \qn' ->
  transRule (Func qn' a v t r) `bindM` \r' ->
  returnM (Func qn' a v t r')

-- translate into non-deterministic function
transNDFunc :: FuncDecl -> M FuncDecl
transNDFunc (Func qn a v t r) = doInDetMode False $
  renameFun qn `bindM` \qn' ->
  transRule (Func qn' a v t r) `bindM` \r' ->
  returnM (Func qn' (a + 1) v (check42 (transTypeExpr a) t) r')

-- renaming of functions respective to their order and the determinism mode
renameFun :: QName -> M QName
renameFun qn@(q, n) =
  isDetMode `bindM` \dm ->
  getNDClass qn `bindM` \ndCl ->
  returnM (q, (detPrefix $ dm && ndCl == DHO) ++ n)

check42 :: (TypeExpr -> TypeExpr) -> TypeExpr -> TypeExpr
check42 f t = case t of
  (TVar (-42)) -> t
  _            -> f t

-- translate a type expression by replacing (->) with Funcs and inserting
-- an additional IDSupply type
transTypeExpr :: Int -> TypeExpr -> TypeExpr
transTypeExpr n t
    -- all arguments are applied, that means
    -- functions as a result are represented as Funcs,
    -- other results are represented as IDSupply -> a
  | n == 0 = FuncType supplyType (transHOTypeExpr t)
  | n >  0 = case t of
              (FuncType t1 t2) ->
                FuncType (transHOTypeExpr t1) (transTypeExpr (n-1) t2) -- FuncType or funcType?
              _ -> error $ "transTypeExpr: " ++ show (n, t)
  | n <  0 = error $ "transTypeExpr: " ++ show (n, t)

-- Recursively translate (->) into Func
transHOTypeExpr :: TypeExpr -> TypeExpr
transHOTypeExpr t@(TVar _)       = t
transHOTypeExpr (FuncType t1 t2) = funcType (transHOTypeExpr t1) (transHOTypeExpr t2)
transHOTypeExpr (TCons qn ts)    = TCons qn (map transHOTypeExpr ts)

-- translate a single rule of a function
-- TODO: Correct handling of external functions
transRule :: FuncDecl -> M Rule
transRule (Func qn _ _ _ (Rule vs e)) =
  isDetMode `bindM` \ dm ->
  transBody qn vs e `bindM` \e' ->
  returnM $ Rule (if dm then vs else vs ++ [suppVarIdx]) e'
transRule (Func qn a _ _ (External _)) =
  isDetMode `bindM` \ dm ->
  let vs = if dm then [1 .. a] else [1 .. a] ++ [suppVarIdx] in
  returnM $ Rule vs (funcCall (externalFunc qn) (map Var vs))

transBody :: QName -> [Int] -> Expr -> M Expr
transBody qn vs exp = case exp of
  -- case expression with variable
  (Case ct e@(Var i) bs) ->
    -- translate branches
    mapM transBranch bs `bindM` \bs' ->
    -- create branches for non-deterministic constructors
    let (Branch (Pattern pConsName _) _:_) = bs in
    newBranches qn vs i pConsName `bindM` \ns ->
    -- TODO: superfluous?
    transExpr e `bindM` \(_, e') ->
    returnM (Case ct e' (bs' ++ ns))
  _ -> transCompleteExpr exp

-- translate case branch
transBranch :: BranchExpr -> M BranchExpr
transBranch (Branch (Pattern p vs) e) =
  transCompleteExpr e `bindM` \e' ->
  returnM (Branch (Pattern p vs) e')

-- create new case branches for added non-deterministic constructors
-- qn'      : qualified name of the function currently processed
-- vs       : function arguments
-- i        : variable matched by case
-- pConsName: name of an arbitrary constructor of the type of the matched
--            variable
newBranches :: QName -> [Int] -> Int -> QName -> M [BranchExpr]
newBranches qn' vs i pConsName =
  isDetMode `bindM` \ dm ->
  -- lookup type name to create appropriate constructor names
  getType pConsName `bindM` \ typeName ->
  let Just pos = find (==i) vs
      is = if dm then [] else [suppVarIdx]
      (vs1, _ : vs2) = break (==pos) vs
      call v = funcCall qn' $ map Var (vs1 ++ v : vs2 ++ is) in
  returnM $
    [ Branch (Pattern (mkChoiceName typeName) [1000, 1001, 1002])
             (liftOr [Var 1000, call 1001, call 1002])
    , Branch (Pattern (mkGuardName typeName) [1000, 1001])
             (liftGuard [Var 1000, call 1001])
    , Branch (Pattern ("", "_") [])
             liftFail
    ] -- TODO Magic numbers?

-- Complete translation of an expression where all newly introduced supply
-- variables are already bound by nested let expressions
transCompleteExpr :: Expr -> M Expr
transCompleteExpr e =
  getNextID `bindM` \i -> -- save current variable id
  transExpr e `bindM` \(g, e') ->
  let e'' = case g of
              []  -> e'
              [v] ->  lazyLet [(v, Var suppVarIdx)] e' in
  setNextID i `bindM_` -- and reset it variable id
  returnM e''

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
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  genIds g (Comb ConsCall qn es')
-- calls to partially applied constructors are treated like calls to partially
-- applied deterministic first order functions.
transExpr (Comb (ConsPartCall i) qn es) =
  isDetMode `bindM` \dm ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  genIds g (myWrap dm True DFO i (Comb (ConsPartCall i) qn es'))

-- fully applied functions
transExpr (Comb FuncCall qn es) =
  getCompOption (\opts -> opts -> optDetOptimization) `bindM` \opt ->
  getNDClass qn `bindM` \ndCl ->
  isDetMode `bindM` \dm ->
  renameFun qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  if opt && (ndCl == DFO || (ndCl == DHO && dm))
    then genIds g (Comb FuncCall qn' es')
    else takeNextID  `bindM` \i ->
         genIds (i:g) (Comb FuncCall qn' (es' ++ [Var i]))

-- partially applied functions
transExpr (Comb (FuncPartCall i) qn es) =
  getCompOption (\opts -> opts -> optDetOptimization) `bindM` \opt ->
  getNDClass qn `bindM` \ndCl ->
  isDetMode `bindM` \dm ->
  renameFun qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs  `bindM` \(g, es') ->
  case ndCl of
    _ -> genIds g (myWrap dm opt ndCl i (Comb (FuncPartCall i) qn' es'))

    -- TODO: we do not care about higher order calls to nd functions right now
    -- _    -> takeNextID `bindM` \i ->
    --   genIds (i:g) (Comb FuncCall qn' (es' ++ [Var i]))

-- let expressions
transExpr (Let vses e) =
  let (vs,es) = unzip vses in
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  transExpr e `bindM` \(ge, e') ->
  genIds (g ++ ge) (Let (zip vs es') e')

-- non-determinism
transExpr (Or e1 e2) = transExpr (qmark e1 e2)

-- free variable
transExpr (Free vs e) =
  transExpr e `bindM` \(g, e') ->
  takeNextIDs (length vs) `bindM` \is ->
  genIds (g++is) (Let (zipWith (\ v i -> (v,generate (Var i))) vs is) e')

-- case
transExpr e@(Case _ _ _) = returnM ([], e) -- TODO give reasonable implementation

genIds :: [VarIndex] -> Expr -> M ([VarIndex], Expr)
genIds [] expr = returnM ([], expr)
genIds ns@(_:_) expr =
  -- get next free variable id
  getNextID `bindM` \i ->
  -- create splitting of supply variables
  let (vroot, v', vs) = mkSplits i ns in
  setNextID v' `bindM_`
  returnM ([vroot], foldr addSplit expr vs)
  where
    addSplit (v, v1, v2) e =
      lazyLet [(v1, leftSupply [Var v]), (v2, rightSupply [Var v])] e
{-
  case vs of
    -- no splitting necessary
    [] -> returnM (ns, e)
    --
    _  -> setNextID (v'+1) `bindM_` returnM ([v'], foldr addSplit e vs)
 where addSplit (v, v1, v2) e' =
        lazyLet [(v1, leftSupply [Var v]), (v2, rightSupply [Var v])] e'
  -}

-- TODO magic numbers
idVar      = 2000

-- Variable index for supply variable
suppVarIdx = 3000

freshVars :: [Int] -> [Int]
freshVars used = filter (`elem` used) [0 .. ]

unzipArgs :: [([VarIndex], e)] -> M ([VarIndex], [e])
unzipArgs ises = returnM (concat is, es) where (is, es) = unzip ises

-- ---------------------------------------------------------------------------
-- Wrapping
-- ---------------------------------------------------------------------------

-- Wrap a function call to make the argument a Func
wrap :: Bool -> Bool -> NDClass -> Int -> Expr -> Expr
wrap True  _   _  _ e = e
wrap False opt nd a e = wrap'' (fun 1 (wrapName nd opt) []) e a

wrap'' :: Expr -> Expr -> Int -> Expr
wrap'' f e n = if n == 0 then e else apply f (wrap'' (point [f]) e (n-1))

wrapName :: NDClass -> Bool -> QName
wrapName ndMode opt = case ndMode of
  DFO -> if opt then ("", "wrapD") else ("", "wrapN")
  _   -> ("", "wrapN")

point :: [Expr] -> Expr
point = fun 2 ("", ".")

apply :: Expr -> Expr -> Expr
apply (Comb (FuncPartCall i) qn xs) e =
  Comb (if i == 1 then FuncCall else FuncPartCall (i - 1)) qn (xs ++ [e])

-- new wrapping, but also broken

myWrap :: Bool -> Bool -> NDClass -> Int -> Expr -> Expr
myWrap True  _   _  _ e = e
myWrap False opt nd a e = newWrap a iw e
  where iw = if opt && nd == DFO then wrapDX else wrapNX

newWrap :: Int -> ([Expr] -> Expr) -> Expr -> Expr
newWrap n innermostWrapper e
  | n == 0 = e
  | n == 1 = innermostWrapper [funId, e]
  | n == 2 = wrapDX [innermostWrapper [funId], e]
  | n == 3 = wrapDX [wrapDX [innermostWrapper [funId]], e]
  | n == 4 = wrapDX [wrapDX [wrapDX [innermostWrapper [funId]]], e]
  | n >  4 = wrapDX [wraps (n-1) (innermostWrapper [fun 1 ("","id") []]), e]
  where wraps m expr = if m <= 1 then expr else wrapDX [wraps (m - 1) expr]

wrapDX exprs = fun 2 ("","wrapDX") exprs
wrapNX exprs = fun 2 ("","wrapNX") exprs
funId = fun 1 ("","id") []


-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

prelude :: String
prelude = renameModule "Prelude"

-- type expressions

tOrRef :: TypeExpr
tOrRef = TCons (prelude, "ID") []

tConstraint :: TypeExpr
tConstraint = TCons (prelude, "Constraint") []

supplyType :: TypeExpr
supplyType  = TCons (prelude, "IDSupply") []

funcType :: TypeExpr -> TypeExpr -> TypeExpr
funcType t1 t2 = TCons (prelude, "Func") [t1, t2]

-- expressions

lazyLet :: [(Int, Expr)] -> Expr -> Expr
lazyLet decls e = Let decls e

strictLet :: [(Int, Expr)] -> Expr -> Expr
strictLet decls e = Let decls $ foldr seqCall e $ map (Var . fst) decls

seqCall :: Expr -> Expr -> Expr
seqCall e1 e2 = funcCall (prelude, "seq") [e1, e2]

funcCall n xs = Comb FuncCall n xs
consCall n xs = Comb ConsCall n xs
constant qn = consCall qn []

fun :: Int -> QName -> [Expr] -> Expr
fun i n xs | length xs == i = funcCall n xs
           | otherwise      = Comb (FuncPartCall (length xs - i)) n xs

int :: Integer -> Expr
int i = constant (prelude, "(C_Int " ++ show i ++ "#)")

char :: Char -> Expr
char c = constant (prelude, "(C_Char " ++ show c ++ "#)")

float :: Float -> Expr
float f = constant (prelude, "(C_Float " ++ show f ++ "#)")

liftOr      = funcCall (prelude, "narrow")
liftGuard   = funcCall (prelude, "guardCons")
liftFail    = funcCall (prelude, "failCons") []
qmark e1 e2 = funcCall (prelude, "OP_qmark") [e1, e2]

splitSupply = funcCall (prelude, "splitSupply")
initSupply  = funcCall (prelude, "initIDSupply") []
leftSupply  = funcCall (prelude, "leftSupply")
rightSupply = funcCall (prelude, "rightSupply")
generate i  = funcCall ("","generate") [i]

{-
funCall n = funcCall ("", n)
bind e1 e2 = funCall ">>=" [e1, e2]
prinT0 = Comb (FuncPartCall 1) ("","print") []
prinT e = funCall "print" [e]
e1 .* e2 = funCall "." [e1,e2]

dfs0 = Comb (FuncPartCall 1) ("","dfs") []
bfs0 = Comb (FuncPartCall 1) ("","bfs") []
par0 = Comb (FuncPartCall 1) ("","par") []
idfs0 g = Comb FuncCall  ("","idfs") [g]
-}

{-
wrap' :: NDClass -> Int -> Expr -> Expr
wrap' nd n e = case n of
  0 -> e
  1 -> funcCall (wrapName nd) [e]
  _ -> funcCall (wrapName DFO) [wrap' nd (n-1) e]
-}

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

-- list of known primitive types
primTypes :: [(QName, QName)]
primTypes = map (\ (x, y) -> ( renameQName ("Prelude", x)
                             , renameQName ("Prelude", y))) $
  [ ("True", "Bool"), ("False", "Bool")
  , ("[]", "List")  , (":", "List")
  ] ++ map (\n -> (tupleType n, 'T':show n)) [2 .. maxTupleArity]

-- Return Nothing if type is no tuple and Just arity otherwise
tupleArity :: String -> Maybe Int
tupleArity s
  | arity > 1 && s == '(' : replicate (arity -1) ',' ++ ")" = Just arity
  | otherwise                                               = Nothing
  where arity = length s - 1

maxTupleArity :: Int
maxTupleArity = 15

tupleType :: Int -> String
tupleType arity = '(' : replicate (arity - 1) ',' ++ ")"

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

-- file utils
withBaseName :: (String -> String) -> String -> String
withBaseName f fn = dirName fn
  ++ separatorChar : (f $ stripSuffix $ baseName fn)
  ++ suffixSeparatorChar : fileSuffix fn

withExtension :: (String -> String) -> String -> String
withExtension f fn = stripSuffix fn ++ f (fileSuffix fn)
