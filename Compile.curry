--- --------------------------------------------------------------------------
--- ID based curry compiler
---
--- @author  Bernd Braßel, Michael Hanus, Björn Peemöller, Fabian Reck
--- @version February 2011
--- --------------------------------------------------------------------------
module Compile where

import Prelude hiding (lookup)
import FiniteMap (FM, addToFM, emptyFM, mapFM, filterFM, fmToList, listToFM, lookupFM)
import Maybe (fromJust, fromMaybe, isJust)
import List (intersperse, find)
import FileGoodies
import FlatCurry
import FlatCurryGoodies (funcName, consName, updQNamesInProg)

import CallGraph
import LiftCase (liftCases)
import Names
  ( renameModule, renameFile, renameQName, detPrefix, mkChoiceName
  , mkGuardName, externalFunc, externalModule )
import Splits (mkSplits)
import CompilerOpts
import qualified AbstractHaskell as AH
import FlatCurry2AbstractHaskell (fcy2abs)
import AbstractHaskellPrinter (showProg)
import qualified FlatCurry2Types as FC2T (fcyTypes2abs)

main :: IO ()
main = do
  (opts, files) <- compilerOpts
  mapIO_ (compile opts) files

compile :: Options -> String -> IO ()
compile opts fn = do
  info opts $ "Compiling '" ++ fn ++ "'"

  info opts "Reading FlatCurry"
  fcy <- readFlatCurry (stripSuffix fn)
  dumpLevel DumpFlat opts fcyName (show fcy)

  info opts "Lifting case expressions"
  let pLifted = liftCases True fcy
  dumpLevel DumpLifted opts liftedName (show pLifted)

  info opts "Renaming symbols"
  let renamed@(Prog _ _ ts _ _)  = rename pLifted
  dumpLevel DumpRenamed opts renamedName (show renamed)

  info opts "Transforming functions"
  let (tProg, report) = transform opts renamed
      ahsFun@(AH.Prog n imps _ ops funs)= fcy2abs tProg
  info opts report
  dumpLevel DumpFunDecls opts funDeclName (show ahsFun)

  info opts "Transforming type declarations"
  let typeDecls = FC2T.fcyTypes2abs ts
  dumpLevel DumpTypeDecls opts typeDeclName (show typeDecls)

  info opts "Combining to Abstract Haskell"
  let ahs = (AH.Prog n ("ID":"Basics":imps) typeDecls ops funs)
  dumpLevel DumpAbstractHs opts abstractHsName (show ahs)

  info opts "Integrating external declarations"
  integrated <- integrateExternals opts ahs fn

  info opts $ "Generating Haskell module '" ++ destFile ++ "'"
  writeFile destFile integrated

    where
    fcyName        = fcyFile $ withBaseName (++ "Dump")      fn
    liftedName     = fcyFile $ withBaseName (++ "Lifted")    fn
    renamedName    = fcyFile $ withBaseName (++ "Renamed")   fn
    funDeclName    = ahsFile $ withBaseName (++ "FunDecls")  fn
    typeDeclName   = ahsFile $ withBaseName (++ "TypeDecls") fn
    abstractHsName = ahsFile fn
    destFile       =  hsFile $ withBaseName renameFile fn
    fcyFile f = withExtension (const ".fcy") f
    ahsFile f = withExtension (const ".ahs") f
    hsFile  f = withExtension (const ".hs")  f

--
integrateExternals :: Options -> AH.Prog -> String -> IO String
integrateExternals opts (AH.Prog m imps td fd od) fn = do
  exts <- lookupExternals opts fn
  let (pragma, extimps, extdecls) = splitExternals exts
      prog' = AH.Prog m (imps ++ extimps) td fd od
  return $ unlines [pragma, showProg prog', unlines extdecls]

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
info opts msg = if opts -> optQuiet
  then return ()
  else putStrLn (msg ++ " ...")

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
-- state monad
-- ---------------------------------------------------------------------------
data Mo st a = M (st -> (st, a))

unM :: Mo st a -> (st -> (st, a))
unM (M x) = x

returnM :: a -> Mo st a
returnM x = M (\st -> (st, x))

bindM :: Mo st a -> (a -> Mo st b) -> Mo st b
bindM f g = M (\st -> case unM f st of
                        (st', x) -> unM (g x) st')

bindM_ :: Mo st a -> Mo st b -> Mo st b
bindM_ f g = f `bindM` \_ -> g

mapM :: (a -> Mo st b) -> [a] -> Mo st [b]
mapM _ [] = returnM []
mapM f (m:ms) = f m       `bindM` \m' ->
                mapM f ms `bindM` \ms' ->
                returnM (m':ms')

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

type TypeMap = FM QName QName

type State =
  { typeMap    :: TypeMap
  , ndResult   :: NDResult
  , nextID     :: VarIndex    -- index for fresh variable
  , detMode    :: Bool        -- determinism mode
  , searchMode :: SearchMode
  , report     :: [String]
  }

defaultState :: State
defaultState =
  { typeMap    = (listToFM (<) primTypes)
  , ndResult   = (emptyFM (<))
  , nextID     = idVar
  , detMode    = False
  , searchMode = NoSearch
  , report     = []
  }

type M a = Mo State a

getState :: M State
getState = M (\st -> (st, st))

putState :: State -> M ()
putState st = M (\ _ -> (st, ()))

updState :: (State -> State) -> M ()
updState f = getState `bindM` \st -> putState (f st)

-- type map

updTypeMap :: (TypeMap -> TypeMap) -> M ()
updTypeMap f = updState (\st -> { typeMap := f $ st -> typeMap | st })

getType :: QName -> M QName
getType qn = getState `bindM` \st ->
  returnM $ fromMaybe (error $ show qn ++ " not in type map" )
  $ (flip lookupFM) qn $ (st -> typeMap)

-- NDResult

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

-- ---------------------------------------------------------------------------
-- Program transformation
-- ---------------------------------------------------------------------------

transform :: Options -> Prog -> (Prog, String)
transform opts prog = (tProg, unlines $ reverse $ state -> report)
 where
  (state, tProg) = unM (transProg (opts -> optDetOptimization) prog) initState
  initState = { searchMode := opts -> optSearchMode
              , ndResult   := analyseNd prog
              | defaultState
              }

transProg :: Bool -> Prog -> M Prog
transProg optimize (Prog m is ts fs _) = -- doInDetMode False $
  -- register constructors
  mapM registerCons ts `bindM_`
  -- translation of the functions
  mapM (transFunc optimize) fs `bindM` \fss ->
  -- ( filter ((`notElem` ["main_","searchTree"]) . snd . funcName) fs)
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

transFunc :: Bool -> FuncDecl -> M [FuncDecl]
transFunc False f = transNDFunc f `bindM` \ fn -> returnM [fn]
transFunc True f@(Func qn _ _ _ _) =
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
  | n == 0 = FuncType supplyType (transHOTypeExpr t)
  | n >  0 = case t of
              (FuncType t1 t2) ->
                FuncType (transHOTypeExpr t1) (transTypeExpr (n-1) t2)
              _ -> error $ "transTypeExpr: " ++ show (n, t)
  | n <  0 = error $ "transTypeExpr: " ++ show (n, t)

-- Recursively translate (->) into Func
transHOTypeExpr :: TypeExpr -> TypeExpr
transHOTypeExpr t@(TVar _)       = t
transHOTypeExpr (FuncType t1 t2) = funcType (transHOTypeExpr t1) (transHOTypeExpr t2)
transHOTypeExpr (TCons qn ts)    = TCons qn $ map transHOTypeExpr ts

-- translate a single rule of a function
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
  _ -> transExprToLet exp

-- translate case branch
transBranch :: BranchExpr -> M BranchExpr
transBranch (Branch (Pattern p vs) e) =
  transExprToLet e `bindM` \e' ->
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

transExprToLet :: Expr -> M Expr
transExprToLet e =
  getNextID `bindM` \i -> -- save current variable id
  transExpr e `bindM` \(g, e') ->
  let e'' = case g of
              []  -> e'
              [v] ->  lazyLet [(v, Var suppVarIdx)] e' in
  setNextID i `bindM_` -- and reset it
  returnM e''

-- transform an expression into a new expression and a list of new variables
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
transExpr e@(Comb (ConsPartCall _) _ _) = returnM ([], e) -- TODO give reasonable implementation

-- functions
transExpr (Comb FuncCall qn es) =
  getNDClass qn `bindM` \ndCl ->
  isDetMode `bindM` \dm ->
  renameFun qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  if ndCl /= ND && dm -- was: ndCl == DFO || (ndCl == DHO && dm)
    then genIds g (Comb FuncCall qn' es')
    else takeNextID  `bindM` \i ->
         genIds (i:g) (Comb FuncCall qn' (es' ++ [Var i]))
transExpr (Comb (FuncPartCall i) qn es) =
  getNDClass qn `bindM` \ndCl ->
  isDetMode `bindM` \dm ->
  renameFun qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs  `bindM` \(g,es') ->
  case ndCl of
    _ -> genIds g (wrap dm ndCl i (Comb (FuncPartCall i) qn' es'))

    -- TODO: we do not care about higher order calls to nd functions right now
    -- _    -> takeNextID `bindM` \i ->
    --   genIds (i:g) (Comb FuncCall qn' (es' ++ [Var i]))

-- let expressions
transExpr (Let vses e) =
  let (vs,es) = unzip vses in
  mapM transExpr es `bindM` unzipArgs  `bindM` \(g, es') ->
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
genIds ns e =
  -- get next free variable id
  getNextID `bindM` \i ->
  -- create splitting of supply variables
  let (v', vs) = mkSplits i ns in
  case vs of
    -- no splitting necessary
    [] -> returnM (ns, e)
    --
    _  -> setNextID (v'+1) `bindM_` returnM ([v'], foldr addSplit e vs)
 where addSplit (v, v1, v2) e' =
        lazyLet [(v1, leftSupply [Var v]), (v2, rightSupply [Var v])] e'

-- TODO magic numbers
idVar      = 2000

-- Variable index for supply variable
suppVarIdx = 3000

freshVars :: [Int] -> [Int]
freshVars used = filter (`elem` used) [0 .. ]

unzipArgs :: [([VarIndex], e)] -> M ([VarIndex], [e])
unzipArgs ises = returnM (concat is, es) where (is, es) = unzip ises

-- Wrap a function with to a Func type
wrap :: Bool -> NDClass -> Int -> Expr -> Expr
wrap True  _  _ e = e
wrap False nd a e = wrap'' (fun 1 (wrapName nd) []) e a

wrap'' :: Expr -> Expr -> Int -> Expr
wrap'' f e n = if n == 0 then e else apply f (wrap'' (point [f]) e (n-1))

wrapName :: NDClass -> QName
wrapName ndMode = case ndMode of
--  DFO -> ("", "wrapD")
  _   -> ("", "wrapN")

point :: [Expr] -> Expr
point = fun 2 ("", ".")

apply :: Expr -> Expr -> Expr
apply (Comb (FuncPartCall i) qn xs) e =
  Comb (if i==1 then FuncCall else FuncPartCall (i-1)) qn (xs++[e])

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
primTypes = map (\ (x, y) -> (renameQName (prelude, x), renameQName (prelude, y))) $
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
