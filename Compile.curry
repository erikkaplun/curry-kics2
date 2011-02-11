module Compile where

import Prelude hiding (lookup)
import FiniteMap hiding (mapFM, filterFM)
import Maybe (fromJust, fromMaybe, isJust)
import List (intersperse, find)
import FileGoodies
import FlatCurry
import FlatCurryGoodies (funcName, consName, updQNamesInProg)

import CallGraph
import LiftCase (liftCases)
import Names (renameModule, renameFile, renameQName, detPrefix)
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
  let ahsFun@(AH.Prog n imps _ ops funs)= fcy2abs $ transform opts renamed
  dumpLevel DumpFunDecls opts funDeclName (show ahsFun)

  info opts "Transforming type declarations"
  let typeDecls = FC2T.fcyTypes2abs ts
  dumpLevel DumpTypeDecls opts typeDeclName (show typeDecls)

  info opts "Combining to Abstract Haskell"
  let ahs = (AH.Prog n ("ID":"Basics":imps) typeDecls ops funs)
  dumpLevel DumpAbstractHs opts abstractHsName (show ahs)


  info opts $ "Generating Haskell module '" ++ destFile ++ "'"
  writeFile destFile (showProg ahs)

    where
    fcyName = fcyFile $ withBaseName (++ "Dump") fn
    liftedName = fcyFile $ withBaseName (++ "Lifted") fn
    renamedName = fcyFile $ withBaseName (++ "Renamed") fn
    funDeclName = ahsFile $ withBaseName (++ "FunDecls") fn
    typeDeclName = ahsFile $ withBaseName (++ "TypeDecls") fn
    abstractHsName = ahsFile fn
    destFile = withExtension (const ".hs") $ withBaseName renameFile fn
    fcyFile f = withExtension (const ".fcy") f
    ahsFile f = withExtension (const ".ahs") f

-- Show an info message unless the quiet flag is set
info :: Options -> String -> IO ()
info opts msg = if opts -> optQuiet
  then return ()
  else putStrLn (msg ++ " ...")

-- Dump an intermediate result to a file
dumpLevel :: Dump -> Options -> String -> String -> IO ()
dumpLevel level opts file src = if level `elem` opts -> dump
  then info opts ("Dumping " ++ file) >> writeFile file src
  else return ()

rename :: Prog -> Prog
rename p@(Prog name imports _ _ _) =
  Prog (renameModule name) (map renameModule imports) td fd od where
  (Prog _ _ td fd od) = updQNamesInProg renameQName p

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

type TypeMap = FM QName QName

type State =
  { typeMap    :: TypeMap
  , cont       :: (Maybe (Bool, QName, [VarIndex]))
  , matchPos   :: Int
  , ndResult   :: NDResult
  , nextID     :: VarIndex
  , detMode    :: Bool
  , searchMode :: SearchMode
  }

defaultState :: State
defaultState =
  { typeMap    = (listToFM (<) primTypes)
  , cont       = Nothing
  , matchPos   = (-1)
  , ndResult   = (emptyFM (<))
  , nextID     = idVar
  , detMode    = False
  , searchMode = NoSearch
  }

-- ---------------------------------------------------------------------------
-- state monad
-- ---------------------------------------------------------------------------
data Mo st a = M (st -> (st, a))

unM :: Mo st a -> (st -> (st, a))
unM (M x) = x

returnM :: a -> Mo st a
returnM x = M (\st -> (st,x))

bindM :: Mo st a -> (a -> Mo st b) -> Mo st b
bindM f g = M (\st -> case unM f st of
                        (st',x) -> unM (g x) st')

bindM_ :: Mo st a -> Mo st b -> Mo st b
bindM_ f g = f `bindM` \_ -> g

mapM :: (a -> Mo st b) -> [a] -> Mo st [b]
mapM _ [] = returnM []
mapM f (m:ms) = f m       `bindM` \m' ->
                mapM f ms `bindM` \ms' ->
                returnM (m':ms')

type M a = Mo State a

run :: State -> M a -> a
run st f = snd (unM f st)

getState :: M State
getState = M (\st -> (st, st))

putState :: State -> M ()
putState st = M (\ _ -> (st, ()))

updState :: (State -> State) -> M ()
updState f = getState `bindM` \st -> putState (f st)

getTypeMap :: M TypeMap
getTypeMap = getState `bindM` \st -> returnM (st -> typeMap)

getNextID :: M Int
getNextID = getState `bindM` \st -> returnM (st -> nextID)

setNextID :: Int -> M ()
setNextID i = updState (\st -> { nextID := i | st })

isDetMode :: M Bool
isDetMode = getState `bindM` \st -> returnM (st -> detMode)

ifDetMode :: M a -> M a -> M a
ifDetMode a b = isDetMode `bindM` \dm -> if dm then a else b

setDetMode :: Bool -> M ()
setDetMode dm = updState (\st -> { detMode := dm | st})

getJustCont :: M (Bool, QName, [VarIndex])
getJustCont = getState `bindM` \st -> returnM (fromJust (st -> cont))

getMatchPos :: M Int
getMatchPos = getState `bindM` \st -> returnM (st -> matchPos)

-- Perform an action in a given detMode and restore the original mode
-- afterwards
doInDetMode :: Bool -> M a -> M a
doInDetMode dm action =
  isDetMode `bindM` \ oldDm ->
  setDetMode dm `bindM_`
  action `bindM` \ retVal ->
  setDetMode oldDm `bindM_`
  returnM retVal

takeNextID :: M Int
takeNextID =
  getState `bindM` \st ->
  let i = st -> nextID in
  putState ({ nextID := (i+1) | st }) `bindM_`
  returnM i

takeNextIDs :: Int -> M [Int]
takeNextIDs n =
  getState `bindM` \st ->
  let i = st -> nextID in
  putState ({ nextID := (i+n) | st }) `bindM_`
  returnM [i .. i+n-1]

putTypeMap :: TypeMap -> M ()
putTypeMap m = updState (\st -> { typeMap := m | st })

updTypeMap :: (TypeMap -> TypeMap) -> M ()
updTypeMap f = getTypeMap `bindM` \m -> putTypeMap (f m)

getNDClass :: QName -> M NDClass
getNDClass qn = getState `bindM` \st ->
  returnM $ fromMaybe (error $ show qn ++ " not analysed" )
  $ (flip lookupFM) qn $ (st -> ndResult)

{-
 `bindM` \nd ->
  case nd of
    DHO -> ifDetMode (returnM DFO) (returnM DHO)
    _ -> returnM nd
-}

-- ---------------------------------------------------------------------------
-- Program transformation
-- ---------------------------------------------------------------------------

transform :: Options -> Prog -> Prog
transform opts prog = run initState (transProg prog) where
  initState = { searchMode := opts -> optSearchMode
              , detMode    := opts -> optHoDetMode
              , ndResult   := analyseNd prog
              | defaultState
              }

transProg :: Prog -> M Prog
transProg (Prog m _ ts fs _) = doInDetMode False $
  --TODO: translation of types not longer necessary
  --      only needed for insertion to the type map
  -- translation of the types
  mapM transData ts `bindM_`
  -- translation of the functions
  mapM transFunc fs `bindM` \fss ->
  -- ( filter ((`notElem` ["main_","searchTree"]) . snd . funcName) fs)
  returnM $ Prog m [prelude] [] (concat fss) []

-- Translation of Curry types to Haskell types

transData :: TypeDecl -> M TypeDecl
transData (Type qn v vs cs) =
  mapM addToMap cs      `bindM_`
  mapM transCons cs     `bindM` \ cs' ->
  newConstructors qn vs `bindM` \ new ->
  returnM $ Type qn v vs (new ++ cs')
  where addToMap c = updTypeMap (\fm -> addToFM fm (consName c) qn)
transData (TypeSyn qn v vs texpr) =
  transTypeExpr texpr `bindM` \texpr' ->
  returnM $ TypeSyn qn v vs texpr'

transCons :: ConsDecl -> M ConsDecl
transCons (Cons qn a v ts) =
  mapM transTypeExpr ts `bindM` \ts' ->
  returnM (Cons qn a v ts')

transTypeExpr :: TypeExpr -> M TypeExpr
transTypeExpr t@(TVar _) = returnM t
transTypeExpr (FuncType t1 t2) =
  transTypeExpr t1 `bindM` \t1' ->
  transTypeExpr t2 `bindM` \t2' ->
  returnM (FuncType t1' t2')
transTypeExpr (TCons qn ts) =
  mapM transTypeExpr ts `bindM` \ts' ->
  returnM (TCons qn ts')

newConstructors :: QName -> [VarIndex] -> M [ConsDecl]
newConstructors qn vs =
  transTypeExpr (TCons qn (map TVar vs)) `bindM` \t ->
  returnM [ cons (orName qn)    [tOrRef, t, t]
          , cons (failName qn)  []
          , cons (guardName qn) [tConstraint, t]
          ]

cons  n xs    = Cons n (length xs) Public xs

orName    (q, n) = (q, "Choice" +|+ n)
failName  (q, n) = (q, "Fail" +|+ n)
guardName (q, n) = (q, "Guard" +|+ n)

-- Translation of Curry functions to Haskell functions

transFunc :: FuncDecl -> M [FuncDecl]
transFunc (Func qn a v t r) =
  getNDClass qn `bindM` \ndCl ->
  case ndCl of
    DFO ->
      transPureFunc qn a v t r `bindM` \ fd ->
      returnM [fd]
    DHO ->
      doInDetMode True  (transPureFunc qn a v t r) `bindM` \fd ->
      doInDetMode False (transNDFunc qn a v t r)   `bindM` \fn ->
      returnM [fd, fn]
    ND ->
      doInDetMode  True (transNDFunc qn a v t r) `bindM` \res ->
      returnM [res]

transPureFunc qn a v t r =
  renameFun qn `bindM` \qn' ->
  transTypeExpr t `bindM` \t' ->
  transRule False qn' r `bindM` \r' ->
  returnM (Func qn' a v t' r')

transNDFunc qn a v t r =
  getNDClass qn `bindM` \ndCl ->
  renameFun qn `bindM` \qn' ->
  check42 (transFuncType ndCl a) t `bindM` \t' ->
  transRule True qn' r `bindM` \r' ->
  returnM (Func qn' (a+1) v t' r')

check42 :: (TypeExpr -> M TypeExpr) -> TypeExpr -> M TypeExpr
check42 f t = case t of
  (TVar (-42)) -> returnM t
  _            -> f t

transFuncType :: NDClass -> Int -> TypeExpr -> M TypeExpr
transFuncType nd n t = case nd of
  DFO -> transTypeExpr t
  _    -> case n of
      0     -> transHOTypeExpr t `bindM` \t' ->
               returnM (FuncType supplyType t')
      _ -> if n < 0
             then error $ "transFunctype: " ++ show (nd,n,t)
             else case t of
               (FuncType t1 t2) ->
                   transHOTypeExpr t1 `bindM` \t1' ->
                   transFuncType nd (n-1) t2 `bindM` \t2' ->
                   returnM (FuncType t1' t2')
               _ -> error $ "transFunctype: " ++ show (nd,n,t)
{-
transFuncType Pure _ t = transTypeExpr t
transFuncType _    0 t =
  transHOTypeExpr t `bindM` \t' ->
  returnM (FuncType supplyType t')
transFuncType nd (n+1) (FuncType t1 t2) =
  transHOTypeExpr t1 `bindM` \t1' ->
  transFuncType nd n t2 `bindM` \t2' ->
  returnM (FuncType t1' t2')
transFuncType n i t = error $ "transFunctype: " ++ show (n,i,t)
-}

transHOTypeExpr :: TypeExpr -> M TypeExpr
transHOTypeExpr t = case t of
  (FuncType t1 t2) ->
    transTypeExpr t1 `bindM` \t1' ->
    transHOTypeExpr t2 `bindM` \t2' ->
    returnM (funcType t1' t2')
  _ -> transTypeExpr t

transRule :: Bool -> QName -> Rule -> M Rule
transRule addArg qn (Rule vs e) =
  updState (\st -> { cont := Just (addArg, qn, vs) | st })  `bindM_`
  transBody e `bindM` \e' ->
  returnM $ Rule (if addArg then vs ++ [suppVarIdx] else vs) e'
transRule addArg qn (External s) = returnM (External s) -- error "Compile.transRule with external rule"

transBody :: Expr -> M Expr
transBody exp = case exp of
  (Case m e@(Var i) bs)->
    getJustCont `bindM` \(_,_,vs) ->
    let Just idx = find (==i) vs in
    updState (\st -> { matchPos := idx | st }) `bindM_`
    mapM transBranch bs `bindM` \bs' ->
    newBranches bs `bindM` \ns ->
    transExpr e `bindM` \(_,e') ->
    returnM (Case m e' (bs'++ns))
  _ ->
    getNextID `bindM` \i ->
    transExpr exp `bindM` \(g, e') ->
    getState `bindM_`  -- No effect ???
    getNextID `bindM_` -- Just to increase the id ???
    let e'' = case g of
                []  -> e'
                [v] -> Let [(v,Var suppVarIdx)] e' in
    setNextID i `bindM_`
    returnM e''

transBranch :: BranchExpr -> M BranchExpr
transBranch (Branch (Pattern p vs) e) =
  getNextID `bindM` \i ->
  transExpr e `bindM` \(g,e') ->
  getState `bindM_`
  getNextID `bindM_`
  let e'' = case g of
              []  -> e'
              [v] -> Let [(v,Var suppVarIdx)] e' in
  setNextID i `bindM_`
  returnM (Branch (Pattern p vs) e'')

newBranches :: [BranchExpr] -> M [BranchExpr]
newBranches (Branch (Pattern qn _) _:_) =
  getTypeMap  `bindM` \m ->
  let qnMatch = fromMaybe (error $ "not in type map "++show qn)
                (lookupFM m qn) in
  getJustCont `bindM` \(addArg, qn', vs) ->
  getMatchPos `bindM` \pos ->
  let is = if addArg then [Var suppVarIdx] else []
      (vs1,_:vs2) = break (==pos) vs
      c e = funcCall qn' (map Var vs1 ++ [e] ++ map Var vs2 ++ is) in
  returnM [Branch (pcons (orName qnMatch) [1000,1001,1002])
                 (liftOr [Var 1000,c (Var 1001),c (Var 1002)]),
          Branch (pcons (guardName qnMatch) [1000,1001])
                 (liftGuard [Var 1000,c (Var 1001)]),
          Branch (pcons ("","_") [])
                 liftFail
         ] -- TODO Magic numbers?

transExpr :: Expr -> M ([VarIndex],Expr)
transExpr e@(Var _) = returnM ([], e)
transExpr (Lit (Intc i)) = returnM ([], int i)
transExpr (Lit (Charc c)) = returnM ([], char c)
transExpr (Comb ConsCall qn es) =
  mapM transExpr es `bindM` unzipArgs `bindM` \(g,es') ->
  genIds g (Comb ConsCall qn es')
transExpr (Comb FuncCall qn es) =
  getNDClass qn `bindM` \ndCl ->
  renameFun qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g,es') ->
  case ndCl of
    -- TODO non-determinism?
    DFO -> genIds g (Comb FuncCall qn' es')
    _    -> takeNextID  `bindM` \i ->
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
transExpr (Let vses e) =
  let (vs,es) = unzip vses in
  mapM transExpr es `bindM` unzipArgs  `bindM` \(g,es') ->
  transExpr e `bindM` \(ge,e') ->
  genIds (g ++ ge) (Let (zip vs es') e')
transExpr (Or e1 e2) = transExpr (Comb FuncCall (prelude,"?") [e1,e2])
transExpr (Free vs e) =
  transExpr e `bindM` \(g,e') ->
  takeNextIDs (length vs) `bindM` \is ->
  genIds (g++is) (Let (zipWith (\ v i -> (v,generate (Var i))) vs is) e')
transExpr e@(Case _ _ _) = returnM ([], e)
transExpr e@(Comb (ConsPartCall _) _ _) = returnM ([], e)

renameFun :: QName -> M QName
renameFun qn@(q, n) =
  isDetMode `bindM` \dm ->
  getNDClass qn `bindM` \ndCl ->
  returnM (q, (detPrefix $ dm && ndCl == DHO) ++ n)

funcType :: TypeExpr -> TypeExpr -> TypeExpr
funcType t1 t2 = TCons (prelude, "Func") [t1,t2]

s +|+ t = s ++ "_" ++ t

unzipArgs :: [([VarIndex], e)] -> M ([VarIndex], [e])
unzipArgs ises = let (is,es) = unzip ises in returnM (concat is,es)

genIds :: [VarIndex] -> Expr -> M ([VarIndex],Expr)
genIds ns e =
    getNextID `bindM` \i ->
    let (v',vs) = mkSplits i ns in
    case vs of
        [] -> returnM (ns, e)
        _  -> setNextID (v'+1) `bindM_` returnM ([v'], foldr addSplit e vs)
 where
   addSplit (v,v1,v2) e' = Let [(v1, fstSplit v)] (Let [(v2, sndSplit v)] e')
   fstSplit v = funCall "leftSupply"  [Var v]
   sndSplit v = funCall "rightSupply" [Var v]

-- TODO magic numbers
idVar      = 2000
suppVarIdx = 3000

tOrRef = TCons (prelude,"ID") []
tConstraint = TCons (prelude,"Constraint") []
supplyType = TCons (prelude,"IDSupply") []
liftOr = funcCall (prelude,"narrow")
liftGuard = funcCall (prelude,"guardCons")
liftFail = funcCall (prelude,"failCons") []
prelude = "Prelude"
splitSupply = funcCall (prelude,"splitSupply")
initSupply  = funcCall (prelude,"initIDSupply") []

pcons n xs    = Pattern n xs
tcons n = TCons ("", n)
consCall n xs = Comb ConsCall n xs
funcCall n xs = Comb FuncCall n xs
funCall n = funcCall ("", n)


int :: Int -> Expr
int i = Comb ConsCall (prelude, "C_Int") [Lit (Intc i)]

char :: Char -> Expr
char c = Comb ConsCall (prelude, "C_Char") [Lit (Charc c)]

-- Wrap a function with to a Func type
wrap :: Bool -> NDClass -> Int -> Expr -> Expr
wrap True _ _ e = e
wrap False nd a e = wrap'' (fun 1 (wrapName nd) []) e a

wrap' nd n e = case n of
  0 -> e
  1 -> funcCall (wrapName nd) [e]
  _ -> funcCall (wrapName DFO) [wrap' nd (n-1) e]
{-
wrap' _  0     e = e
wrap' nd 1     e = funcCall (wrapName nd) [e]
wrap' nd (n+1) e = funcCall (wrapName Pure) [wrap' nd n e]
-}

wrapName :: NDClass -> QName
wrapName ndMode = case ndMode of
  DFO -> ("", "wrapD")
  _    -> ("", "wrapN")

wrap'' f e n = if n == 0 then e else apply f (wrap'' (point [f]) e (n-1))
{-
wrap'' _ e 0     = e
wrap'' f e (n+1) = apply f (wrap'' (point [f]) e n)
-}

fun :: Int -> QName -> [Expr] -> Expr
fun i n xs | length xs==i = Comb FuncCall n xs
           | otherwise    = Comb (FuncPartCall (length xs - i)) n xs

point :: [Expr] -> Expr
point = fun 2 ("", ".")

apply (Comb (FuncPartCall i) qn xs) e =
  Comb (if i==1 then FuncCall else FuncPartCall (i-1)) qn (xs++[e])

bind e1 e2 = funCall ">>=" [e1, e2]
prinT0 = Comb (FuncPartCall 1) ("","print") []
prinT e = funCall "print" [e]
e1 .* e2 = funCall "." [e1,e2]

dfs0 = Comb (FuncPartCall 1) ("","dfs") []
bfs0 = Comb (FuncPartCall 1) ("","bfs") []
par0 = Comb (FuncPartCall 1) ("","par") []
idfs0 g = Comb FuncCall  ("","idfs") [g]
generate i = Comb FuncCall  ("","generate") [i]

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
