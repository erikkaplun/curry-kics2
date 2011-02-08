module Compile where

import Prelude hiding (lookup)
import FiniteMap hiding (mapFM, filterFM)
import Maybe (fromJust)
import List (intersperse, find)
import Unsafe (trace)
import FileGoodies

import FlatCurry
import FlatCurryGoodies (funcName, consName)

import CallGraph
import LiftCase (liftCases)
import PrettyND (prettyNd)
import Splits
import SearchMode
import CompilerOpts

traceM :: String -> M ()
traceM s = trace (s ++ "\n") $ returnM ()

info :: Options -> String -> IO ()
info opts msg = if opts -> quiet then return () else putStrLn msg

main :: IO ()
main = do
  (opts, files) <- compilerOpts
  compile opts files

compile :: Options -> [String] -> IO ()
compile opts (fn:_) = do
  info opts "Reading FlatCurry ..."
  prog <- readFlatCurry fn

  info opts "Lifting case expressions ..."
  let pLifted = liftCases True prog

  info opts "Transforming program ..."
  let p' = run initState (transProg pLifted)

  info opts "Generating Haskell module ..."
  hsProg <- prettyNd p'

  info opts $ "writing to file " ++ destFile ++ "..."
  writeFile destFile hsProg

    where
      destFile = dir ++ separatorChar : "C_" ++ basename ++ ".hs"
      dir = dirName fn
      basename = stripSuffix (baseName fn)
      initState = setSearchMode (opts -> searchMode) $ setDetMode (opts -> hoOpt) def

data Mo st a = M (st -> (st, a))

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

type TypeMap = FM QName QName

data State = State TypeMap (Maybe (Bool,QName,[VarIndex])) Int NDResult VarIndex Bool SearchMode

setTypeMap    :: TypeMap -> State -> State
setTypeMap t (State _ m i n v b s) = State t m i n v b s

setCont       :: Maybe (Bool,QName,[VarIndex]) -> State -> State
setCont m (State t _ i n v b s) = State t m i n v b s

setMatchPos   :: Int -> State -> State
setMatchPos i (State t m _ n v b s) = State t m i n v b s

setNdResult   :: NDResult -> State -> State
setNdResult n (State t m i _ v b s) = State t m i n v b s

setNextID     :: VarIndex -> State -> State
setNextID v (State t m i n _ b s) = State t m i n v b s

setDetMode    :: Bool -> State -> State
setDetMode b (State t m i n v _ s) = State t m i n v b s

setSearchMode :: SearchMode -> State -> State
setSearchMode s (State t m i n v b _) = State t m i n v b s

typeMap    :: State -> TypeMap
typeMap (State t _ _ _ _ _ _) = t

cont       :: State -> Maybe (Bool,QName,[VarIndex])
cont (State _ m _ _ _ _ _) = m

matchPos   :: State -> Int
matchPos (State _ _ i _ _ _ _) = i

ndResult   :: State -> NDResult
ndResult (State _ _ _ n _ _ _) = n

nextID     :: State -> VarIndex
nextID (State _ _ _ _ v _ _) = v

detMode    :: State -> Bool
detMode (State _ _ _ _ _ b _) = b

searchMode :: State -> SearchMode
searchMode (State _ _ _ _ _ _ s) = s

primTypes :: [(QName,QName)]
primTypes = map (\ (x,y) -> ((prelude,x),(prelude,y)))
                [("True","Bool"),("False","Bool")
                ,("[]","List"),(":","List"),("(,)","T2"),("(,,)","T3")]
def :: State
def = State (listToFM (<) primTypes) Nothing (-1) (emptyFM (<)) idVar False NoSearch

getState :: M State
getState = M (\st -> (st,st))

putState :: State -> M ()
putState st = M (\ _ -> (st,()))

updState :: (State -> State) -> M ()
updState f = getState `bindM` \st -> putState (f st)

getTypeMap :: M TypeMap
getTypeMap = getState `bindM` (returnM . typeMap)

getNextID :: M Int
getNextID = getState `bindM` (returnM . nextID)

isDetMode :: M Bool
isDetMode = getState `bindM` (returnM . detMode)

inDetMode :: M a -> M a -> M a
inDetMode a b = isDetMode `bindM` \dm -> if dm then a else b

setDetModeM :: Bool -> M ()
setDetModeM dm = updState (\st -> setDetMode dm st)

takeNextID :: M Int
takeNextID =
  getState `bindM` \st ->
  let i = nextID st in
  putState (setNextID (i+1) st) `bindM_`
  returnM i

takeNextIDs :: Int -> M [Int]
takeNextIDs n =
  getState `bindM` \st ->
  let i = nextID st in
  putState (setNextID (i+n) st) `bindM_`
  returnM [i .. i+n-1]


putTypeMap :: TypeMap -> M ()
putTypeMap m = getState `bindM` \st -> putState (setTypeMap m st)

updTypeMap :: (TypeMap -> TypeMap) -> M ()
updTypeMap f =
  getTypeMap `bindM` \m ->
  putTypeMap (f m)

getNDClass :: QName -> M NDClass
getNDClass qn = getState `bindM`
  (returnM . maybe (error $ show qn ++ " not analysed" ) id . (flip lookupFM) qn . ndResult ) `bindM` \nd ->
  case nd of
    HO -> isDetMode `bindM` \dm -> returnM $ if dm then Pure else HO
    _ -> returnM nd

type M a = Mo State a

run :: State -> M a -> a
run st f = snd (unM f st)

transProg :: Prog -> M Prog
transProg p@(Prog _ _ ts fs _) =
  isDetMode  `bindM` \dm ->
  setDetModeM False  `bindM_`
  updState (setNdResult (analyse p)) `bindM_`
  mapM transData ts `bindM` \ts' ->
  mapM transFunc (filter ((\ x -> not (elem x ["main_","searchTree"])) . snd . funcName) fs)  `bindM` \fss ->
  let fs0 = concat fss in
  setDetModeM dm `bindM_`
  returnM (Prog "Main"
               ["GHC.Prim",
                "Data.IORef",
                "qualified Data.Map",
                "qualified Data.List",
                "Data.DeriveTH",
                "System.IO.Unsafe",
                "Control.Monad",
                "Control.Parallel.TreeSearch",
                "qualified Data.Foldable as Fold",
                "qualified Control.Monad.SearchTree as ST",
                "qualified Data.FMList as FM"]
               ts'
               fs0
               [])

{-
addPrintGoal :: String -> [FuncDecl] -> Int -> M [FuncDecl]
addPrintGoal m fs n =
 getState `bindM`
 (returnM . searchMode) `bindM` \search ->
 let qn = (m,"goal" ++ show n) in
 getNDClass qn `bindM` \ndCl ->
 renameFun qn `bindM` \qn' ->
 let body nd = case nd of
       Pure -> prinT (funcCall qn' [])
       _    -> if search==IterDFS
                 then idfs0 (fun 1 qn' [])
                 else bind initSupply (addSearch search .* funcCall qn' [])

     addSearch NoSearch = prinT0
     addSearch DFS      = dfs0
     addSearch BFS      = bfs0
     addSearch PAR      = par0
     f = Func ("","main") 0 Public (tcons "IO" [tcons "()" []])
              (Rule [] (body ndCl))
 in returnM (f : fs)
-}

transData :: TypeDecl -> M TypeDecl
transData (Type qn v vs cs) =
  mapM addToMap cs      `bindM_`
  renameType qn         `bindM` \ qn' ->
  mapM transCons cs     `bindM` \ cs' ->
  newConstructors qn vs `bindM` \ new ->
  returnM (Type qn' v vs (new++cs'))
  where
    addToMap c = updTypeMap (\fm -> addToFM fm (consName c) qn)

renameType, renameCons, renameFun :: QName -> M QName
renameType (_, n) = case n of
  "[]" -> renameType ("","List")
  "(,)" -> renameType ("","T2")
  "(,,)" -> renameType ("","T3")
  _ -> returnM ("","C_"++n)

renameCons x = case x of
 (_,":") -> renameType ("","Cons")
 (_,"[]") -> renameType ("","Nil")
 (_,"(,)") -> renameType ("","T2")
 (_,"(,,)") -> renameType ("","T3")
 _ -> renameType x

renameFun qn@(_,n) =
  isDetMode `bindM` \dm ->
  setDetModeM False `bindM_`
  getNDClass qn `bindM` \ndCl ->
  setDetModeM dm `bindM_`
  let prefix s = (if dm && ndCl == HO then "d_" else "c_") ++ s in
  if isInfixName n
   then returnM ("",prefix $ "op_"
                     ++ (concat $ intersperse "_" $ map nameOfChar n))
   else returnM ("",prefix $ concatMap nameOfChar n)

isInfixName = all (`elem` "?!#$%^&*+=-<>.:/\\|")

nameOfChar c = case c of
  '<' -> "lt"
  '-' -> "minus"
  '+' -> "plus"
  '*' -> "mult"
  '!' -> "bang"
  '=' -> "eq"
  '/' -> "slash"
  '?' -> "qmark"
  '&' -> "and"
  '>' -> "gt"
  ':' -> "colon"
  '.' -> "point"
  '#' -> "hash"
  '|' -> "bar"
  c'   -> [c'] --error $ "rename char " ++ [c']

transCons :: ConsDecl -> M ConsDecl
transCons (Cons qn a v ts) =
  renameCons qn `bindM` \qn' ->
  mapM transTypeExpr ts `bindM` \ts' ->
  returnM (Cons qn' a v ts')

check42 :: (TypeExpr -> M TypeExpr) -> TypeExpr -> M TypeExpr
check42 f t = case t of
  (TVar n) -> if n == -42 then returnM t else f t
  _        -> f t

transTypeExpr :: TypeExpr -> M TypeExpr
transTypeExpr (TCons qn ts) =
  renameType qn `bindM` \qn' ->
  mapM transTypeExpr ts `bindM` \ts' ->
  returnM (TCons qn' ts')
transTypeExpr (FuncType t1 t2) =
  transTypeExpr t1 `bindM` \t1' ->
  transTypeExpr t2 `bindM` \t2' ->
  returnM (FuncType t1' t2')
transTypeExpr t@(TVar _) = returnM t

transFuncType :: NDClass -> Int -> TypeExpr -> M TypeExpr

transFuncType nd n t = case nd of
  Pure -> transTypeExpr t
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

funcType :: TypeExpr -> TypeExpr -> TypeExpr
funcType t1 t2 = TCons ("","Func") [t1,t2]

newConstructors :: QName -> [VarIndex] -> M [ConsDecl]
newConstructors qn@(_,_) vs =
  let tvs = map TVar vs in
  transTypeExpr (TCons qn tvs) `bindM` \t ->
  returnM [cons (orName qn) [tOrRef,t,t],
          cons (failName qn) [],
          cons (guardName qn) [tConstraint,t]]


cons  n xs    = Cons n (length xs) Public xs
pcons n xs    = Pattern n xs
tcons n = TCons ("",n)
consCall n xs = Comb ConsCall n xs
funcCall n xs = Comb FuncCall n xs
funCall n = funcCall ("",n)


s +|+ t = s ++ "_" ++ t

transFunc :: FuncDecl -> M [FuncDecl]
transFunc (Func qn@(_,n) a v t r) =
  getNDClass qn `bindM` \ndCl ->
  traceM (n ++ " is " ++ (show ndCl)) `bindM_`
  case ndCl of
    Pure -> transPureFunc qn a v t r `bindM` (returnM . (:[]))
    HO   ->
      setDetModeM True  `bindM_`
      transPureFunc qn a v t r `bindM` \fd ->
      setDetModeM False `bindM_`
      transNDFunc   qn a v t r `bindM` \fn ->
      returnM [fd,fn]
    ND ->
      setDetModeM True `bindM_`
      transNDFunc   qn a v t r  `bindM` \res ->
      setDetModeM False `bindM_`
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

transRule :: Bool -> QName -> Rule -> M Rule
transRule addArg qn (Rule vs e) =
  updState (setCont (Just (addArg,qn,vs)))  `bindM_`
  transBody e `bindM` \e' ->
  returnM (Rule (if addArg then vs++[suppVarIdx] else vs) e')

transBody :: Expr -> M Expr
transBody exp = case exp of
    (Case m e@(Var i) bs)->
      getState `bindM` (returnM . fromJust . cont)  `bindM` \(_,_,vs) ->
      let Just idx = find (==i) vs in
      updState (setMatchPos idx) `bindM_`
      mapM transBranch bs `bindM` \bs' ->
      newBranches bs `bindM` \ns ->
      transExpr e `bindM` \(_,e') ->
      returnM (Case m e' (bs'++ns))
    _ ->
      getNextID `bindM` \i ->
      transExpr exp `bindM` \(g,e') ->
      getState `bindM_`
      getNextID `bindM_`
      let e'' = case g of
                  []  -> e'
                  [v] -> Let [(v,Var suppVarIdx)] e' in
      updState (setNextID i)  `bindM_`
      returnM e''

transBranch :: BranchExpr -> M BranchExpr
transBranch (Branch (Pattern p vs) e) =
  getNextID `bindM` \i ->
  renameCons p `bindM` \p' ->
  transExpr e `bindM` \(g,e') ->
  getState `bindM_`
  getNextID `bindM_`
  let e'' = case g of
              []  -> e'
              [v] -> Let [(v,Var suppVarIdx)] e' in
  updState (setNextID i) `bindM_`
  returnM (Branch (Pattern p' vs) e'')

newBranches :: [BranchExpr] -> M [BranchExpr]
newBranches (Branch (Pattern qn _) _:_) =
  getTypeMap  `bindM` \m ->
  let qnMatch = maybe (error $ "not in type map "++show qn) id (lookupFM m qn) in
  getState `bindM` (returnM . fromJust . cont) `bindM` \(addArg,qn',vs) ->
  getState `bindM` (returnM . matchPos) `bindM` \pos ->
  let is = if addArg then [Var suppVarIdx] else []
      (vs1,_:vs2) = break (==pos) vs
      c e = funcCall qn' (map Var vs1 ++ [e] ++ map Var vs2 ++ is) in
  returnM [Branch (pcons (orName qnMatch) [1000,1001,1002])
                 (liftOr [Var 1000,c (Var 1001),c (Var 1002)]),
          Branch (pcons (guardName qnMatch) [1000,1001])
                 (liftGuard [Var 1000,c (Var 1001)]),
          Branch (pcons ("","__") [])
                 liftFail
         ] -- TODO Magic numbers?

transExpr :: Expr -> M ([VarIndex],Expr)

transExpr e@(Var _) = returnM ([],e)
transExpr (Lit (Intc i)) = returnM ([],int i)

transExpr (Comb ConsCall qn es) =
  renameCons qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g,es') ->
  genIds g (Comb ConsCall qn' es')

transExpr (Comb FuncCall qn es) =
  getNDClass qn `bindM` \ndCl ->
  renameFun qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g,es') ->
  case ndCl of
    -- TODO non-determinism?
    Pure -> genIds g (Comb FuncCall qn' es')
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

transExpr (Or e1 e2) = transExpr (Comb FuncCall ("Prelude","?") [e1,e2])

transExpr (Free vs e) =
  transExpr e `bindM` \(g,e') ->
  takeNextIDs (length vs) `bindM` \is ->
  genIds (g++is) (Let (zipWith (\ v i -> (v,generate (Var i))) vs is) e')

transExpr e@(Case _ _ _) = returnM ([],e)
transExpr e@(Comb (ConsPartCall _) _ _) = returnM ([],e)
  -- trace ("other: "++show e) $ return ()


unzipArgs :: [([VarIndex],e)] -> M ([VarIndex],[e])
unzipArgs ises = let (is,es) = unzip ises in returnM (concat is,es)

genIds :: [VarIndex] -> Expr -> M ([VarIndex],Expr)
genIds ns e =
    getNextID  `bindM` \i ->
    let (v',vs) = mkSplits i ns in
    case vs of
        [] -> returnM (ns,e)
        _  -> updState (setNextID (v'+1))  `bindM_` returnM ([v'],foldr addSplit e vs)
 where
   addSplit (v,v1,v2) e' =
     Let [(v1,fstSplit v)] (Let [(v2,sndSplit v)] e')
   fstSplit v = funCall "leftSupply"  [Var v]
   sndSplit v = funCall "rightSupply" [Var v]

idVar      = 2000
suppVarIdx = 3000



orName (_,n) = ("",n +|+ "Choice")
failName (_,n) = ("",n +|+ "Fail")
guardName (_,n) = ("",n +|+ "Guard")

tOrRef = TCons ("","ID") []
tConstraint = TCons ("","Constraint") []
supplyType = TCons ("","IDSupply") []
liftOr = funcCall ("","narrow")
liftGuard = funcCall ("","guardCons")
liftFail = funcCall ("","failCons") []
prelude = "Prelude"
splitSupply = funcCall ("","splitSupply")
initSupply  = funcCall ("","initIDSupply") []

int :: Integer -> Expr
int i = consCall ("","(C_Int " ++ show i ++ "#)") []

wrap :: Bool -> NDClass -> Int -> Expr -> Expr
wrap True _ _ e = e
wrap False nd a e = wrap'' (fun 1 (wrapName nd) []) e a

{-
wrap' _  0     e = e
wrap' nd 1     e = funcCall (wrapName nd) [e]
wrap' nd (n+1) e = funcCall (wrapName Pure) [wrap' nd n e]
-}

wrap' nd n e = case n of
  0 -> e
  1 -> funcCall (wrapName nd) [e]
  _ -> funcCall (wrapName Pure) [wrap' nd (n-1) e]

wrapName ndMode = case ndMode of
                    Pure -> ("","wrapD")
                    _    -> ("","wrapN")

{-
wrap'' _ e 0     = e
wrap'' f e (n+1) = apply f (wrap'' (point [f]) e n)
-}

wrap'' f e n = if n == 0 then e else apply f (wrap'' (point [f]) e (n-1))

point = fun 2 ("",".")

fun i n xs | length xs==i = Comb FuncCall n xs
           | otherwise    = Comb (FuncPartCall (length xs - i)) n xs

apply (Comb (FuncPartCall i) qn xs) e =
  Comb (if i==1 then FuncCall else FuncPartCall (i-1)) qn (xs++[e])

bind e1 e2 = funCall ">>=" [e1,e2]
prinT0 = Comb (FuncPartCall 1) ("","print") []
prinT e = funCall "print" [e]
e1 .* e2 = funCall "." [e1,e2]

dfs0 = Comb (FuncPartCall 1) ("","dfs") []
bfs0 = Comb (FuncPartCall 1) ("","bfs") []
par0 = Comb (FuncPartCall 1) ("","par") []
idfs0 g = Comb FuncCall  ("","idfs") [g]
generate i = Comb FuncCall  ("","generate") [i]
