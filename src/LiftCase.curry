------------------------------------------------------------------
--- This module provides a transformation on FlatCurry programs.
--- The result contains a new function for each nested case 
--- contained in the original program. 
--- The new functions have the names  "_case_ ... i" where i
--- is a number and there are as many underscores as necessary 
--- to avoid name clashes. (Example: _case_0)
--- In addition, all the cases are normalized, i.e. applied to 
--- a variable. This variable is always the last argument of 
--- the newly generated function.
--- The first argument of liftCases is a boolean flag, meaning:
--- False: introduce function for EACH case of original program
--- True:  introduce function for nested and not normalized 
---        cases only
---
--- Some possible improvements:
--- 1) Up to now the newly generated functions are not correctly 
---    typed but have type (TVar (-42)).
---    (done, but untested. hsi)
---
--- @author  bbr,sebf
--- @version October 2006
------------------------------------------------------------------

module LiftCase (liftCases,isCaseAuxFuncName,isCaseAuxFuncType) where

import FlatCurry
import FlatCurryGoodies
import List (isPrefixOf,partition,delete)
import FiniteMap

isCaseAuxFuncName :: String -> Bool
isCaseAuxFuncName n = take 6 n == "_case_"

isCaseAuxFuncType :: TypeExpr -> Bool
isCaseAuxFuncType t = t==TVar (-42)

--- @param - the program to be transformed
liftCases :: Bool -> Prog -> Prog
liftCases nestedOnly p = 
  let fs  = progFuncs p
      aux = genAuxName (map (snd . funcName) fs)
      (exts,ins) = partition isExternal fs
      (newFsf,_,auxFf) = foldr (liftCasesFunc nestedOnly (progName p) aux) 
                               (id,0,id) 
                               ins
   in updProgFuncs (const (newFsf (auxFf exts))) p

type FuncList = [FuncDecl] -> [FuncDecl]
type Result = (FuncList,Int,FuncList)

type M x = Int -> (x, Int, FuncList, [VarIndex])


liftCasesFunc :: Bool -> String -> String -> FuncDecl -> Result -> Result
liftCasesFunc onlyNested mod aux f (esMain,i0,ffMain) = 
  ((updFuncBody (const exp) f:) . esMain,iMain,ffMain . ffeMain)
  where
    body = funcBody f

    (exp,iMain,ffeMain,_) = 
     if onlyNested then (case body of
      Case cm (Var v) bs -> 
         let (e',i',ffe,_)    = trans (Var v) i0
             (bs',i'',ffbs,_) = 
               sequence (map (\ (Branch pat be) -> branch pat (trans be)) bs) i'
          in (Case cm e' bs', i'',ffe . ffbs,[])
      _            -> trans body i0)
     else trans body i0
           
    trans = trExpr var lit comb leT freE or casE branch typed

    var :: VarIndex -> M Expr
    var v i = (Var v,i,id,[v])

    lit :: Literal -> M Expr
    lit l i = (Lit l,i,id,[])

    comb :: CombType -> QName -> [M Expr] -> M Expr
    comb ct n args i = let (args',i',ff,vs) = sequence args i
      in (Comb ct n args',i',ff,vs)

    leT :: [(VarIndex,M Expr)] -> M Expr -> M Expr
    leT bs e i = 
      let (vs,es)  = unzip bs 
          (es',i',ffes,ves) = sequence es i
          (e',i'',ffe,ve) = e i'
      in (Let (zip vs es') e',i'', ffes . ffe,
          filter (not . elemOf vs) (ves ++ ve))

    freE :: [VarIndex] -> M Expr -> M Expr
    freE vs e i = 
      let (e',i',ff,ve) = e i 
       in (Free vs e',i',ff,filter (not . elemOf vs) ve)

    or :: M Expr -> M Expr -> M Expr
    or e1 e2 i = 
      let ([e1',e2'],i',ff,vs) = sequence [e1,e2] i
       in (Or e1' e2',i',ff,vs)

    casE :: CaseType -> M Expr -> [M BranchExpr] -> M Expr
    casE ct e bs i = 
      let (e',i',ffe,ve)     = e i
          (bs',i'',ffbs,vbs) = sequence bs i'
          envRes :: [VarIndex]
          envRes = nub (ve  ++ vbs)
          env = case e' of
                  Var v -> delete v envRes
                  _     -> envRes
       in (genFuncCall mod aux i'' env e',i''+1,
           (genFunc mod aux i'' env e' ct bs':) . ffe . ffbs,
           envRes)

    branch :: Pattern -> M Expr -> M BranchExpr
    branch p e i = 
      let (e',i',ff,ve) = e i
       in (Branch p e',i',ff,removePVars ve p)

    typed :: M Expr -> TypeExpr -> M Expr
    typed e ty i = let (e',i',ff,ve) = e i in (Typed e' ty,i',ff,ve)

sequence :: [M c] -> M [c]
sequence l i = foldr once ([],i,id,[]) l
  where
    once f (es,j,ff1,vs1) = let (e,k,ff2,vs2) = f j
                             in (e:es,k,ff1 . ff2,vs1++vs2)

genFuncCall :: String -> String -> Int -> [VarIndex] -> Expr -> Expr
genFuncCall mod aux i env e = 
  Comb FuncCall (newName mod aux i) (map Var env ++ [e])

genFunc :: String -> String -> Int -> [VarIndex] -> Expr ->
           CaseType -> [BranchExpr] -> FuncDecl
genFunc mod aux i env e ct bs = 
  Func (newName mod aux i) (length env+1) Private funtype $
       Rule args rhs
  where
    v = case e of 
         Var idx -> idx
         _       -> nextLocalName (env++concatMap allVarsBranch bs)
    
    allVarsBranch (Branch p pe) = 
       trPattern (\ _ xs -> xs) (\ _ -> []) p ++ allVars pe

    args = env ++ [v]
    rhs = Case ct (Var v) bs

    funtype = TVar (-42)
              

    
removePVars :: [VarIndex] -> Pattern -> [VarIndex]
removePVars e = trPattern (\ _ vs -> filter (not . elemOf vs) e) (const e)

genAuxName :: [String] -> String
genAuxName = foldl addUnderscores "_case_"

addUnderscores :: String -> String -> String
addUnderscores n m = if isPrefixOf n m then addUnderscores (n++"_") m else n     

elemOf = flip elem

nub :: [VarIndex] -> [VarIndex]
nub xs = map fst $ fmToList $ listToFM (<) $ map (\ i -> (i,())) xs

newName :: String -> String -> Int -> QName
newName m l i = (m,l ++ show i)

nextLocalName :: [VarIndex] -> VarIndex
nextLocalName vs =  (foldr max 0 vs + 1)