------------------------------------------------------------------------------
--- A pretty printer for AbstractHaskell programs.
---
--- This library defines a function "showProg" that shows
--- an AbstractHaskell program in standard Haskell/Curry syntax.
---
--- @author Martin Engelke, Bernd Brassel, Michael Hanus, Marion Mueller,
---         Parissa Sadeghi
--- @version February 2011
------------------------------------------------------------------------------

module AbstractHaskellPrinter(showProg,
                              showTypeDecls,
                              showTypeDecl,
                              showTypeExpr,
                              showFuncDecl,
                              showExpr, showPattern) where

import AbstractHaskell
import List
import Read(readNat)
import Char(isDigit)
import FiniteMap
import Sort (cmpString)
import Maybe (isJust)

-------------------------------------------------------------------------------
-- Functions to print an AbstractHaskell program in standard Curry syntax
-------------------------------------------------------------------------------

--- Shows an AbstractHaskell program in standard Curry syntax.
--- The export list contains the public functions and the
--- types with their data constructors (if all data constructors are public),
--- otherwise only the type constructors.
--- The potential comments in function declarations are formatted as
--- documentation comments.
showProg :: Prog -> String
showProg (Prog m imports typedecls funcdecls opdecls) =
  let exports = showExports typedecls funcdecls in
  "module "++m 
  ++ (if exports=="" then "" else " ("++exports++")")
  ++ " where\n\n"
  ++ showImports imports
  ++ showOpDecls opdecls
  ++ showTypeDecls typedecls
  ++ prefixInter (showFuncDeclOpt (nameFM funcdecls,m)) funcdecls "\n\n"
  ++ "\n"

type NameFM = FM String ()
type Options = (NameFM,String)


defaultOptions :: Options
defaultOptions = (emptyFM lessString,"")


showExports :: [TypeDecl] -> [FuncDecl] -> String
showExports types funcs = 
  let publicTypes = filter isPublicType types
      (withCons, withoutCons) = partition allPublicCons publicTypes
  in
  concat 
    (intersperse ", " 
      (map ((++"(..)") . getTypeName) withCons
       ++ map getTypeName withoutCons
       ++ map getFuncName (filter isPublicFunc funcs)))
  where
    isPublicType :: TypeDecl -> Bool
    isPublicType (Type _ visibility _ _) = visibility==Public 
    isPublicType (TypeSyn _ visibility _ _) = visibility==Public 
    isPublicType (Instance _ _ _ _) = False

    isPublicFunc :: FuncDecl -> Bool
    isPublicFunc (Func _ _ _ visibility _ _) = visibility==Public 

    getTypeName :: TypeDecl -> String
    getTypeName (Type (_,name) _ _ _) = name
    getTypeName (TypeSyn (_,name) _ _ _) = name
    getTypeName (Instance (_,name) _ _ _) = name

    allPublicCons :: TypeDecl -> Bool
    allPublicCons (Type _ _ _ c) = length (filter isPublicCons c) == length c 
      where isPublicCons (Cons _ _ visibility _) = visibility==Public
    allPublicCons (TypeSyn _ _ _ _) = False
    allPublicCons (Instance _ _ _ _) = False

    getFuncName :: FuncDecl -> String
    getFuncName (Func _ (_,name) _ _ _ _) =
        if isInfixOpName name then "("++name++")" else name


showImports :: [String] -> String
showImports imports =
  prefixInter showImport (filter (/=prelude) imports) "\n" ++
  (if imports==[prelude] then "" else "\n\n")

showImport :: String -> String
showImport imp = if imp /= prelude then "import " ++ imp else ""

showOpDecls :: [OpDecl] -> String
showOpDecls opdecls =
  prefixInter showOpDecl opdecls "\n" ++
  (if opdecls == [] then "" else "\n\n")

showOpDecl :: OpDecl -> String
showOpDecl (Op (_,name) fixity precedence) =
  showFixity fixity ++ " " ++ show precedence ++ " " ++
  if isInfixOpName name then name else '`':name++"`"

showFixity :: Fixity -> String
showFixity InfixOp  = "infix"
showFixity InfixlOp = "infixl"
showFixity InfixrOp = "infixr"

--- Shows a list of AbstractHaskell type declarations in standard Curry syntax.
showTypeDecls :: [TypeDecl] -> String
showTypeDecls typedecls =
  prefixInter showTypeDecl typedecls "\n\n" ++
  (if typedecls == [] then "" else "\n\n")

--- Shows an AbstractHaskell type declaration in standard Curry syntax.
showTypeDecl :: TypeDecl -> String
showTypeDecl (TypeSyn (_,name) _ indexlist typeexpr)
   = "type " ++ name
             ++ (prefixMap (showTypeExpr False) (map TVar indexlist) " ")
     ++ " = " ++ showTypeExpr False typeexpr
showTypeDecl (Type (_,name) _ indexlist consdecls)
   = "data " ++ name
             ++ (prefixMap (showTypeExpr False) (map TVar indexlist) " ")
     ++ "\n"++showBlock ("= "++(combineMap showConsDecl consdecls "\n| "))
showTypeDecl (Instance (_,name) texp ctxts rules)
   = "instance " ++ showContext ctxts ++ name ++ " " ++ showTypeExpr True texp
     ++ " where\n" ++ concatMap showInstRule rules
 where
  showInstRule ((_,mname),rule) =
    "  " ++ mname ++ showRule defaultOptions rule ++ "\n"

showContext :: [Context] -> String
showContext [] = ""
showContext [ctxt] = showClass ctxt ++ " => "
showContext cs@(_:_:_) =
  "(" ++ concat (intersperse "," (map showClass cs)) ++ ") => "
   
showClass (Context qn tvars) = showTypeExpr False (TCons qn (map TVar tvars))
   
showConsDecl :: ConsDecl -> String
showConsDecl (Cons (_,name) _ _ typelist)
   = name ++ (prefixMap (showTypeExpr True) typelist " ")

--- Shows an AbstractHaskell type expression in standard Curry syntax.
--- If the first argument is True, the type expression is enclosed
--- in brackets.
showTypeExpr :: Bool -> TypeExpr -> String
showTypeExpr _ (TVar (_,name)) = showTypeVar (showIdentifier name)
showTypeExpr nested (FuncType domain range) =
   maybeShowBrackets nested (showTypeExpr (isFuncType domain) domain ++
                             " -> " ++ showTypeExpr False range)
       
showTypeExpr nested (TCons (mod,name) typelist)
   | mod==prelude && name == "untyped" = "-"
   | otherwise  = maybeShowBrackets (nested && not (null typelist))
                                    (showTypeCons mod name typelist)

-- Show a1,a2,a3 as a_1,a_2,a_3 (due to bug in PAKCS front-end):
showTypeVar (c:cs) =
  if c=='a' && not (null cs) && all isDigit cs
  then c:'_':cs
  else c:cs

-- Remove characters '<' and '>' from identifiers sind these characters
-- are sometimes introduced in new identifiers generated by the front end (for sections)
showIdentifier :: String -> String
showIdentifier = filter (not . flip elem "<>")

isFuncType t = case t of
                  FuncType _ _ -> True
                  _ -> False

--- Shows an AbstractHaskell function declaration in standard Curry syntax.
showFuncDecl = showFuncDeclOpt defaultOptions

showFuncDeclOpt :: Options -> FuncDecl -> String
showFuncDeclOpt opts (Func cmt (_,name) arity _ ftype (Rules rules)) =
  funcComment cmt ++
  (maybe "" 
         (\texp -> bolName ++ " :: " ++ (showTypeExpr False texp)++"\n")
         ftype) ++
  (if funcIsInfixOp then rulePrints arity
      else name ++ (prefixInter (showRule opts) rules ("\n"++name)))
   where
     funcIsInfixOp = isInfixOpName name
     bolName = if funcIsInfixOp then "("++name++")" else name
     rulePrints arity' =
       concat $ intersperse "\n" $
         map (insertName arity' . (span (/=' ')) . tail . (showRule opts)) rules
     insertName arity' (fstArg,rest) = 
         if arity'/=0
           then fstArg++" "++name++rest
           else bolName++" "++fstArg++rest
showFuncDeclOpt _ (Func cmt (_,name) _ _ ftype (External _)) =
  funcComment cmt ++
  (maybe ""
         (\texp -> bolName ++ " :: " ++ (showTypeExpr False texp) ++"\n")
         ftype) ++
  bolName ++ " external"
 where
  bolName = if isInfixOpName name then "("++name++")" else name

-- format function comment as documentation comment
funcComment :: String -> String
funcComment = unlines . map ("--- "++) . lines

showLocalFuncDecl :: Options -> FuncDecl -> String
showLocalFuncDecl opts = showFuncDeclOpt opts

showRule :: Options -> Rule -> String
showRule opts (Rule pattlist crhslist localdecls) =
  prefixMap showPattern pattlist " " ++
  showCrhsList opts crhslist ++
  (if null localdecls
   then ""
   else  "\n   where\n" ++
           showBlock (prefixMap (showLocalDecl opts) localdecls "\n")
  )

showCrhsList :: Options -> [(Expr,Expr)] -> String
showCrhsList _ [] = ""
showCrhsList opts ((g,r):cs)
   | cs == [] && g == Symbol (prelude,"success") 
   =  " = " ++ showExprOpt opts r
   | otherwise 
   = "\n" ++ showBlock (combineMap (showCrhs opts) ((g,r):cs) "\n")

showCrhs :: Options -> (Expr,Expr) -> String
showCrhs opts (cond,expr) =
  "| " ++ showExprOpt opts cond ++ "\n= " ++ showExprOpt opts expr

showLocalDecl :: Options -> LocalDecl -> String
showLocalDecl opts (LocalFunc funcdecl) = showLocalFuncDecl opts funcdecl
showLocalDecl opts (LocalPat pattern expr localdecls) =
  showPattern pattern ++ " = " ++ showExprOpt opts expr ++
  (if null localdecls
   then ""
   else "\n   where\n" ++
        showBlock (prefixMap (showLocalDecl opts) localdecls "\n")
  )
showLocalDecl _ (LocalVar index) = showPattern (PVar index) ++ " free"

--- Shows an AbstractHaskell expression in standard Curry syntax.
showExpr = showExprOpt defaultOptions

showExprOpt :: Options -> Expr -> String
showExprOpt _ (Var (_,name)) = showIdentifier name
showExprOpt _ (Lit lit) = showLiteral lit
showExprOpt opts (Symbol name) 
  = if isInfixOpName (snd name) then "("++showSymbol opts name++")" 
                                else showSymbol opts name
showExprOpt opts (Apply func arg) = showApplication opts (Apply func arg)
showExprOpt opts (Lambda patts expr) = showLambdaOrSection opts patts expr
showExprOpt opts (Let localdecls expr)
   = "let\n" ++ showBlock ((combineMap (showLocalDecl opts) localdecls "\n") 
     ++ "\n in " ++ (showBoxedExpr opts expr))
showExprOpt opts (DoExpr stmts)
   = "\n    do\n" ++ showBlock (combineMap (showStatement opts) stmts "\n")
showExprOpt opts (ListComp expr stmts)
   =    "[ " ++ (showBoxedExpr opts expr) ++ " | "
     ++ (combineMap (showStatement opts) stmts ", ") ++ "]"
showExprOpt opts (Case expr branches)
   =    "case " ++ (showBoxedExpr opts expr) ++ " of\n"
     ++ showBlock (combineMap (showBranchExpr opts) branches "\n")


showSymbol :: Options -> QName -> String
showSymbol (fm,thisModule) (thatModule,symName)
  | thisModule == thatModule = symName
  | isJust (lookupFM fm symName) = thatModule++"."++symName
  | otherwise = symName

-- show a lambda expression as a left/right section, if 
-- it is a literal, var other than the pattern var or non-infix symbol.
-- A better test for sections would need the test for sub expressions
-- which is too complex for this simple purpose.
showLambdaOrSection opts patts expr = case patts of
  [PVar pvar] -> case expr of
     (Apply (Apply (Symbol (_,name)) lexpr) (Var var))
      -> if isInfixOpName name && isAtom lexpr && (Var var/=lexpr)
         then if pvar==var
              then "(" ++ showBoxedExpr opts lexpr ++ " " ++ name ++ ")"
              else if lexpr == (Var pvar)
                   then "(" ++ name ++ " " ++ showExprOpt opts (Var var) ++ ")"
                   else showLambda opts patts expr
         else showLambda opts patts expr
     (Apply (Apply (Symbol (_,name)) (Var var)) rexpr)
      -> if isInfixOpName name && pvar==var && isAtom rexpr && (Var var/=rexpr)
         then "(" ++ name ++ " " ++ showBoxedExpr opts rexpr ++ ")"
         else showLambda opts patts expr
     _ -> showLambda opts patts expr 
  _ -> showLambda opts patts expr

showLambda opts patts expr = "\\" ++ (combineMap showPattern patts " ")
                        ++ " -> " ++ (showExprOpt opts expr)


showStatement :: Options -> Statement -> String
showStatement opts (SExpr expr) = showExprOpt opts expr
showStatement opts (SPat pattern expr)
   = (showPattern pattern) ++ " <- " ++ (showExprOpt opts expr)
showStatement opts (SLet localdecls)
   = case localdecls of
       (decl:[]) -> "let " ++ showLocalDecl opts decl
       _         -> "let\n" ++ showBlock (combineMap (showLocalDecl opts) localdecls "\n")

showPattern :: Pattern -> String
showPattern (PVar (_,name)) = showIdentifier name
showPattern (PLit lit) = showLiteral lit
showPattern (PComb (_,name) []) = name 
showPattern (PComb (mod,name) (p:ps))
   | mod == prelude = showPreludeCons (PComb (mod,name) (p:ps))
   | isAsPattern p = showAsPatternList p   
   | otherwise        = "(" ++ name ++ (prefixMap showPattern (p:ps) " ") ++ ")"
showPattern (PAs (_,name) pat) = showIdentifier name ++ "@" ++ showPattern pat
showPattern (PFuncComb qname pats) = showPattern (PComb qname pats)
   

showPreludeCons :: Pattern -> String
showPreludeCons p
   | name == ":"  = showPatternList p
   | isTuple name = "(" ++ (combineMap showPattern pattlist ",") ++ ")"
   | otherwise    = "(" ++ name ++ (prefixMap showPattern pattlist " ") ++ ")"
   where
     PComb (_,name) pattlist = p

showPatternList :: Pattern -> String
showPatternList p
  | isClosedStringPattern p 
  = '\"':filter (/='\'') (concat (showPatListElems p))++"\""
  | isClosedPatternList p
  = "["++concat (intersperse "," (showPatListElems p))++"]"
  | isAsPattern p
  = showAsPatternList p
  | otherwise = "(" ++ concat (intersperse ":" (showPatListElems p))++")"

showPatListElems (PComb (_,":") [x,xs]) 
  = showPattern x : showPatListElems xs
showPatListElems (PComb (_,"[]") []) = []
showPatListElems (PVar v) = [showPattern (PVar v)]
showPatListElems (PAs name p) = [showPattern (PAs name p)]

isClosedPatternList (PComb (m,":") [_,xs]) =
  m==prelude && isClosedPatternList xs
isClosedPatternList (PComb (m,"[]") []) = m==prelude
isClosedPatternList (PVar _) = False
isClosedPatternList (PAs _ p) = isClosedPatternList p

isClosedStringPattern (PComb (m,":") [x,xs]) 
  = m==prelude && isCharPattern x && isClosedStringPattern xs
isClosedStringPattern (PComb (m,"[]") []) = m==prelude
isClosedStringPattern (PVar _) = False

isCharPattern p = case p of 
                    PLit (Charc _) -> True
                    _                -> False

isAsPattern p = case p of
                  PAs _ _ -> True
                  _        -> False

showAsPatternList (PAs (_,name) p) = 
     name++"@"++"(" ++ concat (intersperse ":" (showPatListElems p))++")"

showBranchExpr :: Options -> BranchExpr -> String
showBranchExpr opts (Branch pattern expr)
   = (showPattern pattern) ++ " -> " ++ (showExprOpt opts expr)

showLiteral :: Literal -> String
showLiteral (Intc i) = show i
showLiteral (Floatc f) = show f
showLiteral (Charc c) = "'"++showCharc (Charc c)++"'"

showCharc :: Literal -> String
showCharc (Charc c) | c=='\n' = "\\n"
                      | c=='\r' = "\\r"
                      | c=='\t' = "\\t"
                      | c=='\\' = "\\\\"
                      | c=='\"' = "\\\""
                      | otherwise = [c]

showBlock :: String -> String
showBlock text
   = combineMap id (map ((++) "     ") (filter ((/=) "") (lines text))) "\n"


showTypeCons :: String -> String -> [TypeExpr] -> String
showTypeCons _ name [] = name
showTypeCons mod name (t:ts)
   | mod == prelude = showPreludeTypeCons name (t:ts)
   | otherwise        = name ++ (prefixMap (showTypeExpr True) (t:ts) " ")

showPreludeTypeCons :: String -> [TypeExpr] -> String
showPreludeTypeCons name typelist
  | name == "[]" && head typelist == TCons (prelude,"Char") [] = "String"
  | name == "[]" = "[" ++ (showTypeExpr False (head typelist)) ++ "]"
  | isTuple name = "(" ++ (combineMap (showTypeExpr False) typelist ",") ++ ")"
  | otherwise    = name ++ (prefixMap (showTypeExpr True) typelist " ")



showApplication :: Options -> Expr -> String
showApplication opts appl
   = case (applicationHead appl) of
       (Symbol name) -> showSymbolApplication opts name appl
       _              -> showSimpleApplication opts appl

applicationHead :: Expr -> Expr
applicationHead expr
   = case expr of
       (Apply func _) -> applicationHead func
       _               -> expr

showSymbolApplication :: Options -> (String,String) -> Expr -> String
showSymbolApplication opts (mod,name) appl
   | mod == prelude && name == ":" 
   = showListApplication opts appl
   | isInfixOpName name 
   = showInfixApplication opts (mod,name) appl
   | mod == prelude && name == "if_then_else" 
   = showITEApplication opts appl
   | isTuple name
   = showTupleApplication opts appl
   | otherwise        
   = showSimpleApplication opts appl

showListApplication :: Options -> Expr -> String
showListApplication opts appl
   | isStringList appl
     = "\"" ++ (showCharListApplication opts appl) ++ "\""
   | isClosedList appl
     = "[" ++ (showConsListApplication opts appl) ++ "]"
   | otherwise
     = "(" ++ (showSimpleListApplication opts appl) ++ ")"

showCharListApplication :: Options -> Expr -> String
showCharListApplication opts (Apply (Apply _ (Lit c)) tail)
   = case tail of
       (Symbol _) -> showCharc c
       _           -> showCharc c ++ showCharListApplication opts tail

showConsListApplication :: Options -> Expr -> String
showConsListApplication opts (Apply (Apply _ head) tail)
   = case tail of
       (Symbol _) -> showBoxedExpr opts head
       _           -> (showBoxedExpr opts head) ++ "," 
                        ++ (showConsListApplication opts tail)

showSimpleListApplication :: Options -> Expr -> String
showSimpleListApplication opts (Apply (Apply _ head) tail)
   = case tail of
       (Symbol _) -> showBoxedExpr opts head ++ ":[]"
       _           -> showBoxedExpr opts head ++ ":" ++ showBoxedExpr opts tail
showSimpleListApplication opts (Apply (Symbol (_,str)) tail)
   = showBoxedExpr opts tail ++ str

showInfixApplication :: Options -> QName -> Expr -> String
showInfixApplication opts infixop (Apply func arg2) 
   = case func of 
       (Apply f arg1) -> case f of
                             (Apply _ arg0) -> 
                                  "(" ++ showBoxedExpr opts arg0 ++ " " ++
                                   showSymbol opts infixop ++ " " ++
                                   showBoxedExpr opts arg1 ++ ") " ++
                                   showBoxedExpr opts arg2
                             _ -> showBoxedExpr opts arg1 ++ " "
                                  ++ showSymbol opts infixop
                                  ++ " " ++ showBoxedExpr opts arg2
       _ -> "(" ++ showSymbol opts infixop ++ ") " ++ (showBoxedExpr opts arg2)

showITEApplication :: Options -> Expr -> String
showITEApplication opts (Apply (Apply (Apply (Symbol _) condExpr) thenExpr) elseExpr)
   =    "if " ++ (showExprOpt opts condExpr) ++ " then "
     ++ (showExprOpt opts thenExpr) ++ " else "
     ++ (showExprOpt opts elseExpr) 
showITEApplication opts (Apply e@(Apply (Apply (Apply _ _) _) _) e')
   = "("++showITEApplication opts e ++ ") "++showBoxedExpr opts e'

showTupleApplication :: Options -> Expr -> String
showTupleApplication opts appl
   = "(" ++ (p_showTuple appl) ++ ")"
   where
   p_showTuple (Apply (Symbol _) arg)
      = showExprOpt opts arg
   p_showTuple (Apply (Apply e1 e2) arg)
      = (p_showTuple (Apply e1 e2)) ++ "," ++ (showExprOpt opts arg)

showSimpleApplication :: Options -> Expr -> String
showSimpleApplication opts appl =
  case appl of
     Apply func arg -> showSimpleApplication opts func ++ " "
                        ++ showBoxedExpr opts arg
     _               -> showBoxedExpr opts appl

showBoxedExpr :: Options -> Expr -> String
showBoxedExpr opts expr
   | isSimpleExpr expr = showExprOpt opts expr
   | otherwise         = "(" ++ showExprOpt opts expr ++ ")"

-------------------------------------------------------------------------------
--- composition functions for AbstractHaskellPrinter
-------------------------------------------------------------------------------

prefixMap :: (a -> String) -> [a] ->  String -> String
prefixMap f xs s
   = concatMap ((++)s) (map f xs)

prefixInter :: (a -> String) -> [a] ->  String -> String
prefixInter f xs s
   = concat $ intersperse s (map f xs)

combineMap :: (a -> String) -> [a] ->  String -> String
combineMap _ [] _ = ""
combineMap f (x:xs) s
   = (f x) ++ (prefixMap f xs s)


dropTags :: String -> String
dropTags (x:xs) = case x of
                    '\"' -> dropTags $ tail $ dropWhile (/='\"') xs
                    '>'  -> xs
                    _    -> dropTags xs

-------------------------------------------------------------------------------
--- tests for various properties of AbstractHaskell constructs
-------------------------------------------------------------------------------

isInfixOpName :: String -> Bool
isInfixOpName = all (`elem` infixIDs)

isStringList :: Expr -> Bool
isStringList (Symbol (mod,name))
   = mod == prelude && name == "[]"
isStringList (Var _) = False
isStringList (Apply head tail)
   = case head of 
       (Apply _ (Lit (Charc _))) -> isStringList tail
       _                            -> False

isClosedList :: Expr -> Bool
isClosedList expr
   = case expr of
       (Apply (Apply (Symbol (mod,name)) _) tail)
          -> mod==prelude && name==":" && isClosedList tail
       (Symbol (mod,name))
          -> mod == prelude && name == "[]"
       _  -> False

isSimpleExpr :: Expr -> Bool
isSimpleExpr expr
   = case expr of
       (Var _)      -> True
       (Lit _)      -> True
       (Symbol (_, name)) -> not $ isInfixOpName name
       (Apply f _)  -> case (applicationHead f) of
                          (Symbol (m,name)) ->  m==prelude &&
                                                 (name == ":"
                                                  || name == "[]"
                                                  || name == "()"
                                                  || isTuple name)
                          _                  -> False
       _             -> False


isAtom :: Expr -> Bool
isAtom expr
   = case expr of
       (Var _)      -> True
       (Lit _)      -> True
       (Symbol (_, name)) -> not $ isInfixOpName name
       _ -> False

isTuple :: String -> Bool
isTuple [] = False
isTuple (x:xs) = (x == '(') && (p1_isTuple xs)
   where
   p1_isTuple [] = False
   p1_isTuple (z:[]) = z == ')'
   p1_isTuple (z1:z2:zs) = (z1 == ',') && (p1_isTuple (z2:zs))

------------------------------------------------------------------------------
--- constants used by AbstractHaskellPrinter
------------------------------------------------------------------------------

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

prelude = "Prelude"

-- enclose string with brackets, if required by first argument:
maybeShowBrackets nested s =
   (if nested then "(" else "") ++ s ++ (if nested then ")" else "")

------------------------------------------------
-- building the map of defined function names
------------------------------------------------

nameFM :: [FuncDecl] -> NameFM
nameFM = foldr addName (emptyFM lessString) 

addName :: FuncDecl -> NameFM -> NameFM
addName (Func _ (_,n) _ _ _ _) fm = addToFM fm n () 

lessString s1 s2 = LT==cmpString s1 s2
