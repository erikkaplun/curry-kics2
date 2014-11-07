------------------------------------------------------------------------------
--- A pretty printer for AbstractHaskell programs.
---
--- This library defines a function "showProg" that shows
--- an AbstractHaskell program in standard Haskell syntax.
---
--- @author Martin Engelke, Bernd Brassel, Michael Hanus, Marion Mueller,
---         Parissa Sadeghi, Bjoern Peemoeller
--- @version May 2014
------------------------------------------------------------------------------
{-# LANGUAGE Records #-}
module AbstractHaskellPrinter
  ( showProg, showModuleHeader, showDecls, showTypeDecls, showTypeDecl
  , showTypeExpr, showFuncDecl, showLiteral, showExpr, showPattern
  ) where

import Char  (isDigit)
import List  (intercalate, partition)
import Maybe (isJust)
import Read  (readNat)

import AbstractHaskell
import Names (isHaskellModule, prelude, curryPrelude, addTrace)

type Options = { currentModule :: String, traceFailure :: Bool }

defaultOptions :: Options
defaultOptions = { currentModule := "", traceFailure := False }

-- ---------------------------------------------------------------------------
-- Functions to print an AbstractHaskell program in standard Curry syntax
-- ---------------------------------------------------------------------------

--- Shows an AbstractHaskell program in standard Curry syntax.
--- The export list contains the public functions and the
--- types with their data constructors (if all data constructors are public),
--- otherwise only the type constructors.
--- The potential comments in function declarations are formatted as
--- documentation comments.
showProg ::Bool ->  Prog -> String
showProg trace (Prog m imports typedecls funcdecls opdecls) =
  intercalate "\n\n" $ filter (not . null) $
    [ showModuleHeader trace m typedecls funcdecls imports
    , showDecls trace m opdecls typedecls funcdecls
    ]

showModuleHeader :: Bool -> String -> [TypeDecl] -> [FuncDecl] -> [String] -> String
showModuleHeader trace m ts fs imps = concat
  [ "module " ++ (if trace then addTrace m else m)
  , " (" ++ exports ++ ")"
  , " where"
  , if null imports then "" else "\n\n" ++ imports
  ]
  where exports = showExports ts fs
        imports = showImports trace imps

showDecls :: Bool -> String -> [OpDecl] -> [TypeDecl] -> [FuncDecl] -> String
showDecls trace m opdecls typedecls funcdecls
  = intercalate "\n\n" $ filter (not . null)
    [ showOpDecls opdecls
    , showTypeDecls opts typedecls
    , showFuncDecls opts funcdecls
    ]
  where opts = { currentModule := m, traceFailure := trace }

-- ---------------------------------------------------------------------------
-- Module Header
-- ---------------------------------------------------------------------------

showExports :: [TypeDecl] -> [FuncDecl] -> String
showExports types funcs =
  let publicTypes = filter isPublicType types
      (withCons, withoutCons) = partition allPublicCons publicTypes
  in
   intercalate ", " $
       map ((++ " (..)") . getTypeName) withCons
    ++ map getTypeName withoutCons
    ++ map getFuncName (filter isPublicFunc funcs)
  where
    isPublicType :: TypeDecl -> Bool
    isPublicType (Type    _ vis _ _) = vis == Public
    isPublicType (TypeSyn _ vis _ _) = vis == Public
    isPublicType (Instance  _ _ _ _) = False

    isPublicFunc :: FuncDecl -> Bool
    isPublicFunc (Func _ _ _ vis _ _) = vis == Public

    getTypeName :: TypeDecl -> String
    getTypeName (Type     (_,name) _ _ _) = name
    getTypeName (TypeSyn  (_,name) _ _ _) = name
    getTypeName (Instance (_,name) _ _ _) = name

    allPublicCons :: TypeDecl -> Bool
    allPublicCons (Type _ _ _ c) = length (filter isPublicCons c) == length c
      where isPublicCons (Cons _ _ vis _) = vis == Public
    allPublicCons (TypeSyn _ _ _ _)  = False
    allPublicCons (Instance _ _ _ _) = False

    getFuncName :: FuncDecl -> String
    getFuncName (Func _ (_,name) _ _ _ _) =
      if isInfixOpName name then "(" ++ name ++ ")" else name

showImports :: Bool -> [String] -> String
showImports trace imports = prefixInter showImport imports "\n"
  where
  showImport imp
      -- Haskell modules are imported unqualified
    | isHaskellModule imp = "import " ++ imp
      -- all Curry modules are imported qualified
    | trace               = "import qualified " ++ addTrace imp ++ " as " ++ imp
    | otherwise           = "import qualified " ++ imp

-- ---------------------------------------------------------------------------
-- Imports + infix operator declarations
-- ---------------------------------------------------------------------------

showOpDecls :: [OpDecl] -> String
showOpDecls opdecls = prefixInter showOpDecl opdecls "\n"

showOpDecl :: OpDecl -> String
showOpDecl (Op (_, name) fixity precedence) =
  showFixity fixity ++ " " ++ show precedence ++ " " ++
  if isInfixOpName name then name else '`' : name ++ "`"

showFixity :: Fixity -> String
showFixity InfixOp  = "infix"
showFixity InfixlOp = "infixl"
showFixity InfixrOp = "infixr"

-- ---------------------------------------------------------------------------
-- Type declarations
-- ---------------------------------------------------------------------------

--- Shows a list of AbstractHaskell type declarations in standard Curry syntax
showTypeDecls :: Options -> [TypeDecl] -> String
showTypeDecls opts tydecls = prefixInter (showTypeDecl opts) tydecls "\n\n"

--- Shows an AbstractHaskell type declaration in standard Curry syntax.
showTypeDecl :: Options -> TypeDecl -> String
showTypeDecl opts (TypeSyn qname _ indexlist typeexpr)
   = "type "  ++ showSymbol opts qname
              ++ prefixMap (showTypeExpr opts False) (map TVar indexlist) " "
     ++ " = " ++ showTypeExpr opts False typeexpr
showTypeDecl opts (Type qname _ indexlist consdecls)
  | null consdecls
  = ""
  | otherwise
  = "data " ++ showSymbol opts qname
    ++ (prefixMap (showTypeExpr opts False) (map TVar indexlist) " ") ++ "\n"
    ++ showBlock ("= " ++ (combineMap (showConsDecl opts) consdecls "\n| "))
showTypeDecl opts (Instance qname texp ctxts rules)
   = "instance " ++ showContext opts ctxts ++ showSymbol opts qname
     ++ " " ++ showTypeExpr opts True texp
     ++ " where\n" ++ concatMap showInstRule rules
 where
  showInstRule ((_, fname), rule) = if isInfixOpName fname
    then "  (" ++ fname ++ ")" ++ showRule opts rule ++ "\n"
    else "  " ++ fname ++ showRule opts rule ++ "\n"

showContext :: Options -> [Context] -> String
showContext _    []         = ""
showContext opts [ctxt]     = showClass opts ctxt ++ " => "
showContext opts cs@(_:_:_) =
  "(" ++ intercalate "," (map (showClass opts) cs) ++ ") => "

showClass :: Options -> Context -> String
showClass opts (Context qn tvars) =
  showTypeExpr opts False (TCons qn (map TVar tvars))

showConsDecl :: Options -> ConsDecl -> String
showConsDecl opts (Cons qname _ _ typelist)
  = showSymbol opts qname ++ prefixMap (showTypeExpr opts True) typelist " "

--- Shows an AbstractHaskell type expression in standard Haskell syntax.
--- If the second argument is True, the type expression is enclosed
--- in brackets.
showTypeExpr :: Options -> Bool -> TypeExpr -> String
showTypeExpr _    _      (TVar (_, name)) = showTypeVar (showIdentifier name)
showTypeExpr opts nested (FuncType dom rng) =
  maybeShowBrackets nested $ showTypeExpr opts (isFuncType dom) dom
                             ++ " -> " ++ showTypeExpr opts False rng
showTypeExpr opts nested (TCons qname@(mod,name) typelist)
   | mod==prelude && name == "untyped" = "-" -- TODO: Can this happen?
   | otherwise  = maybeShowBrackets (nested && not (null typelist))
                                    (showTypeCons opts qname typelist)

--- Shows an AbstractHaskell type signature of a given function name.
showTypeSig :: Options -> String -> TypeSig -> String
showTypeSig _    _     Untyped           = ""
showTypeSig opts fname (FType texp)      =
  (if isInfixOpName fname then "(" ++ fname ++ ")" else fname)
  ++ " :: " ++ showTypeExpr opts False texp ++ "\n"
showTypeSig opts fname (CType ctxt texp) =
  (if isInfixOpName fname then "(" ++ fname ++ ")" else fname)
  ++ " :: " ++ showContext opts ctxt ++ showTypeExpr opts False texp ++ "\n"

-- Show a1,a2,a3 as a_1,a_2,a_3 (due to bug in PAKCS front-end):
showTypeVar :: String -> String
showTypeVar []     = []
showTypeVar (c:cs) =
  if c == 'a' && not (null cs) && all isDigit cs
  then c:'_':cs
  else c:cs

-- Remove characters '<' and '>' from identifiers sind these characters
-- are sometimes introduced in new identifiers generated by the front end
-- (for sections)
showIdentifier :: String -> String
showIdentifier = filter (not . flip elem "<>")

--- Shows an AbstractHaskell function declaration in standard Curry syntax.
showFuncDecl :: FuncDecl -> String
showFuncDecl = showFuncDeclOpt defaultOptions

-- ---------------------------------------------------------------------------
-- Function Declaration
-- ---------------------------------------------------------------------------

showFuncDecls :: Options -> [FuncDecl] -> String
showFuncDecls opts fdecls = prefixInter (showFuncDeclOpt opts) fdecls "\n\n"

showFuncDeclOpt :: Options -> FuncDecl -> String
showFuncDeclOpt opts (Func cmt (_,name) arity _ ftype (Rules rules)) =
  funcComment cmt ++ showTypeSig opts name ftype ++
  (if funcIsInfixOp
    then rulePrints arity
    else name ++ (prefixInter (showRule opts) rules ("\n"++name)))
  where
    funcIsInfixOp     = isInfixOpName name
    bolName           = if funcIsInfixOp then "(" ++ name ++ ")" else name
    rulePrints arity' = intercalate "\n" $
      map (insertName arity' . (span (/= ' ')) . tail . (showRule opts)) rules
    insertName arity' (fstArg, rest) =
        if arity' /= 0
          then fstArg  ++ " " ++ name   ++ rest
          else bolName ++ " " ++ fstArg ++ rest
showFuncDeclOpt opts (Func cmt (_,name) _ _ ftype (External _)) =
  funcComment cmt ++ showTypeSig opts name ftype ++ bolName ++ " external"
  where bolName = if isInfixOpName name then "(" ++ name ++ ")" else name

-- format function comment as documentation comment
funcComment :: String -> String
funcComment = unlines . map ("--- " ++) . lines

showLocalFuncDecl :: Options -> FuncDecl -> String
showLocalFuncDecl opts = showFuncDeclOpt opts

showRule :: Options -> Rule -> String
showRule opts (Rule pattlist crhslist localdecls) =
  prefixMap (showPattern opts) pattlist " " ++
  showCrhsList opts crhslist ++
  (if null localdecls
   then ""
   else  "\n   where\n" ++
           showBlock (prefixMap (showLocalDecl opts) localdecls "\n")
  )

showCrhsList :: Options -> [(Expr,Expr)] -> String
showCrhsList _ [] = ""
showCrhsList opts ((g,r):cs)
   | cs == [] && g == Symbol (prelude, "success")
   =  " = " ++ showExprOpt opts r
   | otherwise
   = "\n" ++ showBlock (combineMap (showCrhs opts) ((g,r):cs) "\n")

showCrhs :: Options -> (Expr, Expr) -> String
showCrhs opts (cond,expr) =
  "| " ++ showExprOpt opts cond ++ "\n= " ++ showExprOpt opts expr

showLocalDecl :: Options -> LocalDecl -> String
showLocalDecl opts (LocalFunc     f) = showLocalFuncDecl opts f
showLocalDecl opts (LocalPat p e ds) =
  showPattern opts p ++ " = " ++ showExprOpt opts e ++
  (if null ds
   then ""
   else "\n   where\n" ++
        showBlock (prefixMap (showLocalDecl opts) ds "\n")
  )

showExpr :: Expr -> String
showExpr = showExprOpt defaultOptions

--- Shows an AbstractHaskell expression in standard Curry syntax.
showExprOpt :: Options -> Expr -> String
showExprOpt _ (Var (_, name))  = showIdentifier name
showExprOpt _ (Lit lit)        = showLiteral lit
showExprOpt opts (Symbol name) = if isInfixOpName (snd name)
  then "(" ++ showSymbol opts name ++ ")"
  else showSymbol opts name
showExprOpt opts (Apply func arg)      = showApplication opts (Apply func arg)
showExprOpt opts (Lambda patts expr)   = showLambdaOrSection opts patts expr
showExprOpt opts (Let localdecls expr) =
  "let\n" ++ showBlock ((combineMap (showLocalDecl opts) localdecls "\n")
  ++ "\n in " ++ (showBoxedExpr opts expr))
showExprOpt opts (DoExpr stmts)        =
  "\n    do\n" ++ showBlock (combineMap (showStatement opts) stmts "\n")
showExprOpt opts (ListComp expr stmts) =
  "[ " ++ (showBoxedExpr opts expr) ++ " | "
       ++ (combineMap (showStatement opts) stmts ", ") ++ "]"
showExprOpt opts (Case expr branches)  =
  "case " ++ (showBoxedExpr opts expr) ++ " of\n"
          ++ showBlock (combineMap (showBranchExpr opts) branches "\n")
showExprOpt opts (Typed e ty) = '(' : showExprOpt opts e ++ " :: "
                                ++ showTypeExpr opts False ty ++ ")"
showExprOpt opts (IfThenElse boolExpr expr1 expr2)  =
  "if " ++ (showBoxedExpr opts boolExpr)
        ++ "\n    then " ++ (showBoxedExpr opts expr1)
        ++ "\n    else " ++ (showBoxedExpr opts expr2)

showSymbol :: Options -> QName -> String
showSymbol opts (modName, symName)
    -- all Haskell modules are imported unqualified
  | isHaskellModule modName            = symName
    -- the current module isn't imported at all
  | modName == (opts :> currentModule) = symName
    -- all Curry modules are imported qualified
  | otherwise                          = modName ++ "." ++ symName

-- show a lambda expression as a left/right section, if
-- it is a literal, var other than the pattern var or non-infix symbol.
-- A better test for sections would need the test for sub expressions
-- which is too complex for this simple purpose.
showLambdaOrSection :: Options -> [Pattern] -> Expr -> String
showLambdaOrSection opts patts expr = case patts of
  [PVar pvar] -> case expr of
    (Apply (Apply (Symbol qname@(_, name)) lexpr) (Var var)) ->
      if isInfixOpName name && isAtom lexpr && (Var var /= lexpr)
        then if pvar == var
          then "(" ++ showBoxedExpr opts lexpr ++ " "
               ++ showSymbol opts qname ++ ")"
          else if lexpr == (Var pvar)
            then "(" ++ showSymbol opts qname ++ " "
                 ++ showExprOpt opts (Var var) ++ ")"
            else showLambda opts patts expr
        else showLambda opts patts expr
    (Apply (Apply (Symbol (_,name)) (Var var)) rexpr) ->
      if isInfixOpName name && pvar==var && isAtom rexpr && (Var var/=rexpr)
        then "(" ++ name ++ " " ++ showBoxedExpr opts rexpr ++ ")"
        else showLambda opts patts expr
    _ -> showLambda opts patts expr
  _ -> showLambda opts patts expr

showLambda :: Options -> [Pattern] -> Expr -> String
showLambda opts patts expr = "\\" ++ (combineMap (showPattern opts) patts " ")
                             ++ " -> " ++ (showExprOpt opts expr)

showStatement :: Options -> Statement -> String
showStatement opts (SExpr expr) = showExprOpt opts expr
showStatement opts (SPat pattern expr)
   = (showPattern opts pattern) ++ " <- " ++ (showExprOpt opts expr)
showStatement opts (SLet localdecls) = case localdecls of
  (decl:[]) -> "let "  ++ showLocalDecl opts decl
  _         -> "let\n" ++ showBlock
                          (combineMap (showLocalDecl opts) localdecls "\n")

showPattern :: Options -> Pattern -> String
showPattern _    (PVar (_,name))  = showIdentifier name
showPattern opts (PLit lit)       = showLitPattern opts lit
showPattern opts (PComb qname []) = showSymbol opts qname
showPattern opts (PComb qname@(mod,_) (p:ps))
  | mod == prelude   = showPreludeCons opts (PComb qname (p:ps))
  | otherwise        = "(" ++ showSymbol opts qname
                       ++ (prefixMap (showPattern opts) (p:ps) " ") ++ ")"
showPattern opts (PAs (_,name) pat)     = showIdentifier name ++ "@"
                                           ++ showPattern opts pat
showPattern opts (PFuncComb qname pats) = showPattern opts (PComb qname pats)

showLitPattern :: Options -> Literal -> String
showLitPattern opts l = case l of
    Charc   _ -> wrapUnboxed (curryPrelude, "C_Char" )
    Floatc  _ -> wrapUnboxed (curryPrelude, "C_Float")
    Intc    _ -> wrapUnboxed (curryPrelude, "C_Int"  )
    Stringc _ -> showLiteral l
  where wrapUnboxed c = '(' : showSymbol opts c ++ ' ' : showLiteral l ++ "#)"

showPreludeCons :: Options -> Pattern -> String
showPreludeCons opts p
  | name == ":"  = showPatternList opts p
  | isTuple name = "(" ++ (combineMap (showPattern opts) pattlist ",") ++ ")"
  | otherwise    = "(" ++ showSymbol opts qname
                   ++ (prefixMap (showPattern opts) pattlist " ") ++ ")"
   where PComb qname@(_, name) pattlist = p

showPatternList :: Options -> Pattern -> String
showPatternList opts p
  | isClosedStringPattern p
  = '\"' : filter (/='\'') (concat (showPatListElems opts p)) ++ "\""
  | isClosedPatternList p
  = "[" ++ intercalate "," (showPatListElems opts p) ++ "]"
  | isAsPattern p
  = showAsPatternList opts p
  | otherwise = "(" ++ intercalate ":" (showPatListElems opts p) ++ ")"

showPatListElems :: Options -> Pattern -> [String]
showPatListElems opts pat = case pat of
  (PComb (_,":")  [x,xs]) -> showPattern opts x : showPatListElems opts xs
  (PComb (_,"[]") [])     -> []
  (PVar v)                -> [showPattern opts (PVar v)]
  (PAs name p)            -> [showPattern opts (PAs name p)]
  _                       -> error "AbstractHaskellPrinter.showPatListElems"

showAsPatternList :: Options -> Pattern -> String
showAsPatternList opts pat = case pat of
  (PAs (_,name) p) -> name ++ "@" ++ "(" ++ intercalate ":" (showPatListElems opts p) ++ ")"
  _                -> error "AbstractHaskellPrinter.showAsPatternList"

showBranchExpr :: Options -> BranchExpr -> String
showBranchExpr opts (Branch pattern expr)
   = (showPattern opts pattern) ++ " -> " ++ (showExprOpt opts expr)

showLiteral :: Literal -> String
showLiteral (Intc    i) = show i
showLiteral (Floatc  f) = show f
showLiteral (Charc   c) = show c
showLiteral (Stringc s) = show s

showBlock :: String -> String
showBlock text
  = combineMap id (map ((++) "     ") (filter ((/=) "") (lines text))) "\n"

showTypeCons :: Options -> QName -> [TypeExpr] -> String
showTypeCons opts qname [] = showSymbol opts qname
showTypeCons opts qname@(mod, name) (t:ts)
  | mod == prelude = showPreludeTypeCons opts name (t:ts)
  | otherwise      = showSymbol opts qname
                     ++ (prefixMap (showTypeExpr opts True) (t:ts) " ")

showPreludeTypeCons :: Options -> String -> [TypeExpr] -> String
showPreludeTypeCons opts name typelist
  | name == "[]" && head typelist == TCons (prelude, "Char") []
    = "String"
  | name == "[]"
    = "[" ++ (showTypeExpr opts False (head typelist)) ++ "]"
  | isTuple name
    = "(" ++ (combineMap (showTypeExpr opts False) typelist ",") ++ ")"
  | otherwise
    = name ++ (prefixMap (showTypeExpr opts True) typelist " ")

showApplication :: Options -> Expr -> String
showApplication opts appl = case (applicationHead appl) of
  (Symbol name) -> showSymbolApplication opts name appl
  _             -> showSimpleApplication opts appl

applicationHead :: Expr -> Expr
applicationHead expr = case expr of
  (Apply func _) -> applicationHead func
  _              -> expr

showSymbolApplication :: Options -> (String, String) -> Expr -> String
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
  | isClosedList appl
    = "[" ++ showConsListApplication opts appl ++ "]"
  | otherwise
    = "(" ++ showSimpleListApplication opts appl ++ ")"

showConsListApplication :: Options -> Expr -> String
showConsListApplication opts expr = case expr of
  (Apply (Apply _ head) tail) -> case tail of
    (Symbol _) -> showBoxedExpr opts head
    _          -> (showBoxedExpr opts head) ++ "," ++ (showConsListApplication opts tail)
  _                           -> error "AbstractHaskellPrinter.showConsListApplication"

showSimpleListApplication :: Options -> Expr -> String
showSimpleListApplication opts expr = case expr of
  (Apply (Apply _ head) tail)   -> case tail of
    (Symbol _) -> showBoxedExpr opts head ++ ":[]"
    _          -> showBoxedExpr opts head ++ ":" ++ showBoxedExpr opts tail
  (Apply (Symbol (_,str)) tail) -> showBoxedExpr opts tail ++ str
  _                             -> error "AbstractHaskellPrinter.showSimpleListApplication"

showInfixApplication :: Options -> QName -> Expr -> String
showInfixApplication opts infixop expr = case expr of
  (Apply func arg2) -> case func of
    (Apply f arg1) -> case f of
      (Apply _ _) -> "(" ++ showInfixApplication opts infixop func ++ ") " ++ showBoxedExpr opts arg2
      _           -> showBoxedExpr opts arg1 ++ " "
                     ++ showSymbol opts infixop
                     ++ " " ++ showBoxedExpr opts arg2
    _              -> "(" ++ showSymbol opts infixop ++ ") " ++ (showBoxedExpr opts arg2)
  _                 -> error "AbstractHaskellPrinter.showInfixApplication"

showITEApplication :: Options -> Expr -> String
showITEApplication opts expr = case expr of
  (Apply (Apply (Apply (Symbol _) condExpr) thenExpr) elseExpr) ->
      "if " ++ (showExprOpt opts condExpr) ++ " then "
        ++ (showExprOpt opts thenExpr) ++ " else "
        ++ (showExprOpt opts elseExpr)
  (Apply e@(Apply (Apply (Apply _ _) _) _) e')                  ->
      "("++showITEApplication opts e ++ ") "++showBoxedExpr opts e'
  _                                                             ->
      error "AbstractHaskellPrinter.showITEApplication"

showTupleApplication :: Options -> Expr -> String
showTupleApplication opts appl = "(" ++ (p_showTuple appl) ++ ")"
  where
   p_showTuple expr = case expr of
      (Apply (Symbol _) arg)    -> showExprOpt opts arg
      (Apply (Apply e1 e2) arg) -> (p_showTuple (Apply e1 e2)) ++ "," ++ (showExprOpt opts arg)
      _                         -> error "AbstractHaskellPrinter.showTupleApplication"

showSimpleApplication :: Options -> Expr -> String
showSimpleApplication opts appl = case appl of
  Apply func arg -> showSimpleApplication opts func ++ " "
                    ++ showBoxedExpr opts arg
  _               -> showBoxedExpr opts appl

showBoxedExpr :: Options -> Expr -> String
showBoxedExpr opts expr
  | isSimpleExpr expr = showExprOpt opts expr
  | otherwise         = "(" ++ showExprOpt opts expr ++ ")"

------------------------------------------------------------------------------
--- composition functions for AbstractHaskellPrinter
------------------------------------------------------------------------------

prefixMap :: (a -> String) -> [a] ->  String -> String
prefixMap f xs s = concatMap (s ++) (map f xs)

prefixInter :: (a -> String) -> [a] ->  String -> String
prefixInter f xs s = intercalate s (map f xs)

combineMap :: (a -> String) -> [a] ->  String -> String
combineMap _ []     _ = ""
combineMap f (x:xs) s = f x ++ prefixMap f xs s

dropTags :: String -> String
dropTags []     = []
dropTags (x:xs) = case x of
  '\"' -> dropTags $ tail $ dropWhile (/= '\"') xs
  '>'  -> xs
  _    -> dropTags xs

------------------------------------------------------------------------------
--- tests for various properties of AbstractHaskell constructs
------------------------------------------------------------------------------

isClosedPatternList :: Pattern -> Bool
isClosedPatternList pat = case pat of
  (PComb (m,":") [_, xs]) -> m == prelude && isClosedPatternList xs
  (PComb (m,"[]") [])     -> m == prelude
  (PVar _)                -> False
  (PAs _ p)               -> isClosedPatternList p
  _                       -> error "AbstractHaskellPrinter.isClosedPatternList"

isClosedStringPattern :: Pattern -> Bool
isClosedStringPattern pat = case pat of
  (PComb (m,":") [x,xs]) -> m == prelude && isCharPattern x && isClosedStringPattern xs
  (PComb (m,"[]") [])    -> m == prelude
  (PVar _)               -> False
  (PAs _ _)              -> False
  _                      -> error "AbstractHaskellPrinter.isClosedStringPattern"

isCharPattern :: Pattern -> Bool
isCharPattern p = case p of
  PLit (Charc _) -> True
  _              -> False

isAsPattern :: Pattern -> Bool
isAsPattern p = case p of
    PAs _ _ -> True
    _       -> False

isInfixOpName :: String -> Bool
isInfixOpName = all (`elem` infixIDs)

isClosedList :: Expr -> Bool
isClosedList expr = case expr of
  (Apply (Apply (Symbol (mod,name)) _) tail)
    -> mod == prelude && name == ":" && isClosedList tail
  (Symbol (mod,name))
    -> mod == prelude && name == "[]"
  _  -> False

isSimpleExpr :: Expr -> Bool
isSimpleExpr expr = case expr of
  (Var _)            -> True
  (Lit _)            -> True
  (Symbol (_, name)) -> not $ isInfixOpName name
  (Apply f _)        -> case (applicationHead f) of
    (Symbol (m,name)) -> m == prelude &&
                         (    name == ":"
                           || name == "[]"
                           || name == "()"
                           || isTuple name
                         )
    _                  -> False
  _             -> False


isFuncType :: TypeExpr -> Bool
isFuncType t = case t of
  FuncType _ _ -> True
  _            -> False

isAtom :: Expr -> Bool
isAtom expr = case expr of
  (Var _)            -> True
  (Lit _)            -> True
  (Symbol (_, name)) -> not $ isInfixOpName name
  _                  -> False

isTuple :: String -> Bool
isTuple []     = False
isTuple (x:xs) = (x == '(') && (p1_isTuple xs)
  where
    p1_isTuple []         = False
    p1_isTuple (z:[])     = z == ')'
    p1_isTuple (z1:z2:zs) = (z1 == ',') && (p1_isTuple (z2:zs))

------------------------------------------------------------------------------
--- constants used by AbstractHaskellPrinter
------------------------------------------------------------------------------

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

-- enclose string with brackets, if required by first argument
maybeShowBrackets :: Bool -> String -> String
maybeShowBrackets nested s
  | nested    = "(" ++ s ++ ")"
  | otherwise = s
