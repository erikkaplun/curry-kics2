module PrettyFlat where

import Char (isUpper)
import Maybe
import Pretty
import FlatCurry
import FlatCurryGoodies
import StyledText
import System

prelude = "Prelude"

arrow = operator (text "->")
bar = operator (char '|')
dcolon = operator (text "::")

type Precs = [(QName,Int)]

precFillEncloseSep :: Int -> Int -> Doc -> Doc -> Doc -> [Doc] -> Doc
precFillEncloseSep p1 p2 l r s ds =
  fillEncloseSep (pre p1 p2 l) (pre p1 p2 r) s ds
 where
  pre pO pI br
    | pO<pI = empty
    | True  = br

isInfixName :: QName -> Bool
isInfixName (_,n) = all (`elem` infixIDs) n

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

isTupleName :: QName -> Bool
isTupleName (mod,name) = mod == prelude && elem (take 2 name) ["()","(,"]

showStyledProg :: Prog -> String
showStyledProg = prettyProg 78

prettyProg :: Int -> Prog -> String
prettyProg n = pretty n . progDoc . updProgExps elimApp

prettyTypeExpr :: String -> TypeExpr -> String
prettyTypeExpr mod = pretty 78 . typeExprDoc mod False

prettyTypes :: String -> [TypeDecl] -> String
prettyTypes mod = pretty 78 . typesDoc mod

prettyOps :: [OpDecl] -> String
prettyOps = pretty 78 . opsDoc

showProg :: Prog -> String
showProg = plainText . showStyledProg

printStyledProg :: String -> IO ()
printStyledProg f = readFlatCurry f >>= printStyledText . showStyledProg

mainPrint :: IO ()
mainPrint = getArgs >>= mapIO_ printProg

printProg :: String -> IO ()
printProg f = readFlatCurryFile f >>= putStrLn . showProg

-- viewStyledProg :: WidgetRef -> Prog -> GuiPort -> IO ()
-- viewStyledProg ref prog = setStyledText ref (showStyledProg prog)

--viewProg :: WidgetRef -> Prog -> GuiPort -> IO ()
--viewProg ref prog = setValue ref (showProg prog)

keyword, consname :: String -> Doc
keyword = magentaDoc . text
consname = greenDoc . text

operator, literal, marked :: Doc -> Doc
operator = blueDoc
literal = cyanDoc
marked = bgYellowDoc . boldDoc

block :: Doc -> Doc
block = group . hang 1

def :: Doc -> [Doc] -> Doc -> Doc
def name params body = block (name <> paramDoc <$> body)
 where
  paramDoc = if null params then empty 
              else space <> align (fillSep params) --(map varDoc params))
{-
fillMulti :: (Doc -> Doc) -> (Doc -> Doc) -> [Doc] -> Doc
fillMulti _ _ [] = empty
fillMulti x y (d:ds)
  = align (fillSep ((x d) : map (group . (linebreak<>) . y) ds))
-}
app :: Doc -> [Doc] -> Doc
app d ds = if null ds then d
            else block (fillEncloseSep empty empty space (d:ds))
            --else block (d <$> fillEncloseSep empty empty space ds)
            --else block (d <$> fillMulti (empty<>) (empty<>) ds)

layout :: [Doc] -> Doc
layout = align . compose (combine (linesep "; "))

qname :: String -> QName -> Doc
qname prog mn@(mod,name)
  | mn == (prelude,"[]") || isTupleName mn = operator (text name)
  | isInfixName mn = parens (operator (text name))
  | otherwise -- do not show module names for Haskell generation
    = if -- True 
         mod `elem` [prelude,prog,""] 
       then txt (correctName name) 
       else consname mod <> dot <> (txt name)
 where
  txt (n:ame) = if isUpper n then consname (n:ame) else text (n:ame)

correctName :: String -> String
correctName name = 
  let name' = name --filter (not . flip elem ['#','.']) name
  in
  case name' of 
       ('_':xs) -> xs
       _        -> name'

d1 <$>> d2 = d1 <$> line <> d2

progDoc :: Prog -> Doc
progDoc prog@(Prog name imps types funcs ops)
  = moduleHeaderDoc name (exportedNames name prog) <$>>
    impsDoc imps <$>> opsDoc ops <$>>
    typesDoc name types <$>>
    funcsDoc (precs ops) name funcs

precs :: [OpDecl] -> Precs
precs = map (\(Op name _ i) -> (name,i))

exportedNames :: String -> Prog -> [Doc]
exportedNames mod (Prog _ _ types funcs _)
  = map typeExpDoc (filter ((Public==) . typeVisibility) types)
 ++ map (qname mod . funcName) (filter ((Public==) . funcVisibility) funcs)
 where
  typeExpDoc tdecl =
    let ecs = filter ((Public==) . consVisibility)
                     (trType (\_ _ _ cs -> cs) (\_ _ _ _ -> []) tdecl)
     in qname mod (typeName tdecl) <> if null ecs then empty else text "(..)"

moduleHeaderDoc :: String -> [Doc] -> Doc
moduleHeaderDoc name exports
  = keyword "module" <+> consname name <+> 
    keyword "where"

exportsDoc :: [Doc] -> Doc
exportsDoc xs
  = group (nest 1 (lparen <$> align (fillSep (punctuate comma xs)) <$> rparen))

impsDoc :: [String] -> Doc
impsDoc imps = vcat (map ((keyword "import" <+>) . consname) imps)

opsDoc :: [OpDecl] -> Doc
opsDoc ops = vcat (map opDoc ops)

opDoc :: OpDecl -> Doc
opDoc (Op n@(_,name) fix prec)
  = keyword "infix" <> fixDoc fix <+> int prec <+> operator (text infname)
 where
  infname = if isInfixName n then name else '`':name++"`"
  fixDoc InfixOp = empty
  fixDoc InfixlOp = keyword "l"
  fixDoc InfixrOp = keyword "r"

typesDoc :: String -> [TypeDecl] -> Doc
typesDoc mod = vcat . map (typeDoc mod)

typeDoc :: String -> TypeDecl -> Doc
typeDoc mod (Type name _ params cs)
  = def (keyword "data" <+> qname mod name) 
        (map tvarDoc params) 
        (consDeclsDoc mod cs)

typeDoc mod (TypeSyn name _ params syn)
  = def (keyword "type" <+> qname mod name) 
        (map varDoc params)
        (operator equals <+> typeExprDoc mod False syn)

varDoc :: Int -> Doc
varDoc = text . ('x':) . show

tvarDoc :: Int -> Doc
tvarDoc i
  | i>25      = text ("x" ++ show i)
  | otherwise = text [chr (97+i)]

consDeclsDoc :: String -> [ConsDecl] -> Doc
consDeclsDoc mod
  = fillEncloseSep (operator equals<>space) empty (bar<>space)
  . map ((<>space) . consDeclDoc mod)
  --= fillMulti (equals<+>) (bar<+>) . map (consDeclDoc mod)

consDeclDoc :: String -> ConsDecl -> Doc
consDeclDoc mod (Cons name _ _ args)
  = app (qname mod name) (map (typeExprDoc mod True) args)

typeExprDoc :: String -> Bool -> TypeExpr -> Doc
typeExprDoc _ _ (TVar n) = tvarDoc n

typeExprDoc mod br (TCons name args)
  | null args = qname mod name
  | name == (prelude,"[]") = brackets (typeExprDoc mod False (head args))
  | isTupleName name = tupled (map (typeExprDoc mod False) args)
  | otherwise 
    = par br $ app (qname mod name) (map (typeExprDoc mod True) args)

typeExprDoc mod br typ@(FuncType _ _)
  = par br $ fillEncloseSep empty empty (space<>arrow<>space)
              --fillMulti (empty<>) (arrow<+>)
     (map (typeExprDoc mod True) (argTypes typ) ++ 
      [typeExprDoc mod False (resultType typ)])

par br = if br then parens else id

funcsDoc :: Precs -> String -> [FuncDecl] -> Doc
funcsDoc pr mod funcs = vcat (punctuate line (map (funcDoc pr mod) funcs))

funcDoc :: Precs -> String -> FuncDecl -> Doc
funcDoc pr mod (Func name _ _ typ rule)
  = funcTypeDeclDoc mod name typ <$>
    ruleDoc pr mod name rule

funcTypeDeclDoc :: String -> QName -> TypeExpr -> Doc
funcTypeDeclDoc mod name typ
  | typ /= TVar (-42)
  = def (qname mod name) [] (funcTypeDoc mod (argTypes typ) (resultType typ))
  | otherwise = empty

funcTypeDoc :: String -> [TypeExpr] -> TypeExpr -> Doc
funcTypeDoc mod args res
  = fillEncloseSep (dcolon<>space) empty (arrow<>space)
  --= fillMulti (dcolon<+>) (arrow<+>)
     (map ((<>space) . typeExprDoc mod True) (args++[res]))

ruleDoc :: Precs -> String -> QName -> Rule -> Doc
ruleDoc pr mod name (Rule args body)
  = def (qname mod name) 
        (map varDoc args) 
        (equals <+> align (expDoc pr 0 mod False body))

ruleDoc _ mod name (External _)
  = qname mod name <+> keyword "external" -- <+> string decl

expDoc,expDoc2 :: Precs -> Int -> String -> Bool -> Expr -> Doc
expDoc pr _ mod br exp =
  maybe (maybe (expDoc2 pr 0 mod br exp)
          (\l -> list (map (expDoc pr 0 mod False) l))
          (toList exp))
    (\s -> if null s then consname "[]" else literal (dquotes (text s)))
      (toString exp)

expDoc2 _ _ _ _ (Var n) = varDoc n
expDoc2 _ _ _ _ (Lit l) = litDoc l

expDoc2 pr p mod br (Comb ct name args)
  | ct == FuncCall && name == (prelude,"apply")
    = par br $ app (expDoc pr 0 mod True (args!!0)) 
                           [expDoc pr 0 mod True (args!!1)]
  | ct == ConsCall && isTupleName name
    = tupled (map (expDoc pr 0 mod False) args)
  | isInfixName name && length args == 2
    = align $ precFillEncloseSep pOp p lbr rbr empty
               [expDoc pr pOp mod True (args!!0) 
               ,space <> operator (text (snd name)) <> space
               ,expDoc pr pOp mod True (args!!1)]
--     = par br $ app (expDoc mod True (args!!0))
--                    [text (snd name), expDoc mod True (args!!1)]
  | otherwise
    = par (not (null args) && br) $
       app (qname mod name) (map (expDoc pr 0 mod True) args)
 where
  (lbr,rbr) = if br then (lparen,rparen) else (empty,empty)
  pOp = case lookup name pr of
             Just pr -> pr
             Nothing -> 0 

expDoc2 pr _ mod br (Let bs e)
  = par br $ hang 1 $ 
     keyword keylet <+> letBindsDoc pr mod bs <$> 
     keyword "in" <+> expDoc pr 0 mod False e

  where keylet = case bs of 
         [(v,_)] -> if v >= 2000 then "let!" else "let"
         _ -> "let"

expDoc2 pr _ mod br (Free vs e)
--   | null vs = marked (expDoc mod br e)
--   | any (<0) vs = expDoc mod br e
--   | otherwise
    = par br $ hang 1 $ 
       keyword "let" <+> align (fillSep (punctuate comma (map varDoc vs))) <+>
       keyword "free" <$> keyword "in" <+> expDoc pr 0 mod False e

expDoc2 pr _ mod br (FlatCurry.Or e1 e2)
  = expDoc pr 0 mod br (Comb FuncCall (prelude,"?") [e1,e2])

expDoc2 pr _ mod br (Case ct e bs)
  = par br $ hang 1 $ 
     caseTypeDoc ct <> keyword "case" <+> align (expDoc pr 0 mod False e) 
       <+> keyword "of" <$> layout (map (branchDoc pr mod) bs)

branchDoc :: Precs -> String -> BranchExpr -> Doc
branchDoc pr mod (Branch pat e)
  = def (patternDoc mod pat <+> arrow) [] (align (expDoc pr 0 mod False e))

caseTypeDoc :: CaseType -> Doc
caseTypeDoc Rigid = empty
caseTypeDoc Flex  = empty -- literal $ text "{-f-}"

patternDoc :: String -> Pattern -> Doc
patternDoc mod (Pattern name args)
  | null args = qname mod name
  | isTupleName name = tupled (map varDoc args)
  | isInfixName name && length args == 2
    = varDoc (args!!0) <> operator (text (snd name)) <> varDoc (args!!1)
  {-| name == (prelude,":")
    = varDoc (args!!0) <> operator colon <> varDoc (args!!1)-}
  | otherwise = qname mod name <+> hsep (map varDoc args)

patternDoc _ (LPattern l) = litDoc l

letBindsDoc :: Precs -> String -> [(Int,Expr)] -> Doc
letBindsDoc pr mod = layout . map (letBindDoc pr mod)

letBindDoc :: Precs -> String -> (Int,Expr) -> Doc
letBindDoc pr mod (n,e) = 
  varDoc n <+> operator equals <+> expDoc pr 0 mod False e

litDoc :: Literal -> Doc
litDoc (Intc n) = literal (int n)
litDoc (Floatc x) = literal (float x)
litDoc (Charc c) = literal (squotes (text (quoteChar c)))

quoteChar c = maybe [c] id (lookup c specialChars)

-- more?
specialChars = [('\\',"\\\\"),('\n',"\\n"),('\r',"\\r"),('\t',"\\t")]

toString :: Expr -> Maybe String
toString exp
  = case exp of
      Comb ConsCall ("Prelude","[]") [] -> Just ""
      Comb ConsCall ("Prelude",":") [Lit (Charc c),cs] ->
        toString cs >>- Just . (quoteChar c++)
      _ -> Nothing

toList :: Expr -> Maybe [Expr]
toList exp
  = case exp of
      Comb ConsCall ("Prelude","[]") [] -> Just []
      Comb ConsCall ("Prelude",":") [x,xs] -> toList xs >>- Just . (x:)
      _ -> Nothing

-- introduces over-applications on purpose!
elimApp :: Expr -> Expr
elimApp = updCombs elim
 where
  elim ct name args
    | ct == FuncCall && name == (prelude,"apply") && isComb (head args) &&
      combName (head args) /= (prelude,"apply")
      = extend (head args) (args!!1)
    | otherwise = Comb ct name args
  extend (Comb ct name args) arg = Comb ct name (args++[arg])


-- testHtml name
--   = readFlatCurry name >>=
--     writeFile (name++".html") . showHtmlPage . HtmlPage name [] . (:[]) . 
--     styledHtml . showStyledProg

-- Apply x y = Comb FuncCall ("Prelude","apply") [x,y]

-- test e = pretty 78 $ expDoc "" False $ e

