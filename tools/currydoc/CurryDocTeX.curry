----------------------------------------------------------------------
--- Functions to generate documentation in TeX format.
---
--- @author Michael Hanus
----------------------------------------------------------------------

module CurryDocTeX where

import CurryDocParams
import CurryDocRead
import FlatCurry
import HTML
import HtmlParser
import List
import Pandoc(markdown2latex)

--------------------------------------------------------------------------
-- Generates the documentation of a module in HTML format where the comments
-- are already analyzed.
generateTexDocs :: DocParams -> AnaInfo -> String -> String
                -> [(SourceLine,String)] -> IO ([String],String)
generateTexDocs docparams anainfo progname modcmts progcmts = do
  let fcyname = flatCurryFileName progname
  putStrLn $ "Reading FlatCurry program \""++fcyname++"\"..."
  (Prog _ imports types functions _) <- readFlatCurryFile fcyname
  let textypes = concatMap (genTexType progcmts) types
      texfuncs = concatMap (genTexFunc progcmts anainfo) functions
      modcmt   = fst (splitComment modcmts)
  modcmttex <- if withMarkdown docparams
               then markdown2latex modcmt
               else return (htmlString2Tex modcmt)
  return $
    (imports,
     "\\currymodule{"++getLastName progname++"}\n" ++
     modcmttex ++ "\n" ++
     (if null textypes then ""
      else "\\currytypesstart\n" ++ textypes ++ "\\currytypesstop\n") ++
     (if null texfuncs then ""
      else "\\curryfuncstart\n" ++ texfuncs ++ "\\curryfuncstop\n")
    )

-- translate a string possibly containing HTML tags into a corresponding
-- LaTeX string:
htmlString2Tex :: String -> String
htmlString2Tex s = showLatexExps (parseHtmlString s)

-- generate short HTML documentation for a function if it is exported:
genTexFunc progcmts _ (Func (_,fname) _ fvis ftype _) =
  if fvis==Public
  then "\\curryfunction{" ++ string2tex fname ++ "}{" ++
       "\\curryfuncsig{" ++ string2tex (showId fname) ++ "}{" ++
         showTexType False ftype ++ "}}{" ++
         htmlString2Tex
               (fst (splitComment (getFuncComment fname progcmts))) ++ "}\n"
  else ""

--- generate TeX documentation for a datatype if it is exported:
genTexType progcmts (Type (_,tcons) tvis tvars constrs) =
  if tvis==Public
  then
   let (datacmt,conscmts) = splitComment (getDataComment tcons progcmts)
    in "\\currydata{" ++ tcons ++ "}{" ++
       htmlString2Tex datacmt ++ "}{" ++
       concatMap (genHtmlCons (getCommentType "cons" conscmts)) constrs
       ++ "}\n"
  else ""
 where
  genHtmlCons conscmts (Cons (_,cname) _ cvis argtypes) =
    if cvis==Public
    then "\\currycons{" ++ cname ++ "}{" ++
         concatMap (\t->showTexType True t++" $\\to$ ") argtypes ++
                   tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++ "}{" ++
         (maybe ""
                (\ (call,cmt) -> "{\\tt " ++ call ++ "}" ++ htmlString2Tex cmt)
                (getConsComment conscmts cname))
         ++ "}\n"
    else ""

genTexType progcmts (TypeSyn (tcmod,tcons) tvis tvars texp) =
  if tvis==Public
  then let (typecmt,_) = splitComment (getDataComment tcons progcmts) in
       "\\currytype{" ++ tcons ++ "}{" ++
       (if tcons=="String" && tcmod=="Prelude"
        then "String = [Char]"
        else tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++ " = " ++
                      showTexType False texp ) ++ "}{" ++
       htmlString2Tex typecmt ++ "}\n"
  else ""

-- Pretty printer for types in Curry syntax as TeX string.
-- first argument is True iff brackets must be written around complex types
showTexType :: Bool -> TypeExpr -> String
showTexType _ (TVar i) = [chr (97+i)]
showTexType nested (FuncType t1 t2) =
   brackets nested
    (showTexType (isFunctionType t1) t1 ++ " $\\to$ " ++ showTexType False t2)
showTexType nested (TCons tc ts)
 | ts==[]  = snd tc
 | tc==("Prelude","[]") && (head ts == TCons ("Prelude","Char") [])
   = "String"
 | tc==("Prelude","[]")
   = "[" ++ showTexType False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                     -- tuple type
   = "(" ++ concat (intersperse "," (map (showTexType False) ts)) ++ ")"
 | otherwise
   = brackets nested
      (snd tc ++ " " ++ concat (intersperse " " (map (showTexType True) ts)))


-- convert string into TeX:
string2tex :: String -> String
string2tex = concatMap char2tex
 where
  char2tex c | c==chr 228  = "\\\"a"
             | c==chr 246  = "\\\"o"
             | c==chr 252  = "\\\"u"
             | c==chr 196  = "\\\"A"
             | c==chr 214  = "\\\"O"
             | c==chr 220  = "\\\"U"
             | c==chr 223  = "\\ss{}"
             | c=='~'      = "{\\char126}"
             | c=='\\'     = "{\\char92}"
             | c=='<'      = "{$<$}"
             | c=='>'      = "{$>$}"
             | c=='_'      = "\\_"
             | c=='#'      = "\\#"
             | c=='$'      = "\\$"
             | c=='%'      = "\\%"
             | c=='{'      = "\\{"
             | c=='}'      = "\\}"
             | c=='&'      = "\\&"
             | otherwise   = [c]


