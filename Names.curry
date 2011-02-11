-----------------------------------------------------------------------
--- Operations to change names of the original program into names
--- used in the target program
-----------------------------------------------------------------------
module Names where

import Char (isAlphaNum)
import List (intersperse)
import Maybe (fromJust, isJust)

import AbstractHaskell

--- Rename qualified type constructor.
renType (mn,fn) = (mkModName mn, mkTypeName fn)

-- Rename qualified data constructor.
renCons (mn,fn) = (mkModName mn, mkConName fn)

-- Rename qualified defined function.
renFunc (mn,fn) = (mkModName mn, mkFunName fn)

-- Name of the module providing external function definitions.
externalModule :: String
externalModule = "External"

-- rename defined functions
mkFunName :: String -> String
mkFunName = replaceNonIdChars "" "op_" 
-- TODO should we provide a prefix for functions as well? Otherwise there may
-- be nameclashes if one implements the infix operation ($$) and the function 
-- "op_dollar_dollar".


-- Generating Names for introduced constructors
mkChoiceName    (q, n) = (q, "Choice" +|+ n)
mkFailName  (q, n) = (q, "Fail" +|+ n)
mkGuardName (q, n) = (q, "Guard" +|+ n)
s +|+ t = s ++ "_" ++ t

-- rename data constructors
mkConName :: String -> String
mkConName n
  | n == "[]"     = "OP_Nil"
  | n == ":"      = "OP_Cons"
  | n == "()"     = "OP_Unit"
  | head n == '(' = "OP_Tuple" ++ show (length n - 1)
  | otherwise     = replaceNonIdChars "C_" "OP_" n

-- unrename data constructors (must be improved)
umkConName :: String -> String
umkConName n
  | n == "OP_Nil"    = "[]"
  | n == "OP_Cons"   = ":"
  | n == "OP_Unit"   = "()"
  | take 2 n == "C_" = drop 2 n
  | otherwise        = n

renameModule :: String -> String
renameModule = ("Curry_" ++)

renameQName :: QName -> QName
renameQName (q, n) = (renameModule q, genRename n)

genRename :: String -> String
genRename n
  | n == "[]"     = "OP_List"
  | n == "()"     = "OP_Unit"
  | head n == '(' = "OP_Tuple" ++ show (length n - 1)
  | otherwise     = replaceNonIdChars "c_" "op_" n

-- rename type constructors
mkTypeName :: String -> String
mkTypeName n
  | n == "[]"     = "OP_List"
  | n == "()"     = "OP_Unit"
  | head n == '(' = "OP_Tuple" ++ show (length n - 1)
  | otherwise     = replaceNonIdChars "C_" "OP_" n

detPrefix :: Bool -> String
detPrefix b = if b then "d_" else "c_"

-- rename modules
mkModName :: String -> String
mkModName = ("Curry_" ++)

renameFile :: String -> String
renameFile = renameModule -- until hierarchical module names are supported

mkExtModName :: String -> String
mkExtModName = ("External" ++)

compareNameForType :: String -> String
compareNameForType = (++ "_compare")

mkExtFunName :: String -> String
mkExtFunName = ("external_" ++) . mkFunName

isInfixName :: String -> Bool
isInfixName = all (`elem` "?!#$%^&*+=-<>.:/\\|")

showOpChar :: Char -> String
showOpChar c = case c of
  '_' -> "_" --"underscore" TODO: Can this lead to a name clash?
  '~' -> "tilde"
  '!' -> "bang"
  '@' -> "at"
  '#' -> "hash"
  '$' -> "dollar"
  '%' -> "percent"
  '^' -> "caret"
  '&' -> "ampersand"
  '*' -> "star"
  '+' -> "plus"
  '-' -> "minus"
  '=' -> "eq"
  '<' -> "lt"
  '>' -> "gt"
  '?' -> "qmark"
  '.' -> "dot"
  '/' -> "slash"
  '|' -> "bar"
  '\\' ->"backslash"
  ':' -> "colon"
  '(' -> "oparen"
  ')' -> "cparen"
  '[' -> "obracket"
  ']' -> "cbracket"
  ',' -> "comma"
  '\'' -> "tick"
  _   -> error ("unexpected symbol: " ++ show c)

-- | replaces characters that are not valid haskell identifiers,
-- | if there were no characters replaced, the first prefix,
-- | otherwise the snd prefix ist prepended
replaceNonIdChars :: String -> String -> String -> String
replaceNonIdChars pfxNonOp pfxOp str =
   case strings of
     []  -> error "replaceNonIdChars: empty identifier"
     [s] -> if isAlphaNum (head str)
               then pfxNonOp ++ s
               else pfxOp    ++ s
     _   -> pfxOp ++ concat (intersperse "_" strings)
 where strings = separateAndReplace isAlphaNumOrUS showOpChar str
       isAlphaNumOrUS c = isAlphaNum c || c=='_'


separateAndReplace :: (a -> Bool) -> (a -> [a]) -> [a] -> [[a]]
separateAndReplace pred f list =
  case rest of
    [] -> case sep of
           [] -> []
           _  -> [sep]
    (x:xs) -> case sep of
               [] -> f x : separateAndReplace pred f xs
               _  -> sep : f x : separateAndReplace pred f xs
 where (sep,rest) = break  (not . pred) list


